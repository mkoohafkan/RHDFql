#' Get List Keys or Values
#'
#' Get the value of a list key, or the key of a list value.
#'
#' @param x A key or value.
#' @param l A named list.
#' @param invert If `TRUE`, return the key associated with 
#'  the given value.
#' @return A key or value.
#'
#' @importFrom utils stack
#' @keywords internal
get_key = function(x, l, invert = FALSE) {
	if (!invert) {
		if (x %in% names(l)) {
			l[[x]]
		} else {
			NULL
		}
	}
	else {
		d = stack(l)
		if (x %in% d$values) {
			as.character(d[d$values == x, "ind"])
		} else {
			NULL
		}
	}
}

#' Format Sequence For HDFql
#'
#' Format an integer sequence for selection with HDFql.
#'
#' @param s An integer sequence.
#' @return A vector of character representations of the integer sequence.
#'
#' @keywords internal
as_hdfql_sequence = function(s) {
	s = as.integer(s)
	m = c(0, diff(s))
	b = rle(m)
	b$values[b$lengths == 1L & b$values != 1] = 0
	l = cumsum(!inverse.rle(b))
	d = function(x) {
		paste0(formatC(range(x[, 1]), format = "d"),
			collapse = paste0(":", formatC(unique(x[-1, -1]), format = "d"), ":"))
	}
	f = c(by(cbind(s, m), l, d))
	sub("::.*", "", sub(":1:", ":", f))

}

#' HDF Data Type to R Type
#'
#' @param dtype The HDF data type.
#' @return The equivalent R class, or `NULL` if not found.
#'
#' @keywords internal
dtype_to_rtype = function(dtype) {
	rtype = get_key(dtype, hql_Rtypes(), FALSE)
	if (is.null(rtype) || length(rtype) == 0L) {
		stop("No corresponding R class for HDF data type ", dtype)
	}
	if (rtype == "integer64") {
		if (!requireNamespace("bit64")) {
			stop("Support for ", dtype, 'requires package "bit64"')
		}
	}
	rtype
}

#' R Type to HDF Data Type
#'
#' @param rtype The R class.
#' @return The equivalent HDF data type, or `NULL` if not found.
#'
#' @keywords internal
rtype_to_dtype = function(rtype, stop.on.error = TRUE) {
	if (rtype == "integer64") {
		if (!requireNamespace("bit64")) {
			stop("Support for ", rtype, 'requires package "bit64"')
		}
	}
	dtype = get_key(rtype, hql_Rtypes(), TRUE)
	# drop "var" types
	dtype = dtype[!grepl("VAR.+$", dtype)]
	# drop "tiny", small, and unsigned
	dtype = dtype[!grepl("TINY|SMALL|UNSIGNED", dtype)]
	# drop float
	dtype = dtype[!grepl("FLOAT", dtype)]
	if (is.null(dtype) || length(dtype) == 0L) {
		if (stop.on.error) {
			stop("No corresponding HDF data type for R class ", rtype)
		} else {
			dtype = NULL
		}
	}
	dtype
}

#' HDF Integer Output to Character
#'
#' Convert integer data from an HDF file to characters.
#'
#' @param x An integer array.
#' @param trim If `TRUE`, trim whitespace from the character data.
#' @return A character array.
#'
#' @keywords internal
int_to_char = function(x, trim = FALSE) {
	y = tryCatch(rawToChar(as.raw(x)),
		error = function(e) e)
	if ("error" %in% class(y)) {
		warning(y$message, call. = FALSE)
		# handle embedded nuls
		y = readBin(as.raw(x), "raw", length(x))
		y[y == as.raw(0)] = as.raw(0x20)
		y = rawToChar(y)
	}
	# remove whitespace
	if (trim) {
		trimws(y, "both")
	} else {
		y
	}
}
