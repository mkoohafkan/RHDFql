#' @include Connection.R
NULL

#' Execute HDFql Statement With Error
#' @keywords internal
execute_catch_error = function(statement) {
	if (hql$wrapper$hdfql_execute(statement) < 0) {
		stop(hql$wrapper$hdfql_error_get_message())
	}  
}

HDFqlResult <- function(connection, statement) {
	if (!dbIsValid(connection)) {
		stop("Could not connect to ", shQuote(connection@db))
	}
	execute_catch_error(paste("USE FILE", shQuote(connection@db)))
	# initialize cursor
	cursor = hql$wrapper$hdfql_cursor()
	new("HDFqlResult",
		connection = connection,
		statement = statement,
		# need to wrap cursor in list for type checking
		cursor = list(cursor),
		executed = FALSE
	)
}

#' @rdname DBI
#' @export
setClass(
  "HDFqlResult",
  contains = "DBIResult",
  slots = list(
    connection = "HDFqlConnection",
		statement = "character",
		# need to wrap cursor in list for type checking
		cursor = "list",
		executed = "logical"
  )
)

#' @rdname DBI
#' @inheritParams methods::show
#' @export
setMethod(
  "show", "HDFqlResult",
  function(object) {
		cat("<HDFqlResult>\n")
		cat("  db: ", object@connection@db, "\n")
		cat("  statement: ", object@statement, "\n")
	}
)

#' @rdname DBI
#' @inheritParams DBI::dbClearResult
#' @export
setMethod(
  "dbClearResult", "HDFqlResult",
  function(res, ...) {
		TRUE
	}
)

#' @rdname DBI
#' @inheritParams DBI::dbFetch
#' @export
setMethod(
  "dbFetch", "HDFqlResult",
	function(res, n = -1, ...) {
		# execute statement to populate cursor
		hql$wrapper$hdfql_cursor_use(res@cursor[[1]])
		execute_catch_error(res@statement)
    # get data specs
		dtype = dbColumnInfo(res)$type
		dcount = dbGetRowCount(res)
		# execute statement to populate cursor
		rtype = dtype_to_rtype(dtype)
		# get data
		if (grepl("SHOW", toupper(res@statement))) {
		  # get with cursor
			rObj = vector(rtype, dcount)
			for (i in seq_along(rObj)) {
				hql$wrapper$hdfql_cursor_next(res@cursor[[1]])
				rObj[i] = res@cursor[[1]]()
			}
		} else if (grepl("SELECT", toupper(res@statement))) {
		  # get with memory
			rObj = vector(rtype, dcount)
			if (hql$wrapper$hdfql_variable_transient_register(rObj) < 0L) {
				stop("Error registering variable for fetching data.")
			}
			newstatement = paste(res@statement, "INTO MEMORY",
			hql$wrapper$hdfql_variable_get_number(rObj))
			execute_catch_error(newstatement)
		} else {
		  rObj = TRUE
		}
		res@executed = TRUE
		data.frame(X1 = rObj, stringsAsFactors = FALSE)
	}
)

#' @rdname DBI
#' @inheritParams DBI::dbHasCompleted
#' @export
setMethod(
  "dbHasCompleted", "HDFqlResult",
  function(res, ...) {
    testthat::skip("Not yet implemented: dbHasCompleted(Result)")
	}
)

#' @rdname DBI
#' @inheritParams DBI::dbGetInfo
#' @export
setMethod(
  "dbGetInfo", "HDFqlResult",
  function(dbObj, ...) {
    # Optional
    getMethod("dbGetInfo", "DBIResult", asNamespace("DBI"))(dbObj, ...)
	}
)

#' @rdname DBI
#' @inheritParams DBI::dbIsValid
#' @export
setMethod(
  "dbIsValid", "HDFqlResult",
  function(dbObj, ...) {
    testthat::skip("Not yet implemented: dbIsValid(Result)")
	}
)

#' @rdname DBI
#' @inheritParams DBI::dbGetStatement
#' @export
setMethod(
  "dbGetStatement", "HDFqlResult",
  function(res, ...) {
    res@statement
	}
)

#' @rdname DBI
#' @inheritParams DBI::dbColumnInfo
#' @export
setMethod(
  "dbColumnInfo", "HDFqlResult",
	function(res, ...) {
	  #if(!dbIsValid(res))
		intcode = hql$wrapper$hdfql_cursor_get_data_type(res@cursor[[1]])
		if (intcode < 0) {
			stop(hql$wrapper$hdfql_error_get_message())
		}
		dtype = get_key(intcode, hql_data_types(), TRUE)
		data.frame(name = "X1", type = dtype,
		  stringsAsFactors = FALSE)
	}
)

#' @rdname DBI
#' @inheritParams DBI::dbGetRowCount
#' @export
setMethod(
  "dbGetRowCount", "HDFqlResult",
  function(res, ...) {
		hql$wrapper$hdfql_cursor_get_count(res@cursor[[1]])
	}
)

#' @rdname DBI
#' @inheritParams DBI::dbGetRowsAffected
#' @export
setMethod(
  "dbGetRowsAffected", "HDFqlResult",
  function(res, ...) {
    testthat::skip("Not yet implemented: dbGetRowsAffected(Result)")
	}
)

#' @rdname DBI
#' @inheritParams DBI::dbBind
#' @export
setMethod(
  "dbBind", "HDFqlResult",
  function(res, params, ...) {
    testthat::skip("Not yet implemented: dbBind(Result)")
	}
)
