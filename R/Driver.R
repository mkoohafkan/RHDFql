#' @include RHDFql.R
NULL

#' DBI methods
#'
#' Implementations of pure virtual functions defined in the `DBI` package.
#' @name DBI
NULL

#' HDFql driver
#'
#' TBD.
#'
#' @export
#' @import methods DBI
#' @examples
#' \dontrun{
#' #' library(DBI)
#' RHDFql::HDFql()
#' }
HDFql <- function(libpath) {
	if (hql_is_loaded() & !missing(libpath)) {
		if (hql.paths$install != libpath) {
			stop("Cannot connect to multiple versions ",
				"of HDFql. Currently connected to ",
				shQuoute(hql.paths$install))
		}
	} else {
		hql_load(libpath)
	}
  new("HDFqlDriver")
}

#' @rdname DBI
#' @export
setClass("HDFqlDriver", contains = "DBIDriver")

#' @rdname DBI
#' @inheritParams methods::show
#' @export
setMethod(
  "show", "HDFqlDriver",
  function(object) {
		cat("<HDFqlDriver>\n")
		cat("  libpath:", hql.paths$install, "\n")
		cat("  status: ", if (hql_is_loaded()) "loaded"
			else "not loaded", "\n")
		cat("  version: ", if (hql_is_loaded()) hql$wrapper$HDFQL_VERSION
				else "", "\n")

	}
)

#' @rdname DBI
#' @inheritParams DBI::dbConnect
#' @export
setMethod("dbConnect", "HDFqlDriver",
	function(drv, db, ..., bigint = c("integer64", "integer", "numeric", "character")) {
		bigint = match.arg(bigint)
		new("HDFqlConnection",
			db = db,
			bigint = bigint
		)
	}
)


#' @rdname DBI
#' @export
setMethod("dbUnloadDriver", "HDFqlDriver", function(drv, ...) {
	hql_unload()
})

#' @rdname DBI
#' @inheritParams DBI::dbDataType
#' @export
setMethod(
  "dbDataType", "HDFqlDriver",
  function(dbObj, obj, ...) {
    # Optional: Can remove this if all data types conform to SQL-92
    tryCatch(
      getMethod("dbDataType", "DBIObject", asNamespace("DBI"))(dbObj, obj, ...),
      error = function(e) testthat::skip("Not yet implemented: dbDataType(Driver)"))
	}
)

#' @rdname DBI
#' @inheritParams DBI::dbDataType
#' @export
setMethod(
  "dbDataType", c("HDFqlDriver", "list"),
  function(dbObj, obj, ...) {
    # rstats-db/DBI#70
    testthat::skip("Not yet implemented: dbDataType(Driver, list)")
	}
)

#' @rdname DBI
#' @inheritParams DBI::dbIsValid
#' @export
setMethod(
  "dbIsValid", "HDFqlDriver",
  function(dbObj, ...) {
		if (hql$wrapper$hdfql_execute(paste("USE FILE", dbObj)) > 0)
			FALSE
		else
			TRUE
	}
)

#' @rdname DBI
#' @inheritParams DBI::dbGetInfo
#' @export
setMethod(
  "dbGetInfo", "HDFqlDriver",
  function(dbObj, ...) {
    testthat::skip("Not yet implemented: dbGetInfo(Driver)")
	}
)
