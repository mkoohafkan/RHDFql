#' @include Driver.R
NULL

#' @rdname DBI
#' @export
setClass(
  "HDFqlConnection",
  contains = "DBIConnection",
	slots = list(
		db = "character",
		bigint = "character"
  )
)

#' @rdname DBI
#' @inheritParams methods::show
#' @export
setMethod(
  "show", "HDFqlConnection",
  function(object) {
		cat("<HDFqlConnection>\n")
		cat("  db: ", object@db, "\n")
		cat("  bigint: ", object@bigint, "\n")
  })

#' @rdname DBI
#' @inheritParams DBI::dbIsValid
#' @export
setMethod(
  "dbIsValid", "HDFqlConnection",
	function(dbObj, ...) {
		if (hql$wrapper$hdfql_execute(paste("USE FILE", shQuote(dbObj@db))) < 0)
			FALSE
		else
			TRUE
  })

#' @rdname DBI
#' @inheritParams DBI::dbDisconnect
#' @export
setMethod(
  "dbDisconnect", "HDFqlConnection",
  function(conn, ...) {
    if (!dbIsValid(conn)) {
      warning("Connection already closed.", call. = FALSE)
		} else {
			if (hql$wrapper$hdfql_execute(paste("CLOSE FILE", shQuote(dbObj@db))) < 0) {
				stop("Could not close file ", shQuote(dbObj@db), call. = FALSE)
			}
			else
				TRUE
		}
  })

#' @rdname DBI
#' @inheritParams DBI::dbSendQuery
#' @export
setMethod(
  "dbSendQuery", c("HDFqlConnection", "character"),
  function(conn, statement, ...) {
    HDFqlResult(connection = conn, statement = statement)
  })

#' @rdname DBI
#' @inheritParams DBI::dbSendStatement
#' @export
setMethod(
  "dbSendStatement", c("HDFqlConnection", "character"),
	function(conn, statement, ...) {
    HDFqlResult(connection = conn, statement = statement)
  })

#' @rdname DBI
#' @inheritParams DBI::dbDataType
#' @export
setMethod(
  "dbDataType", "HDFqlConnection",
  function(dbObj, obj, ...) {
    tryCatch(
      getMethod("dbDataType", "DBIObject", asNamespace("DBI"))(dbObj, obj, ...),
      error = function(e) testthat::skip("Not yet implemented: dbDataType(Connection)"))
  })

#' @rdname DBI
#' @inheritParams DBI::dbQuoteString
#' @export
setMethod(
  "dbQuoteString", c("HDFqlConnection", "character"),
  function(conn, x, ...) {
    # Optional
    getMethod("dbQuoteString", c("DBIConnection", "character"), asNamespace("DBI"))(conn, x, ...)
  })

#' @rdname DBI
#' @inheritParams DBI::dbQuoteIdentifier
#' @export
setMethod(
  "dbQuoteIdentifier", c("HDFqlConnection", "character"),
  function(conn, x, ...) {
    # Optional
    getMethod("dbQuoteIdentifier", c("DBIConnection", "character"), asNamespace("DBI"))(conn, x, ...)
  })

