#' @rdname DBI
#' @inheritParams DBI::dbWriteTable
#' @param overwrite Allow overwriting the destination table. Cannot be
#'   `TRUE` if `append` is also `TRUE`.
#' @param append Allow appending to the destination table. Cannot be
#'   `TRUE` if `overwrite` is also `TRUE`.
#' @export
setMethod(
	"dbWriteTable", c("HDFqlConnection", "character", "data.frame"),
	function(conn, name, value, overwrite = FALSE, append = FALSE, ...) {
		testthat::skip("Not yet implemented: dbWriteTable(Connection, character, data.frame)")
	})

#' @rdname DBI
#' @inheritParams DBI::dbReadTable
#' @export
setMethod(
	"dbReadTable", c("HDFqlConnection", "character"),
	function(conn, name, ...) {
		testthat::skip("Not yet implemented: dbReadTable(Connection, character)")
	})

#' @rdname DBI
#' @inheritParams DBI::dbListTables
#' @export
setMethod(
	"dbListTables", "HDFqlConnection",
	function(conn, ...) {
		testthat::skip("Not yet implemented: dbListTables(Connection)")
	}
)

#' @rdname DBI
#' @inheritParams DBI::dbExistsTable
#' @export
setMethod(
	"dbExistsTable", c("HDFqlConnection", "character"),
	function(conn, name, ...) {
		testthat::skip("Not yet implemented: dbExistsTable(Connection)")
	}
)

#' @rdname DBI
#' @inheritParams DBI::dbListFields
#' @export
setMethod(
	"dbListFields", c("HDFqlConnection", "character"),
	function(conn, name, ...) {
		testthat::skip("Not yet implemented: dbListFields(Connection, character)")
	}
)

#' @rdname DBI
#' @inheritParams DBI::dbRemoveTable
#' @export
setMethod(
	"dbRemoveTable", c("HDFqlConnection", "character"),
	function(conn, name, ...) {
		testthat::skip("Not yet implemented: dbRemoveTable(Connection, character)")
	}
)

#' @rdname DBI
#' @inheritParams DBI::dbGetInfo
#' @export
setMethod(
	"dbGetInfo", "HDFqlConnection",
	function(dbObj, ...) {
		testthat::skip("Not yet implemented: dbGetInfo(Connection)")
	}
)

#' @rdname DBI
#' @inheritParams DBI::dbBegin
#' @export
setMethod(
	"dbBegin", "HDFqlConnection",
	function(conn, ...) {
		stop("Transactions are not supported by HDFql.")
	}
)

#' @rdname DBI
#' @inheritParams DBI::dbCommit
#' @export
setMethod(
	"dbCommit", "HDFqlConnection",
	function(conn, ...) {
		stop("Transactions are not supported by HDFql.")
	}
)

#' @rdname DBI
#' @inheritParams DBI::dbRollback
#' @export
setMethod(
	"dbRollback", "HDFqlConnection",
	function(conn, ...) {
		stop("Transactions are not supported by HDFql.")
	}
)
