#' List Foreign Keys
#'
#' @description List the foreign keys of a database table.
#'
#' @param conn a \code{DBIConnection}.
#'
#' @param id an \code{\link[DBI]{Id}} specifying a database table.
#'
#' @param \dots additional arguements.
#'
#' @section Value: a compact representation of the foreign keys.
#'
#' @export
dbListForeignKeys <- function(conn, id, ...) {
  UseMethod("dbListForeignKeys")
}


#' @export
dbListForeignKeys.default <- function(conn, id, ...) {
  warning("no method to discover foreign keys for ", class(conn)[1],
          " connections")

  NULL
}


#' @export
dbListForeignKeys.SQLiteConnection <- function(conn, id, ...) {
  check_id(id)

  query <- "SELECT `table`, `from`, `to`
              FROM pragma_foreign_key_list(%s)
             ORDER BY `id`, `seq`;"
  #' @importFrom DBI dbQuoteLiteral
  query <- sprintf(query, dbQuoteLiteral(conn, id@name["table"]))

  #' @importFrom DBI dbGetQuery
  split_by_id(dbGetQuery(conn, query), "table")
}
