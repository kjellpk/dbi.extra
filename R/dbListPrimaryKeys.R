#' List Primary Keys
#'
#' @description List the primary keys of a database table.
#'
#' @param conn a \code{DBIConnection}.
#'
#' @param id an \code{\link[DBI]{Id}} specifying a database table.
#'
#' @param \dots additional arguements.
#'
#' @section Value: a character vector containing the names of the primary
#'                 key columns.
#'
#' @export
dbListPrimaryKeys <- function(conn, id, ...) {
  UseMethod("dbListPrimaryKeys")
}


#' @export
dbListPrimaryKeys.default <- function(conn, id, ...) {
  warning("no method to discover primary keys for ", class(conn)[1],
          " connections")

  NULL
}



#' @export
dbListPrimaryKeys.SQLiteConnection <- function(conn, id, ...) {
  check_id(id)

  #' @importFrom DBI dbQuoteLiteral
  query <- paste0("SELECT name FROM pragma_table_info(",
                  dbQuoteLiteral(conn, id@name["table"]),
                  ") WHERE pk > 0 ORDER BY pk;")

  #' @importFrom DBI dbGetQuery
  dbGetQuery(conn, query)$name
}
