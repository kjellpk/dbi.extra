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

  query <- paste("SELECT name FROM pragma_table_info(%s)",
                 "WHERE pk > 0 ORDER BY pk;")

  #' @importFrom DBI dbQuoteLiteral
  query <- sprintf(query, dbQuoteLiteral(conn, id@name[["table"]]))

  #' @importFrom DBI dbGetQuery
  dbGetQuery(conn, query)$name
}



#' @export
"dbListPrimaryKeys.Microsoft SQL Server" <- function(conn, id, ...) {
  check_id(id)
  components <- id@name

  xref <- c(catalog = "@table_qualifier",
            schema = "@table_owner",
            table = "@table_name")
  
  names(components) <- xref[names(components)]

  #' @importFrom DBI dbQuoteString
  statement <- paste(names(components), dbQuoteString(conn, components), sep = " = ")
  statement <- paste("EXEC sp_pkeys", paste(statement, collapse = ", "))

  #' @importFrom DBI dbGetQuery
  dbGetQuery(conn, statement)$COLUMN_NAME
}
