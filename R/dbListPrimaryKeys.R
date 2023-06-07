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
#' @rdname dbListPrimaryKeys
#' @export
setGeneric(name = "dbListPrimaryKeys",
           valueClass = "character",
           def = function(conn, id, ...) standardGeneric("dbListPrimaryKeys"))

#' @importFrom methods setGeneric .valueClassTest



dbListPrimaryKeys_default <- function(conn, id, ...) {
  character(0)
}



#' @importFrom methods setMethod
#' @importClassesFrom DBI DBIConnection
#' @rdname dbListPrimaryKeys
#' @aliases dbListPrimaryKeys,DBIConnection,Id-method
#' @export
setMethod(f = "dbListPrimaryKeys",
          signature = c(conn = "DBIConnection", id = "Id"),
          definition = dbListPrimaryKeys_default)



dbListPrimaryKeys_SQLiteConnection <- function(conn, id, ...) {
  check_id(id)


  query <- paste("SELECT name FROM pragma_table_info(%s)",
                 "WHERE pk > 0 ORDER BY pk;")

  #' @importFrom DBI dbQuoteLiteral
  query <- sprintf(query, dbQuoteLiteral(conn, id@name[["table"]]))

  #' @importFrom DBI dbGetQuery
  dbGetQuery(conn, query)$name
}



#' @importFrom methods setMethod
#' @rdname dbListPrimaryKeys
#' @aliases dbListPrimaryKeys,SQLiteConnection,Id-method
#' @export
setMethod(f = "dbListPrimaryKeys",
          signature = c(conn = "SQLiteConnection", id = "Id"),
          definition = dbListPrimaryKeys_SQLiteConnection)



dbListPrimaryKeys_Microsoft_SQL_Server <- function(conn, id, ...) {
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



#' @importFrom methods setMethod
#' @export
setMethod(f = "dbListPrimaryKeys",
          signature = c(conn = "Microsoft SQL Server", id = "Id"),
          definition = dbListPrimaryKeys_Microsoft_SQL_Server)
