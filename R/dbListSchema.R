#' List Database Schema
#'
#' @description List a database schema by table including column names
#'              and primary keys.
#'
#' @param conn a \code{DBIConnection}.
#'
#' @param prefix an \code{\link[DBI]{Id}} specifying a database schema.
#'               See the output from \code{\link[DBI]{dbListObjects}} when
#'               \code{is_prefix} is \code{TRUE}.
#'
#' @param \dots additional arguements.
#'
#' @section Value: a compact representation of the database schema.
#'
#' @export
dbListSchema <- function(conn, prefix = NULL, ...) {
  UseMethod("dbListSchema")
}


#' @export
dbListSchema.default <- function(conn, prefix = NULL, ...) {
  check_id(prefix)

  #' @importFrom DBI dbListObjects
  schema <- dbListObjects(conn, prefix = prefix, ...)
  schema <- schema[!schema$is_prefix, "table", drop = FALSE]

  #' @importFrom DBI dbListFields
  fields <- lapply(schema$table, function(u, v) dbListFields(v, u), v = conn)
  schema <- cbind(schema, column_names = I(fields))

  pk <- lapply(schema$table, function(u, v) dbListPrimaryKeys(v, u), v = conn)
  schema <- cbind(schema, primary_keys = I(pk))

  schema
}


#' @export
dbListSchema.MariaDBConnection <- function(conn, prefix = NULL, ...) {
  check_id(prefix)

  cols <- c("TABLE_SCHEMA", "TABLE_NAME", "COLUMN_NAME", "COLUMN_KEY")
  o <- c("TABLE_SCHEMA", "TABLE_NAME", "ORDINAL_POSITION")
  #' @importFrom DBI Id
  from <- Id(schema = "information_schema", table = "COLUMNS")

  if (is.null(prefix)) {
    prefix <- "DATABASE()"
  } else {
    #' @importFrom DBI dbQuoteString
    prefix <- dbQuoteString(conn, prefix@name[["schema"]])
  }

  #' @importFrom DBI dbQuoteIdentifier
  query <- paste("SELECT",
                 paste(dbQuoteIdentifier(conn, cols), collapse = ", "),
                 "FROM",
                 dbQuoteIdentifier(conn, from),
                 "WHERE TABLE_SCHEMA =",
                 prefix,
                 "ORDER BY",
                 paste(dbQuoteIdentifier(conn, o), collapse = ", "))

  #' @importFrom DBI dbGetQuery
  schema <- dbGetQuery(conn, query)

  split_by_id(schema, idcols = c("TABLE_SCHEMA", "TABLE_NAME"))
}
