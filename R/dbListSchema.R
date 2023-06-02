#' List Database Schema
#'
#' @description List a database schema by table including column names.
#'
#' @param conn a \code{DBIConnection}.
#'
#' @param prefix an \code{\link[DBI]{Id}} specifying fully qualified path
#'               in the database's namespace, or NULL.
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

  schema
}



#' @export
"dbListSchema.Microsoft SQL Server" <- function(conn, prefix = NULL, ...) {
  check_id(prefix)

  if (!is.null(prefix)) {
    components <- prefix@name
  } else {
    components <- character()
  }

  if (is.na(catalog <- components["catalog"])) {
    catalog <- dbGetInfo(conn)$dbname
  }

  q1 <- "SELECT \"TABLE_CATALOG\", \"TABLE_SCHEMA\", \"TABLE_NAME\", \"COLUMN_NAME\" AS \"column_names\"
           FROM %s.\"INFORMATION_SCHEMA\".\"COLUMNS\""

  if (!is.na(schema <- components["schema"])) {
    q1 <- paste0(q1, "\nWHERE \"TABLE_SCHEMA\" = '", schema, "'")
  }
  
  q1 <- paste0(q1, "\nORDER BY \"TABLE_CATALOG\", \"TABLE_SCHEMA\", \"TABLE_NAME\", \"ORDINAL_POSITION\"")
  q1 <- sprintf(q1, dbQuoteIdentifier(conn, catalog))

  split_by_id(dbGetQuery(conn, q1), c("TABLE_CATALOG", "TABLE_SCHEMA", "TABLE_NAME"))
}



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
