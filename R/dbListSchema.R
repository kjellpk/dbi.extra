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
#' @rdname dbListSchema
#'
#' @export
setGeneric(name = "dbListSchema",
           def = function(conn, prefix = NULL, ...) {
                   standardGeneric("dbListSchema")
                 },
           valueClass = "data.frame",
           signature = "conn")

#' @importFrom methods setGeneric .valueClassTest



dbListSchema_default <- function(conn, prefix = NULL, ...) {
  check_id(prefix)

  #' @importFrom DBI dbListObjects
  schema <- dbListObjects(conn, prefix = prefix, ...)
  schema <- schema[!schema$is_prefix, "table", drop = FALSE]
  names(schema) <- "id"

  #' @importFrom DBI dbListFields
  fields <- lapply(schema$id, function(u, v) dbListFields(v, u), v = conn)
  
  cbind(schema, column_names = I(fields))
}



#' @importFrom methods setMethod
#' @importClassesFrom DBI DBIConnection
#' @rdname dbListSchema
#' @aliases dbListSchema,DBIConnection,Id-method
#' @export
setMethod(f = "dbListSchema",
          signature = c(conn = "DBIConnection"),
          definition = dbListSchema_default)



dbListSchema_Microsoft_SQL_Server <- function(conn, prefix = NULL, ...) {
  check_id(prefix)

  if (!is.null(prefix)) {
    components <- prefix@name
  } else {
    components <- character()
  }

  if (is.na(catalog <- components["catalog"])) {
    #' @importFrom DBI dbGetInfo
    catalog <- dbGetInfo(conn)$dbname
  }

  q1 <- "SELECT \"TABLE_CATALOG\" AS \"catalog\",
                \"TABLE_SCHEMA\" AS \"schema\",
                \"TABLE_NAME\" AS \"table\",
                \"COLUMN_NAME\" AS \"column_names\"
           FROM %s.\"INFORMATION_SCHEMA\".\"COLUMNS\""

  if (!is.na(schema <- components["schema"])) {
    q1 <- paste0(q1, "\nWHERE \"TABLE_SCHEMA\" = '", schema, "'")
  }

  q1 <- paste0(q1, "\nORDER BY \"TABLE_CATALOG\", \"TABLE_SCHEMA\", \"TABLE_NAME\", \"ORDINAL_POSITION\"")
  #' @importFrom DBI dbQuoteIdentifier
  q1 <- sprintf(q1, dbQuoteIdentifier(conn, catalog))

  #' @importFrom DBI dbGetQuery
  split_by_id(dbGetQuery(conn, q1), c("catalog", "schema", "table"))
}


#' @importFrom methods setMethod
#' @export
setMethod(f = "dbListSchema",
          signature = c(conn = "Microsoft SQL Server"),
          definition = dbListSchema_Microsoft_SQL_Server)
