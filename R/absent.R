#' @importFrom DBI dbListFields
#' @importFrom methods setMethod

#' @export
setMethod("dbListFields", signature("Microsoft SQL Server", "Id"),
           function(conn, name, ...) {
             #' @importFrom odbc odbcConnectionColumns
             odbcConnectionColumns(conn, name, ...)$name
           }
)
