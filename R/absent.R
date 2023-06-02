#' @importFrom DBI dbListFields
#' @importFrom methods setMethod
## @importClassesFrom odbc Microsoft SQL Server

#' @export
setMethod("dbListFields", signature("Microsoft SQL Server", "Id"),
           function (conn, name, ...) {
           #' @importFrom odbc odbcConnectionColumns
             odbcConnectionColumns(conn, name, ...)$name
           }
)
