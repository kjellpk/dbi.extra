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
#' @rdname dbListForeignKeys
#'
#' @export
setGeneric(name = "dbListForeignKeys",
           valueClass = "data.frame",
           def = function(conn, id, ...) standardGeneric("dbListForeignKeys"))

#' @importFrom methods setGeneric .valueClassTest



dbListForeignKeys_default <- function(conn, id, ...) {
  data.frame(id = I(list()), primary = I(list()), foreign = I(list()))
}



#' @importFrom methods setMethod
#' @importClassesFrom DBI DBIConnection
#' @rdname dbListForeignKeys
#' @aliases dbListForeignKeys,DBIConnection,Id-method
#' @export
setMethod(f = "dbListForeignKeys",
          signature = c(conn = "DBIConnection", id = "Id"),
          definition = dbListForeignKeys_default)



dbListForeignKeys_SQLiteConnection <- function(conn, id, ...) {
  check_id(id)

  query <- "SELECT `table`, `to` AS `primary`, `from` AS `foreign`
              FROM pragma_foreign_key_list(%s)
             ORDER BY `id`, `seq`;"
  #' @importFrom DBI dbQuoteLiteral
  query <- sprintf(query, dbQuoteLiteral(conn, id@name["table"]))

  #' @importFrom DBI dbGetQuery
  split_by_id(dbGetQuery(conn, query), "table")
}



#' @importFrom methods setMethod
#' @importClassesFrom RSQLite SQLiteConnection
#' @rdname dbListForeignKeys
#' @aliases dbListForeignKeys,SQLiteConnection,Id-method
#' @export
setMethod(f = "dbListForeignKeys",
          signature = c(conn = "SQLiteConnection", id = "Id"),
          definition = dbListForeignKeys_SQLiteConnection)



dbListForeignKeys_Microsoft_SQL_Server <- function(conn, id, ...) {
  check_id(id)
  components <- id@name

  xref <- c(catalog = "@fktable_qualifier",
            schema = "@fktable_owner",
            table = "@fktable_name")

  names(components) <- xref[names(components)]

  #' @importFrom DBI dbQuoteString
  statement <- paste(names(components), dbQuoteString(conn, components), sep = " = ")
  statement <- paste("EXEC sp_fkeys", paste(statement, collapse = ", "))

  #' @importFrom DBI dbGetQuery
  fk <- dbGetQuery(conn, statement)
  fk <- fk[, c("PKTABLE_QUALIFIER", "PKTABLE_OWNER", "PKTABLE_NAME", "PKCOLUMN_NAME", "FKCOLUMN_NAME")]
  names(fk) <- c("catalog", "schema", "table", "primary", "foreign")

  split_by_id(fk, c("catalog", "schema", "table"))
}



#' @importFrom methods setMethod
#' @export
setMethod(f = "dbListForeignKeys",
          signature = c(conn = "Microsoft SQL Server", id = "Id"),
          definition = dbListForeignKeys_Microsoft_SQL_Server)
