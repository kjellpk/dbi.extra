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
  warning("no method to look up foreign keys for ", class(conn)[1],
          " connection")

  NULL
}



#' @export
dbListForeignKeys.SQLiteConnection <- function(conn, id, ...) {
  check_id(id)

  query <- "SELECT `table`, `to` AS `primary`, `from` AS `foreign`
              FROM pragma_foreign_key_list(%s)
             ORDER BY `id`, `seq`;"
  #' @importFrom DBI dbQuoteLiteral
  query <- sprintf(query, dbQuoteLiteral(conn, id@name["table"]))

  #' @importFrom DBI dbGetQuery
  split_by_id(dbGetQuery(conn, query), "table")
}



#' @export
"dbListForeignKeys.Microsoft SQL Server" <- function(conn, id, ...) {
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
