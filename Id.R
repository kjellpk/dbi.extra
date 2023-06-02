dbQualifyIds <- function(conn, ids, ...) {
  UseMethod("dbQualifyIds")
}


"dbQualifyIds.Microsoft SQL Server" <- function(conn, ids, ...) {
  if (!is.list(ids)) {
    ids <- list(ids)
  }

  #' @importFrom methods is
  #' @importFrom DBI Id
  if (!all(vapply(ids, is, FUN.VALUE = FALSE, class2 = "Id"))) {
    stop(deparse(substitute(ids)), " is not an ", sQuote("Id"), " or a list of ", sQuote("Id"), "s")
  }

  #' @importFrom DBI dbGetInfo
  default_catalog <- dbGetInfo(conn)$dbname
  #' @importFrom DBI dbGetQuery
  default_schema <- dbGetQuery(conn, "SELECT SCHEMA_NAME();")[[1]]

  SQL_Server_Hierarchy <- c("catalog", "schema", "table")  

  for(i in seq_along(ids)) {
    components <- ids[[i]]@name

    if (!all(names(components) %in% SQL_Server_Hierarchy)) {
      ignored <- setdiff(names(components), SQL_Server_Hierarchy)
      warning("components not in SQL Server hierarchy ignored: ", paste(ignored, collapse = ", "))
    }

    if (is.na(components["schema"])) {
      components["schema"] <- default_schema
    }
 
    if (is.na(components["catalog"])) {
      components["catalog"] <- default_catalog
    }

    #' @importFrom DBI Id
    ids[[i]] <- Id(components[intersect(SQL_Server_Hierarchy, names(components))])
  }

  ids
}

