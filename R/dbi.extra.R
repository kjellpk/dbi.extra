#Private package environment
e <- new.env(parent = emptyenv())



.onLoad <- function(libname, pkgname) {
  pkg_ns <- asNamespace(pkgname)

  chinook_fun <- function() {
    if (is.null(e$chinook_conn)) {
      path <- system.file(file.path("samples", "Chinook_Sqlite.sqlite"),
                          package = "dbi.extra")
      #' @importFrom DBI dbConnect
      #' @importFrom RSQLite SQLite
      e$chinook_conn <- dbConnect(SQLite(), path)
    }
    e$chinook_conn
  }

  makeActiveBinding("chinook", chinook_fun, env = pkg_ns)

  NULL
}



.onUnload <- function(libpath) {
  if (!is.null(e$chinook_conn)) {
    #' @importFrom DBI dbDisconnect
    try(dbDisconnect(e$chinook_conn), silent = TRUE)
  }

  NULL
}



#' Chinook SQLite sample database in the \pkg{dbi.extra} package
#' @usage chinook
#' @name chinook
#' @export
NULL
