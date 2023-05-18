#' Compact Storage for DBI Info
#'
#' @description Store database \emph{Info} in a compact data structure
#'              indexed by an \code{\link[DBI]{Id}}. This function is
#'              provided to simplify creating methods for DBI-extending
#'              packages.
#'
#' @param x a \code{\link{data.frame}} where one or more of the columns
#'          identifies a \emph{table}.
#'
#' @param idcols a character vector specifying the subset of the columns
#'               of \code{x} that will be passed to \code{\link[DBI]{Id}}.
#'
#' @export
split_by_id <- function(x, idcols) {
  keep <- setdiff(names(x), idcols)
  x <- split(x, f = x[, idcols])

  ids <- unname(lapply(x, `[`, i = 1, j = idcols, drop = FALSE))
  #' @importFrom DBI Id
  ids <- lapply(ids, function(u) Id(unlist(as.list(u))))

  cols <- list()
  for (k in keep) {
    u <- unname(lapply(x, `[[`, i = k))
    cols[[k]] <- u[!is.na(u) & nchar(u) > 0]
  }

  do.call(data.frame, c(list(id = I(ids)), lapply(cols, I)))
}


check_id <- function(id) {
  #' @importFrom methods is
  #' @importFrom DBI Id
  if (!is.null(id) & !is(id, "Id")) {
    stop(sQuote("id"), " arguement is not an ", sQuote("Id"))
  }

  invisible()
}
