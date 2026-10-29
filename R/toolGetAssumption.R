#' Get an assumption by ID
#'
#' Look up the values of one assumption in the EDGETassumptions source.
#'
#' @param id assumption ID, e.g. "A01"
#' @param by column used for the names of the result
#' @param ... equality filters on other columns, e.g. technology = "BEV"
#' @return named numeric vector of the values, named by column `by`
#' @details Source: EDGETassumptions v1 (README.txt in the source folder).
#' @author Jarusch Muessel
#' @importFrom madrat readSource
toolGetAssumption <- function(id, by = "univocalName", ...) {
  tab <- readSource("EDGETassumptions", subtype = "v1", convert = FALSE)
  rows <- tab[tab$id == id, ]
  if (nrow(rows) == 0) stop("toolGetAssumption: no rows for ID ", id)
  filters <- list(...)
  for (col in names(filters)) rows <- rows[rows[[col]] == filters[[col]], ]
  if (nrow(rows) == 0) stop("toolGetAssumption: no rows for ID ", id, " after filtering")
  if (anyDuplicated(rows[[by]])) stop("toolGetAssumption: names by '", by, "' not unique for ID ", id)
  stats::setNames(rows$value, rows[[by]])
}
