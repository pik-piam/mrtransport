#' Perform source specific transformations to ensure a compatible structure.
#'
#' Map the source categories to the EDGE-T categories. Apply the full structure of the decision tree.
#'
#' @author Johanna Hoppe
#' @param x the input data read via readSource, a magpie object
#' @return a quitte object

#' @import data.table
#' @importFrom rmndt magpie2dt
#' @export

toolPrepareEurostatEnergyCountryDataSheets <- function(x) {
  region <- Eurostatsector <- period     <-
    technology <- univocalName <- variable <- unit <- period <- NULL

  mapfile <- system.file("extdata", "mappingEurostatToEDGET.csv", package = "mrtransport", mustWork = TRUE)
  mappingEurostat <- fread(mapfile, skip = 0)
  setkey(mappingEurostat, Eurostatsector)
  dt <- magpie2dt(x)
  setkey(dt, region, Eurostatsector, period)

  dt <- merge.data.table(dt, mappingEurostat, all.x = TRUE, allow.cartesian = TRUE)

  dt <- dt[, c("region", "period", "univocalName", "technology", "variable", "unit", "value")]
  setkey(dt, region, period, univocalName, technology, variable, unit)

  if (anyNA(dt) == TRUE) {
    stop("Eurostat data contains NAs")
  }
  return(dt)
}
