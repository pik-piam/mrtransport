#' Perform source specific transformations to ensure a compatible structure.
#'
#' Map the source categories to the EDGE-T categories. Apply the full structure of the decision tree.
#'
#' @author Johanna Hoppe
#' @param magpieobj the input data read via readSource, a magpie object
#' @param sourcetype one of the different EDGE-T inputdata sources
#' @return a quitte object
#'
#' @importFrom rmndt magpie2dt
#' @importFrom data.table fread setnames setkey merge
#' @export

toolPrepareEUROSTAT <- function(magpieobj, subtype) {
  mapfile <- mappingEUROSTAT <- dt <- NULL

  mapfile <- system.file("extdata", "mappingEUROSTATtoEDGET.csv",
   package = "mredgetransport", mustWork = TRUE)
  mappingEUROSTAT <- fread(mapfile, skip = 0)
  setkey(mappingEUROSTAT, EUROSTATsector)
  dt <- magpie2dt(magpieobj)
  setkey(dt, region, EUROSTATsector, period)

  dt <- merge(dt, mappingEUROSTAT, all.x = TRUE, allow.cartesian = TRUE)

  dt <- dt[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "univocalName", "variable", "unit", "period", "value")]
  setkey(dt, region, sector, subsectorL1, subsectorL2, subsectorL3, vehicleType, technology, univocalName, variable, unit, period)

  if (nrow(dt[is.na(value)]) > 0) {
    stop("EUROSTAT data contains NAs")
  }
  return(dt)
}
