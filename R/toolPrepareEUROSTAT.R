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
#' @importFrom data.table fread

toolPrepareEUROSTAT <- function(magpieobj, subtype) {

  mapfile <- system.file("extdata", "mappingEUROSTATtoEDGET.csv",
   package = "mredgetransport", mustWork = TRUE)
  mappingEUROSTAT = fread(mapfile, skip = 0)
  setkey(mappingEUROSTAT, EUROSTAT)

  dt <- magpie2dt(magpieobj)
  setnames(dt, "variable", "EUROSTAT")
  setkey(dt, region, EUROSTAT, period)

  dt <- merge(dt, mappingEUROSTAT, all.x = TRUE, allow.cartesian = TRUE)

  dt <- dt[, c("region", "period", "unit", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName", "value")]
  setkey(dt, region,  sector, subsectorL3, subsectorL2, subsectorL1, vehicleType, technology, period, unit, univocalName)
  return(dt)
}
