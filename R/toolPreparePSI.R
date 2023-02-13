#' Perform source specific transformations to ensure a compatible structure.
#'
#' Map the source categories to the EDGE-T categories. Apply the full logit structure.
#'
#' @author Johanna Hoppe
#' @param magpieobj the input data read via readSource, a magpie object
#' @param sourcetype one of the different EDGE-T inputdata sources
#' @return a quitte object
#'
#' @importFrom rmndt magpie2dt
#' @importFrom data.table fread

toolPreparePSI <- function(magpieobj, subtype) {
  mappingPSI <- fread("~/Git_repos/mredgeTransport/inst/extdata/mappingPSItoEDGET.csv")
  #mapfile <- system.file("extdata", "mappingPSItoEDGET.csv",
  # package = "mredgeTransport", mustWork = TRUE)
  #mappingPSI = fread(mapfile, skip = 0)
  setkey(mappingPSI, technologyPSI, vehicleTypePSI)
  dt <- magpie2dt(magpieobj)
  dt[, vehicleTypePSI := gsub("_",".", vehicleTypePSI)]
  dt <- merge(dt, mappingPSI, all.x = TRUE, by = c("technologyPSI", "vehicleTypePSI"))
  dt <- dt[!sector == ""]
  dt <- unique(dt[, .(region, period, unit, sector, subsectorL3, subsectorL2, subsectorL1, vehicleType, technology, univocalName, value)])
  setkey(dt, region,  sector, subsectorL3, subsectorL2, subsectorL1, vehicleType, technology, period, unit, univocalName)

  return(dt)
}
