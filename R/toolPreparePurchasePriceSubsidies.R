#' Perform source specific transformations to ensure a compatible structure.
#'
#' Map the source categories to the EDGE-T categories. Apply the full structure of the decision tree.
#'
#' @author Caroline Cronjaeger
#' @param x the input data read via readSource, a magpie object
#' @return a quitte object

#' @import data.table
#' @importFrom rmndt magpie2dt
#' @export
#'

toolPreparePurchasePriceSubsidies <- function(x) {

  region <- subsidiesVehicleType <- subsidiesTechnology <- unit <- NULL

  mapfile <- system.file("extdata", "mappingSubsidiesToEDGET.csv",
                         package = "mrtransport", mustWork = TRUE)
  mappingSubsidies <- fread(mapfile, skip = 0)
  setkey(mappingSubsidies, subsidiesVehicleType, subsidiesTechnology)
  dt <- magpie2dt(x)
  setkey(dt, region, subsidiesVehicleType, subsidiesTechnology)

  dt <- merge.data.table(dt, mappingSubsidies, all.x = TRUE, allow.cartesian = TRUE)
  monUnit <- gsub(".*?(\\d{4}).*", "US$\\1", mrdrivers::toolGetUnitDollar())
  dt[, unit := paste0(monUnit, "/veh")]
  dt <- dt[, c("region", "period", "univocalName", "technology", "variable", "unit", "value")]

  return(dt)
}
