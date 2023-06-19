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

toolPrepareTRACCS <- function(magpieobj, subtype) {

  mapfile <- system.file("extdata", "mappingTRACCStoEDGET.csv",
   package = "mredgetransport", mustWork = TRUE)
  mappingTRACCS = fread(mapfile, skip = 0)
  setkey(mappingTRACCS, TRACCS_category, TRACCS_vehicle_type, TRACCS_technology)
  weight <- readSource("TRACCS", subtype = "vehPopulation")
  weight <- magpie2dt(weight)[, unit := NULL]
  setkey(weight, region,  TRACCS_category, TRACCS_vehicle_type, TRACCS_technology, period)
  setnames(weight, "value", "vehPop")

  # some technologies have zero or no demand for certain countries
  #-> set to 1 so that they are equally considered
  #e.g. CNG and LPG are both mapped on NG. In most countries the population of NG Trucks is zero.
  #To keep the information on the energy Intensity nevertheless for these countries, both NG and LPG
  #get a 1 as weight and are thus considered equally when calculating the energy Intensity of NG Trucks
  weight[vehPop == 0, vehPop := 1]

  dt <- magpie2dt(magpieobj)
  dt <- dt[TRACCS_technology != "Other"]
  setkey(dt, region, TRACCS_category, TRACCS_vehicle_type, TRACCS_technology, period)

  if (subtype %in% c("energyIntensity", "loadFactor", "annualMileage")) {
    dt <- merge(dt, weight, all.x = TRUE)
    dt <- merge(dt, mappingTRACCS, all.x = TRUE)
    dt <- dt[!sector == ""]
    dt <- unique(dt[, .(value = sum(value * vehPop) / sum(vehPop)), by = c("region", "period", "unit", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName")])
  } else if (subtype %in% c("roadFeDemand", "roadVkmDemand", "histEsDemand", "railFeDemand", "vehPopulation")){
    dt <- merge(dt, mappingTRACCS, all.x = TRUE)
    dt <- dt[!sector == ""]
    dt <- unique(dt[, .(value = sum(value)), by = c("region", "period", "unit", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName")])
  } else if (subtype == "fuelEnDensity") {
    #do nothing as fuel energy density cannot be mapped on EDGE-T structure
  }

  dt <- dt[, c("region", "period", "unit", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName", "value")]
  setkey(dt, region,  sector, subsectorL3, subsectorL2, subsectorL1, vehicleType, technology, period, unit, univocalName)

}
