#' Perform source specific transformations to ensure a compatible structure.
#'
#' Map the source categories to the EDGE-T categories. Apply the full structure of the decision tree.
#'
#' @author Johanna Hoppe
#' @param x the input data read via readSource, a magpie object
#' @param subtype one of the different EDGE-T inputdata subtypes
#' @return a quitte object
#'
#' @import data.table
#' @importFrom rmndt magpie2dt

toolPrepareTRACCS <- function(x, subtype) {
  subsectorL3 <- variable <- . <- subsectorL1 <- subsectorL2 <- technology <- period <- value <-
    univocalName <- vehicleType <- unit <- region <- sector <- technology <- vehicleType <-
    TRACCS_category <- TRACCS_vehicle_type <- TRACCS_technology <- vehPop <- NULL

  mapfile <- system.file("extdata", "mappingTRACCStoEDGET.csv",
   package = "mredgetransport", mustWork = TRUE)
  mappingTRACCS <- fread(mapfile, skip = 0)
  setkey(mappingTRACCS, TRACCS_category, TRACCS_vehicle_type, TRACCS_technology)
  weight <- readSource("TRACCS", subtype = "fleetData")
  weight <- magpie2dt(weight)[, c("unit", "variable") := NULL]
  setkey(weight, region,  TRACCS_category, TRACCS_vehicle_type, TRACCS_technology, period)
  setnames(weight, "value", "vehPop")

  # some technologies have zero or no demand for certain countries
  #-> set to 1 so that they are equally considered
  #e.g. CNG and LPG are both mapped on NG. In most countries the population of NG Trucks is zero.
  #To keep the information on the energy Intensity nevertheless for these countries, both NG and LPG
  #get a 1 as weight and are thus considered equally when calculating the energy Intensity of NG Trucks
  weight[vehPop == 0, vehPop := 1]

  dt <- magpie2dt(x)
  dt <- dt[TRACCS_technology != "Other"]
  setkey(dt, region, TRACCS_category, TRACCS_vehicle_type, TRACCS_technology, period)

  if (subtype %in% c("energyIntensity", "loadFactor", "annualMileage")) {
    dt <- merge.data.table(dt, weight, all.x = TRUE)
    dt <- merge.data.table(dt, mappingTRACCS, all.x = TRUE)
    dt <- dt[!sector == ""]
    dt <- unique(dt[, .(value = sum(value * vehPop) / sum(vehPop)),
                    by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType",
                           "technology", "univocalName", "variable", "unit", "period")])
  } else if (subtype %in% c("roadFuelConsumption", "roadESdemand", "histESdemand", "railFeDemand", "fleetData")) {
    dt <- merge.data.table(dt, mappingTRACCS, all.x = TRUE)
    dt <- dt[!sector == ""]
    dt <- unique(dt[, .(value = sum(value)),
                    c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3",
                    "vehicleType", "technology", "univocalName", "variable", "unit", "period")])
  } else if (subtype == "fuelEnDensity") {
    #do nothing as fuel energy density cannot be mapped on EDGE-T structure
  }

  dt <- dt[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType",
               "technology", "univocalName", "variable", "unit", "period", "value")]
  setkey(dt, region,  sector, subsectorL1, subsectorL2, subsectorL3, vehicleType, technology,
         univocalName, variable, unit, period)
  if (anyNA(dt) == TRUE) {
    stop("TRACCS data contains NAs")
  }
  return(dt)
}
