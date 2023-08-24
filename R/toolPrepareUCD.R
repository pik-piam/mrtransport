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
#' @importFrom data.table fread setkey setnames
#' @export

toolPrepareUCD <- function(magpieobj, subtype) {
  mapfile <- mappingUCD <- weight <- NULL

  mapfile <- system.file("extdata", "mappingUCDtoEDGET.csv",
                        package = "mredgetransport", mustWork = TRUE)
  mappingUCD <- fread(mapfile, skip = 0)
  setkey(mappingUCD, UCD_sector, mode, size_class, UCD_technology, UCD_fuel)
  weight <- readSource("UCD", subtype = "feDemand")
  #fe data is given only for 2005
  weight <- magpie2dt(weight)[, c("unit", "period", "variable") := NULL]
  setnames(weight, "value", "fe")

  # some technologies have zero or no demand for certain countries
  #-> set to 1 so that they are equally considered
  #e.g. CNG and LPG are both mapped on NG. In most countries the population of NG Trucks is zero.
  #To keep the information on the energy Intensity nevertheless for these countries, both NG and LPG
  #get a 1 as weight and are thus considered equally when calculating the energy Intensity of NG Trucks
  weight[fe == 0, fe := 1]
  weight[is.na(fe), fe := 1]

  dt <- magpie2dt(magpieobj)
  dt[, size_class := gsub("_", ".", size_class)]
  weight[, size_class := gsub("_", ".", size_class)]
  setkey(dt, region, UCD_sector, mode, size_class, UCD_technology, UCD_fuel, period)
  setkey(weight, region, UCD_sector, mode, size_class, UCD_technology, UCD_fuel)
  if (subtype %in% c("energyIntensity", "loadFactor", "CAPEX", "nonFuelOPEX", "CAPEXandNonFuelOPEX")) {
    dt <- merge(dt, weight, all.x = TRUE)
    dt <- merge(dt, mappingUCD, all.x = TRUE)
    dt <- dt[!sector == ""]
    dt <- unique(dt[, .(value = sum(value * fe) / sum(fe)), c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "univocalName", "variable", "unit", "period")])
  } else if (subtype == "annualMileage") {
    #Annual mileage is not technology/fuel specific in UCD
    dt <- unique(dt[, c("UCD_technology", "UCD_fuel") := NULL])
    weight <- weight[, .(fe = sum(fe)), by = .(region, UCD_sector, mode, size_class)]
    dt <- merge(dt, weight, all.x = TRUE, allow.cartesian = TRUE)
    dt <- merge(dt, mappingUCD, all.x = TRUE, allow.cartesian = TRUE)
    dt <- dt[!sector == ""]
    dt <- unique(dt[, .(value = sum(value * fe) / sum(fe)), by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "univocalName", "variable", "unit", "period")])
  } else if (subtype %in% c("feDemand", "nonMotorizedDemand")) {
    dt <- merge(dt, mappingUCD, all.x = TRUE)
    dt <- dt[!sector == ""]
    dt <- unique(dt[, .(value = sum(value)), by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "univocalName", "variable", "unit", "period")])
  } else if (subtype == "speed"){
    #the mapping for speed differs. Only the size classes Heavy Bus, Light Bus, Moped, Motorcycle (50-250cc), Motorcycle (>250cc) and Scooter are adressed seperately
    mappingUCD[!(size.class %in% c("Heavy Bus", "Light Bus", "Moped", "Motorcycle (50-250cc)", "Motorcycle (>250cc)", "Scooter")), size.class := "All"]
    dt <- merge(dt, mappingUCD, all.x = TRUE)
    dt <- dt[!sector == ""]
    dt <- unique(dt[, .(value = sum(value * fe) / sum(fe)), by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "univocalName", "variable", "unit", "period")])
  }
  dt <- dt[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "univocalName", "variable", "unit", "period", "value")]
  setkey(dt, region,  sector, subsectorL1, subsectorL2, subsectorL3, vehicleType, technology, univocalName, variable, unit, period)

  if (anyNA(dt) == TRUE) {
    stop("UCD data contains NAs")
  }
  return(dt)
}
