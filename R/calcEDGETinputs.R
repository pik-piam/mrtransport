#' Provide EDGE-Transport input parameters
#' @author Johanna Hoppe
#' @param subtype one of the parameters required for EDGE-T
#' @importFrom rmndt approx_dt

calcEdgeTransportSAinputs <- function(subtype, adjustments = TRUE) {
  #logitStructure <- fread(system.file("extdata/logitStructure.csv", package = "mredgeTransport", mustWork = TRUE))
  logitStructure <- fread("C:/Users/johannah/Documents/Git_repos/mredgetransport/inst/extdata/logitStructure.csv")[, temporal := "all"][, spatial := "all"]
  years <- data.table(temporal = "all", period = c(1990,
             seq(2005, 2060, by = 5),
             seq(2070, 2110, by = 10),
             2130, 2150))
  ISOcountries <- fread("~/Git_repos/mredgeTransport/inst/extdata/regionmapping21EU11.csv")
  #countries <- system.file("extdata", "regionmapping21EU11.csv",
  #package = "mredgeTransport", mustWork = TRUE)
  #countries = fread(countries, skip = 0)
  setnames(ISOcountries, c("CountryCode"), c("region"))
  ISOcountriesEUR <- ISOcountries[missingH12 == "EUR", c("region")]$region
  ISOcountries <- ISOcountries[, c("region")][, spatial := "all"]

  completeDataSet <- merge(logitStructure, ISOcountries, by = "spatial", allow.cartesian = TRUE)[, spatial := NULL]
  #Some vehicleTypes and modes are not available in all countries
  logitMapCountrySpecificVehicleTypes <- fread("C:/Users/johannah/Documents/Git_repos/mredgetransport/inst/extdata/logitMapCountrySpecificVehicleTypes.csv")
  completeDataSet <- merge(completeDataSet, logitMapCountrySpecificVehicleTypes, by = c("region", "univocalName"), all = TRUE)
  completeDataSet <- completeDataSet[present == 1][, present := NULL]
  completeDataSet <- merge(completeDataSet, years, by = "temporal", allow.cartesian = TRUE)[, temporal := NULL]
  years <- years[, c(period)]
  setkey(completeDataSet, region,  sector, subsectorL3, subsectorL2, subsectorL1, vehicleType, technology, period, univocalName)

  switch(
    subtype,
    "energyIntensity" ={
      unit <- "MJ/vehkm"
      description <- "Energy intensity on technology level. Sources: TRACCS, PSI, UCD, GCAM"
      weight <- calcOutput("GDP", aggregate = FALSE)[, unique(q$period), "gdp_SSP2"]

      #enIntGCAM <- toolPrepareGCAM(readSource("GCAM", subtype), subtype)
      enIntGCAM <- toolPrepareGCAM(convertGCAM(readGCAM(subtype), subtype), subtype)
      #enIntUCD <- toolPrepareUCD(readSource("UCD", subtype), subtype)
      enIntUCD <- toolPrepareUCD(convertUCD(readUCD(subtype), subtype), subtype)
      #enIntTRACCS <- toolPrepareTRACCS(readSource("TRACCS", subtype), subtype)
      enIntTRACCS <- toolPrepareTRACCS(convertTRACCS(readTRACCS(subtype), subtype), subtype)
      #enIntPSI <- toolPreparePSI(readSource("PSI", subtype), subtype)
      enIntPSI <- toolPreparePSI(convertPSI(readPSI(subtype), subtype), subtype)
      #PSI energy intensity needs to be transformed to MJ/vehkm
      kJPerVehkmtoMJperVehkm <- 1e-3
      enIntPSI[, value := value * kJPerVehkmtoMJperVehkm][, unit := "MJ/vehkm"]
      #Inter- and extrapolate all data to model input data years
      data <- list(enIntGCAM = enIntGCAM, enIntUCD = enIntUCD, enIntTRACCS = enIntTRACCS, enIntPSI = enIntPSI)
      data <- lapply(data, approx_dt, years, "period", "value",
               c("region", "unit", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName"), extrapolate = TRUE)

      #TRACCS>PSI>GCAM
      #1: Used parts of TRACCS energy intensity in raw data
      #-TRACCS data is used completetly but originally lasts only until 2010 + includes only EUR iso countries except for "ALA" (Aland Islands)

      #2: Used parts of PSI energy intensity in raw data
      #- Used for Trucks in non-EUR countries extrapolated to all years and all non-EUR countries
      #- Used for alternative Cars (BEV,FCEV,HEV) extrapolated to all years and all countries
      #- Trends are applied on the TRACCS data for EUR iso countries except "ALA" to future years > 2010 in toolAdjustEnergyIntensity
      energyIntensityRawPSITrucks <- data$enIntPSI[vehicleType %in% c("Truck (0-3.5t)", "Truck (18t)", "Truck (26t)", "Truck (40t)") & !(region %in% ISOcountriesEUR)]
      energyIntensityRawPSIalternativeCars <- data$enIntPSI[technology %in% c("BEV", "FCEV", "Hybrid Electric")]

      #3: Used parts of the GCAM energy Intensity
      #- Only conventional cars (Liquids, NG) in non-EUR countries
      #- All other data for non-EUR countries
      #- "ALA" is a special case because it is not included in the TRACCS data
      energyIntensityRawGCAMconventionalCars <- data$enIntGCAM[subsectorL1 == "trn_pass_road_LDV_4W" & technology %in% c("Liquids", "NG") & (!(region %in% ISOcountriesEUR) | region == "ALA")]
      energyIntensityRawGCAMnonCarsnonEUR <- data$enIntGCAM[!(subsectorL1 == "trn_pass_road_LDV_4W") & (!(region %in% ISOcountriesEUR) | region == "ALA")]

      energyIntensityRaw <- rbind(enIntTRACCS, energyIntensityRawPSITrucks, energyIntensityRawPSIalternativeCars, energyIntensityRawGCAMconventionalCars,
                                  energyIntensityRawGCAMnonCarsnonEUR)

      setkey(energyIntensityRaw, region,  sector, subsectorL3, subsectorL2, subsectorL1, vehicleType, technology, period, univocalName)

      #Include data adjustments: fill gaps and correct data if necessary based on projects, other sourced and own assumptions
      energyIntensity <- toolAdjustEnergyIntensity(energyIntensityRaw, ISOcountriesEUR, data$enIntPSI)

      #Check whether data is complete
      check <- merge(completeDataSet, energyIntensity, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("Energy intensity input data is incomplete")
      }

    },

    "annualMileage" = {

    },

    "histEsDemand" = {

    },

    "loadFactor" = {
    }
  )

  return(list(
    x           = as.magpie(as.data.frame(q)),
    weight      = weight,
    unit        = unit,
    description = description
  ))
}
