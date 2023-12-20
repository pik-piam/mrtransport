#' Provide EDGE-Transport input parameters
#' @author Johanna Hoppe
#' @param subtype one of the parameters required for EDGE-T SA
#' @param IEAharm switch IEA harmonization of energy intensity on and off
#' @param SSPscen shared socioeconomic pathway
#' @import data.table
#' @import mrremind
#' @importFrom rmndt approx_dt
#' @importFrom madrat readSource calcOutput

calcEdgeTransportSAinputs <- function(subtype, SSPscen = "SSP2EU", IEAharm = TRUE) { # nolint: cyclocomp_linter

  temporal <- spatial <- present <- period <- region <-
    subsectorL2   <- technology <- univocalName <- gdppc <- speed <-
    altTech <- variable <- value <- regionCode12 <- multiplier <- NULL

  years <- data.table(temporal = "all", period = c(
    1990,
    seq(2005, 2060, by = 5),
    seq(2070, 2110, by = 10),
    2130, 2150
  ))
  # decisionTree.csv contains all possible branches of the decision tree
  decisionTree <- fread(system.file("extdata/decisionTree.csv", package = "mrtransport", mustWork = TRUE))
  decisionOptions <- decisionTree[, c("univocalName", "technology")]
  decisionOptions[, temporal := "all"][, spatial := "all"]
  # Not all countries feature the same branches of the decision tree - Some vehicleTypes and modes are not
  # available in certain countries
  # Here we create the full structure of the nested decision tree differentiated for all countries to make it testable
  ISOcountriesMap <- system.file("extdata", "regionmappingISOto21to12.csv",
    package = "mrtransport", mustWork = TRUE
  )
  ISOcountriesMap <- fread(ISOcountriesMap, skip = 0)
  setnames(ISOcountriesMap, c("countryCode"), c("region"))
  ISOcountries <- ISOcountriesMap[, c("region")][, spatial := "all"]

  completeDataSet <- merge.data.table(decisionOptions, ISOcountries, by = "spatial", allow.cartesian = TRUE)
  completeDataSet[, spatial := NULL]
  mapCountrySpecificVehicleTypes <- fread(system.file("extdata/mapCountrySpecificVehicleTypes.csv",
    package = "mrtransport", mustWork = TRUE))

  completeDataSet <- merge.data.table(completeDataSet, mapCountrySpecificVehicleTypes,
    by = c("region", "univocalName"), all = TRUE)
  completeDataSet <- completeDataSet[present == 1][, present := NULL]
  completeDataSet <- merge.data.table(completeDataSet, years, by = "temporal", allow.cartesian = TRUE)
  completeDataSet[, temporal := NULL]
  completeDataSet[, check := 1]
  years <- years[, c(period)]
  setkey(completeDataSet, region, period, univocalName, technology)

  # categories for filtering data
  categories <- c("trn_pass_road_LDV_4W", "trn_pass_road_LDV_2W", "trn_freight_road", "trn_pass", "trn_freight", "trn_pass_road")

   findEntries <- function(category, dataTable){
     test <- dataTable[, lapply(.SD, function(x) grepl(category, x))]
     entries <- unique(decisionTree[rowSums(test) > 0]$univocalName)
   }

  filterEntries <- sapply(categories, findEntries, dataTable = decisionTree, USE.NAMES = TRUE)

  switch(subtype,
    "energyIntensity" = {
      unit <- "MJ/vehkm"
      description <- "Energy intensity on technology level. Sources: TRACCS, PSI, UCD, GCAM"
      weight <- calcOutput("GDP", aggregate = FALSE)[, years, paste0("gdp_", SSPscen)]

      # calc different source data
      enIntGCAM <- toolPrepareGCAM(readSource("GCAM", subtype), subtype)
      enIntUCD <- toolPrepareUCD(readSource("UCD", subtype), subtype)
      enIntTRACCS <- toolPrepareTRACCS(readSource("TRACCS", subtype), subtype)
      enIntPSI <- toolPreparePSI(readSource("PSI", subtype))

      # Inter- and extrapolate all data to model input data years
      data <- list(enIntGCAM = enIntGCAM, enIntUCD = enIntUCD, enIntTRACCS = enIntTRACCS, enIntPSI = enIntPSI)
      data <- lapply(data, approx_dt, years, "period", "value",
                     c("region", "univocalName","technology", "variable", "unit"), extrapolate = TRUE)

      # merge.data.table data
      # TRACCS>PSI>GCAM
      # 1: TRACCS data
      # Used parts of TRACCS energy Intensity: TRACCS data is used completely
      countriesTRACCS <- unique(data$enIntTRACCS$region)

      # 2: GCAM data
      # Used parts of the GCAM energy Intensity:
      # Conventional cars (Liquids, NG) in non-TRACCS countries
      energyIntensityRawGCAMconventionalCarsnonTRACCS <- data$enIntGCAM[univocalName %in% filterEntries$trn_pass_road_LDV_4W &
                                                         technology %in% c("Liquids", "NG") &
                                                         !region %in% countriesTRACCS]
      # All other data for non-TRACCS countries except for Trucks
      energyIntensityRawGCAMnonCarsnonTRACCS <- data$enIntGCAM[!univocalName %in% filterEntries$trn_pass_road_LDV_4W &
                                                               !univocalName %in% filterEntries$trn_freight_road &
                                                               !region %in% countriesTRACCS]
      # Energy Intensity data for Freight Rail, Passenger Rail, HSR, Domestic Aviation, International Aviation,
      # Domestic Shipping, International Shipping is not provided by TRACCS. Hence GCAM data is used for all countries
      energyIntensityRawGCAMmissingTRACCScat <- data$enIntGCAM[univocalName %in% c("Freight Rail", "Passenger Rail",
                                                               "HSR", "Domestic Aviation", "International Aviation",
                                                               "Domestic Ship", "International Ship") &
                                                               region %in% countriesTRACCS]
      # Alternative technologies for motorcycles are missing in the TRACCS database and are taken from GCAM also
      # for TRACCS countries
      energyIntensityRawGCAMalt2WheelersTRACCSreg <- data$enIntGCAM[univocalName %in% filterEntries$trn_pass_road_LDV_2W &
                                                                    region %in% countriesTRACCS & technology == "BEV"]

      #3: PSI data
      # Used for Trucks in non-TRACCS countries
      energyIntensityRawPSITrucks <- data$enIntPSI[univocalName %in% filterEntries$trn_freight_road & !region %in% countriesTRACCS]
      # TRACCS data does not include NG Truck (7.5t), Truck (18t), Truck (26t), Truck (40t) -> data is taken from PSI
      # TRACCS data does not include NG Truck (7_5t), Truck (18t), Truck (26t), Truck (40t) -> data is taken from PSI
      energyIntensityRawPSItrucksNGTRACCSreg <- data$enIntPSI[univocalName %in% c("Truck (7_5t)", "Truck (18t)",
                                                              "Truck (26t)", "Truck (40t)") &
                                                              technology == "NG" & region %in% countriesTRACCS]
      # Used for alternative Cars (BEV,FCEV,HEV) in TRACCS countries
      energyIntensityRawPSIalternativeTechTRACCSreg <- data$enIntPSI[technology %in% c("BEV", "FCEV", "Hybrid electric")
                                                                     & region %in% countriesTRACCS]
      # Use only data for vehicle types that are listed in the TRACCS data base
      TRACCSVehTypes <- copy(data$enIntTRACCS)
      TRACCSVehTypes <- unique(TRACCSVehTypes[, c("value", "technology") := NULL])
      # Apply only on the vehicle types that are in general available from the PSI dataset
      # (2 Wheelers and Busses are not provided by PSI)
      TRACCSVehTypes <- TRACCSVehTypes[univocalName %in% unique(energyIntensityRawPSIalternativeTechTRACCSreg$univocalName)] # nolint: line_length_linter
      energyIntensityRawPSIalternativeTechTRACCSreg <- merge.data.table(energyIntensityRawPSIalternativeTechTRACCSreg,
                                                                        TRACCSVehTypes, all.y = TRUE,
                                                                        by = c("region", "univocalName", "variable", "unit", "period"))
      # Used for alternative Cars (BEV,FCEV,HEV) in non-TRACCS countries
      # For non TRACCS iso countries the available vehicle types differ.
      # Use the additional data on alternative Cars only for the existing vehicle types in GCAM
      energyIntensityRawPSIalternativeCarsnonTRACCS <- data$enIntPSI[technology %in% c("BEV", "FCEV", "Hybrid electric") & # nolint: line_length_linter
                                                                     !region %in% countriesTRACCS]
      # Create structure for GCAM vehicle types and alternative tech options
      GCAMVehTypes <- energyIntensityRawGCAMconventionalCarsnonTRACCS[univocalName %in% filterEntries$trn_pass_road_LDV_4W &
                                                                      !region %in% countriesTRACCS]
      GCAMVehTypes <- unique(GCAMVehTypes[, c("value", "technology", "variable", "unit") := NULL])[, altTech := 1]
      AltTechOpt <- data.table(technology = c("BEV", "FCEV", "Hybrid electric"), altTech = c(1, 1, 1))
      GCAMVehTypes <- merge.data.table(GCAMVehTypes, AltTechOpt, by = "altTech", allow.cartesian = TRUE)
      GCAMVehTypes[, altTech := NULL]
      energyIntensityRawPSIalternativeCarsnonTRACCS <- merge.data.table(energyIntensityRawPSIalternativeCarsnonTRACCS,
                                                       GCAMVehTypes, all.y = TRUE, by = c("region", "period", "univocalName",
                                                        "technology"))
      # Large Car is missing for some nonTRACCS regions, filled in toolAdjustEnergyIntensity
      # -> NAs are introduced for unit & variable
      energyIntensityRawPSIalternativeCarsnonTRACCS[, variable := "Energy intensity"][, unit := "MJ/vehkm"]

      energyIntensityRaw <- rbind(
        data$enIntTRACCS, energyIntensityRawGCAMconventionalCarsnonTRACCS, energyIntensityRawGCAMmissingTRACCScat,
        energyIntensityRawGCAMnonCarsnonTRACCS, energyIntensityRawGCAMalt2WheelersTRACCSreg,
        energyIntensityRawPSITrucks, energyIntensityRawPSItrucksNGTRACCSreg,
        energyIntensityRawPSIalternativeTechTRACCSreg,
        energyIntensityRawPSIalternativeCarsnonTRACCS)

      # Include data adjustments: fill gaps and correct data if necessary based on projects, other sources and own
      # assumptions
      energyIntensity <- toolAdjustEnergyIntensity(energyIntensityRaw, countriesTRACCS, data$enIntPSI, filterEntries)
      # Harmonize energy intensity data in order to match IEA final energy values
      if (IEAharm == TRUE) {
        energyIntensity <- mrtransport::toolIEAharmonization(energyIntensity)
      }

      # Add energy intensity of zero for active modes
      activeModes <- completeDataSet[univocalName %in% c("Cycle", "Walk")]
      activeModes[, unit := "MJ/vehkm"][, variable := "Energy intensity"][, value := 0][, check := NULL]
      energyIntensity <- rbind(energyIntensity, activeModes)

      energyIntensity <- energyIntensity[, c("region", "period", "univocalName", "technology",
                    "variable", "unit", "value")]
      setkey(energyIntensity,  region, period, univocalName, technology, variable, unit)

      # Check whether data is complete
      check <- merge.data.table(completeDataSet, energyIntensity, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("Energy intensity input data is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("Unnecessary data is provided")
      } else if (length(unique(check$unit)) > 1) {
        stop("Something went wrong in generating energy intensity input data.
             Data does not have the same unit.")
      } else if (length(unique(check$variable)) > 1) {
        stop("Something went wrong in generating energy intensity input data.
             Data does not have the same variable type.")
      } else if (anyNA(energyIntensity) == TRUE) {
        stop("Energy intensity data includes NAs")
      }

      quitteobj <- energyIntensity
    },
    "annualMileage" = {
      unit <- "vehkm/veh/yr"
      description <- "Annual mileage on technology level. Sources: TRACCS, UCD"
      weight <- calcOutput("GDP", aggregate = FALSE)[, years, paste0("gdp_", SSPscen)]

      # calc different source data
      AMTRACCS <- toolPrepareTRACCS(readSource("TRACCS", subtype), subtype)
      AMUCD <- toolPrepareUCD(readSource("UCD", subtype), subtype)

      # Inter- and extrapolate all data to model input data years
      data <- list(AMTRACCS = AMTRACCS, AMUCD = AMUCD)
      data <- lapply(data, approx_dt, years, "period", "value",
        c("region", "univocalName", "technology",
          "variable", "unit"), extrapolate = TRUE)

      # merge.data.table data
      # TRACCS is used first, than UCD
      # EU data is used from TRACCS, rest is filled with UCD
      annualMileageRaw <- rbind(data$AMTRACCS, data$AMUCD[!(region %in% unique(data$AMTRACCS$region))])

      setkey(annualMileageRaw, region, period, univocalName, technology, variable, unit)

      annualMileage <- toolAdjustAnnualMileage(annualMileageRaw, completeDataSet, filterEntries)

      # Add annual mileage of zero for active modes
      activeModes <- completeDataSet[univocalName %in% c("Cycle", "Walk")]
      activeModes[, unit := "vehkm/veh/yr"][, variable := "Annual mileage"][, value := 0][, check := NULL]
      annualMileage <- rbind(annualMileage, activeModes)

      annualMileage <- annualMileage[, c("region", "period", "univocalName", "technology",
                                 "variable", "unit", "value")]
      setkey(annualMileage, region, period, univocalName, technology, variable, unit)

      # Check whether data is complete
      check <- merge.data.table(completeDataSet, annualMileage, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("Annual mileage input data is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("Unnecessary data is provided")
      } else if (length(unique(check$unit)) > 1) {
        stop("Something went wrong in generating annual mileage input data. Data does not have the same unit.")
      } else if (length(unique(check$variable)) > 1) {
        stop("Something went wrong in generating annual mileage input data. Data does not have the same variable type.")
      } else if (anyNA(annualMileage) == TRUE) {
        stop("Annual mileage data includes NAs")
      }

      quitteobj <- annualMileage
    },
    "histESdemand" = {
      unit <- "billion (p|t)km/yr"
      description <- "Energy service demand on technology level. Sources: GCAM, TRACCS, EUROSTAT"
      weight <- NULL

      # calc different source data
      esDemandGCAM <- toolPrepareGCAM(readSource("GCAM", subtype), subtype)
      esDemandTRACCS <- toolPrepareTRACCS(readSource("TRACCS", subtype), subtype)
      feDemandEUROSTAT <- toolPrepareEUROSTAT(readSource("EUROSTAT", "feDemand"))
      enIntensity <- magpie2dt(calcOutput(type = "EdgeTransportSAinputs", subtype = "energyIntensity",
                                          IEAharm = FALSE, warnNA = FALSE, aggregate = FALSE))
      enIntensity[, unit := NULL][, variable := NULL]
      loadFactor <- magpie2dt(calcOutput(type = "EdgeTransportSAinputs", subtype = "loadFactor",
                                         warnNA = FALSE, aggregate = FALSE))
      loadFactor[, unit := NULL][, variable := NULL]

      # Inter- and extrapolate all data to model input data years
      data <- list(esDemandGCAM = esDemandGCAM, esDemandTRACCS = esDemandTRACCS, feDemandEUROSTAT = feDemandEUROSTAT)
      # The historical energy service demand is only used for years <= 2010, future years will be calculated by demand
      # regression in the model
      data <- lapply(data, approx_dt, years[years <= 2010], "period", "value",
        c("region", "univocalName", "technology",
           "variable", "unit"), extrapolate = TRUE)

      # Calc Energy Service demand based on FE data from EUROSTAT for bunkers
      setnames(enIntensity, "value", "enIntensity")
      setnames(loadFactor, "value", "loadFactor")
      esDemandEUROSTAT <- merge.data.table(data$feDemandEUROSTAT, enIntensity,
                          by = c("region", "univocalName", "technology", "period"))
      esDemandEUROSTAT <- merge.data.table(esDemandEUROSTAT, loadFactor, by = c("region", "period", "univocalName", "technology"))
      toBillion <- 1e-09
      esDemandEUROSTAT[, value := (value / enIntensity) * loadFactor * toBillion][, c("enIntensity", "loadFactor") := NULL]
      esDemandEUROSTAT[univocalName %in% c(filterEntries$trn_pass, "International Aviation"), unit := "billion pkm/yr"]
      esDemandEUROSTAT[univocalName %in% c(filterEntries$trn_freight, "International Ship"), unit := "billion tkm/yr"]
      esDemandEUROSTAT[, variable := "Energy service demand"]

      # merge.data.table data
      # TRACCS data is used completely
      # EUROSTAT data is used completely
      # GCAM data is used for regions that are not included in TRACCS, bunkers for regions that are not included
      # in EUROSTAT (non EU-27) and modes that are not included in TRACCS
      # CHE, GBR, ISL, MKD, NOR, TUR are included in TRACCS but not in EUROSTAT
      missingBunkers <- data$esDemandGCAM[region %in% c("CHE", "GBR", "ISL", "MKD", "NOR", "TUR") &
                                          univocalName %in% c("International Aviation", "Domestic Aviation",
                                          "Domestic Ship", "International Ship")]
      # GCAM is used for modes not provided by TRACCS for TRACCS regions. 4 Wheelers must be excluded as GCAM
      # uses different vehicle types and bunkers are used from EUROSTAT
      # For some reason energy service demand for Truck(0-3_5t)/Light commercial vehicles is not reported by TRACCS
      # -> also taken from GCAM
      missingModes <- data$esDemandGCAM[region %in% unique(data$esDemandTRACCS$region) &
        !univocalName %in% unique(data$esDemandTRACCS$univocalName) &
        !univocalName %in% c("International Aviation", "Domestic Aviation", "Domestic Ship", "International Ship") &
        !univocalName %in% filterEntries$trn_pass_road_LDV_4W]
      esDemandRaw <- rbind(
        data$esDemandTRACCS, esDemandEUROSTAT, data$esDemandGCAM[!(region %in% unique(data$esDemandTRACCS$region))],
        missingBunkers, missingModes
      )

      esDemand <- toolAdjustEsDemand(esDemandRaw, ISOcountriesMap, completeDataSet, filterEntries)

      esDemand <- esDemand[, c("region", "period", "univocalName", "technology",
                               "variable", "unit", "value")]
      setkey(esDemand,  region, period, univocalName, technology,
             variable, unit)

      # Check whether data is complete
      check <- merge.data.table(completeDataSet[period <= 2010], esDemand, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("Historical energy service demand input data is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("Unnecessary data is provided")
      } else if (length(unique(check$unit)) > 2) {
        stop("Something went wrong in generating historical energy service demand input data.
             Data does not have the same unit.")
      } else if (length(unique(check$variable)) > 1) {
        stop("Something went wrong in generating historical energy service demand input data. Data does not have
             the same variable type.")
      } else if (anyNA(esDemand) == TRUE) {
        stop("Historical energy service demand data includes NAs")
      }

      quitteobj <- esDemand
    },
    "loadFactor" = {
      unit <- "(t|p)/veh"
      description <- "Load factor on technology level that states the tons/number of passengers in a vehicle.
                      Sources: TRACCS, GCAM"
      weight <- calcOutput("GDP", aggregate = FALSE)[, years, paste0("gdp_", SSPscen)]

      # read different source data
      LFTRACCS <- toolPrepareTRACCS(readSource("TRACCS", subtype), subtype)
      LFGCAM <- toolPrepareGCAM(readSource("GCAM", subtype), subtype)
      # Inter- and extrapolate all data to model input data years
      data <- list(LFTRACCS = LFTRACCS, LFGCAM = LFGCAM)
      data <- lapply(data, approx_dt, years, "period", "value",
                     c("region", "univocalName", "technology",
                       "variable", "unit"), extrapolate = TRUE)

      # merge.data.table data
      # EUROSTAT>TRACCS>GCAM
      #- EU data is used from TRACCS, rest is filled with GCAM
      countriesTRACCS <- unique(data$LFTRACCS$region)
      loadFactorRaw <- rbind(data$LFTRACCS, data$LFGCAM[!(region %in% countriesTRACCS & univocalName %in% c(filterEntries$trn_pass_road, filterEntries$trn_freight_road))])
      loadFactor <- toolAdjustLoadFactor(loadFactorRaw, completeDataSet, countriesTRACCS, filterEntries)

      # Add load factor of zero for active modes
      activeModes <- completeDataSet[univocalName %in% c("Cycle", "Walk")]
      activeModes[, unit := "p/veh"][, variable := "Load factor"][, value := 0][, check := NULL]
      loadFactor <- rbind(loadFactor, activeModes)

      loadFactor <- loadFactor[, c("region", "period", "univocalName", "technology",
                                  "variable", "unit", "value")]
      setkey(loadFactor, region, period, univocalName, technology,
             variable, unit)

      # Check whether data is complete
      check <- merge.data.table(completeDataSet, loadFactor, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("Load factor input data is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("Unnecessary data is provided")
      } else if (length(unique(check$unit)) > 2) {
        stop("Something went wrong in generating load factor input data. Data  unit.")
      } else if (length(unique(check$variable)) > 1) {
        stop("Something went wrong in generating load factor input data. Data does not have the same variable type.")
      } else if (anyNA(loadFactor) == TRUE) {
        stop("Load factor data includes NAs")
      }
      quitteobj <- loadFactor
    },
    "CAPEXtrackedFleet" = {
      unit <- "US$2005/veh"
      description <- "CAPEX for vehicle types that feature fleet tracking (cars, trucks and busses).
                      Sources: UCD, PSI"
      weight <- calcOutput("GDP", aggregate = FALSE)[, years, paste0("gdp_", SSPscen)]

      # read PSI CAPEX
      CAPEXPSI <- toolPreparePSI(readSource("PSI", "CAPEX"))
      # read UCD CAPEX given in US$2005/vkt and US$2005/veh
      CAPEXUCD <- toolPrepareUCD(readSource("UCD", "CAPEX"), "CAPEX")
      # For some modes UCD offers only a combined value for CAPEX and non-fuel OPEX given in US$2005/vehkm
      CAPEXcombinedUCD <- toolPrepareUCD(readSource("UCD", "CAPEXandNonFuelOPEX"), "CAPEXandNonFuelOPEX")

      # Inter- and extrapolate all data to model input data years
      data <- list(CAPEXPSI = CAPEXPSI, CAPEXUCD = CAPEXUCD, CAPEXcombinedUCD = CAPEXcombinedUCD)
      data <- lapply(data, approx_dt, years, "period", "value",
                     c("region", "univocalName", "technology",
                       "variable", "unit"), extrapolate = TRUE)

      # Use only for cars (trucks and busses are given combined with non fuel OPEX)
      CAPEXUCD4W <- data$CAPEXUCD[univocalName %in% filterEntries$trn_pass_road_LDV_4W]

      # CAPEX for Busses and Trucks given combined with non-fuel OPEX in the UCD data
      CAPEXcombinedUCD <- data$CAPEXcombinedUCD
      CAPEXcombinedUCD <- CAPEXcombinedUCD[univocalName %in% filterEntries$trn_freight_road | univocalName == "Bus"]

      # merge.data.table data
      # PSI vehicle purchase costs are used for LDV 4 Wheelers in EUR
      PSIcarsEUR <- data$CAPEXPSI[region %in% ISOcountriesMap[regionCode12 == "EUR"]$region]
      # PSI CAPEX for 4 Wheelers feature only purchase costs - take other capital costs from UCD for EUR regions
      CAPEXraw <- rbind(PSIcarsEUR, CAPEXUCD4W[!(region %in% ISOcountriesMap[regionCode12 == "EUR"]$region) |
                                                 (region %in% ISOcountriesMap[regionCode12 == "EUR"]$region & !variable == "Capital costs (purchase)")],
                        CAPEXcombinedUCD)

      GDPpcMERmag <- calcOutput("GDPpc", aggregate = FALSE,
                                unit = "constant 2005 US$MER")[, years, paste0("gdppc_", SSPscen)]
      GDPpcMER <- magpie2dt(GDPpcMERmag, yearcol = "period", regioncol = "region", valcol = "gdppc")[, variable := NULL]
      CAPEX <- toolAdjustCAPEXtrackedFleet(CAPEXraw, ISOcountriesMap, years, completeDataSet, GDPpcMER, filterEntries)

      CAPEX <-  CAPEX[, c("region", "period", "univocalName", "technology",
                          "variable", "unit", "value")]
      setkey(CAPEX, region, period, univocalName, technology,
             variable, unit)

      # CAPEXtrackedFleet data only includes CAPEX data for LDV 4 Wheelers, Trucks and Busses
      completeDataCAPFleet <- completeDataSet[univocalName %in% filterEntries$trn_freight_road | univocalName %in% filterEntries$trn_pass_road_LDV_4W |
                                         univocalName == "Bus"]

      # Check whether data is complete
      check <- merge.data.table(completeDataCAPFleet, CAPEX, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("CAPEX input data for vehicle types that feature fleet tracking is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("CAPEX input data for vehicle types that feature fleet tracking includes unnecessary data")
      } else if (length(unique(check$unit)) > 1) {
        stop("Something went wrong in generating CAPEX input data for the tracked fleet.
             Data does not have the same unit.")
      } else if (anyNA(CAPEX) == TRUE) {
        stop("CAPEX data for the tracked fleet includes NAs")
      }

      quitteobj <- CAPEX
    },
    "nonFuelOPEXtrackedFleet" = {
      unit <- "US$2005/veh/yr"
      description <- "Non-fuel OPEX on technology level for vehicle types that feature fleet tracking
                     (cars, trucks, busses). Sources: UCD, PSI"
      weight <- calcOutput("GDP", aggregate = FALSE)[, years, paste0("gdp_", SSPscen)]

      nonFuelOPEXUCD <- toolPrepareUCD(readSource("UCD", "nonFuelOPEX"), "nonFuelOPEX")
      nonFuelOPEXUCD <- nonFuelOPEXUCD[univocalName %in% filterEntries$trn_pass_road_LDV_4W]
      # For trucks and busses UCD offers only a combined value for CAPEX and non-fuel OPEX given in US$2005/vehkm
      nonFuelOPEXcombinedUCD <- toolPrepareUCD(readSource("UCD", "CAPEXandNonFuelOPEX"), "CAPEXandNonFuelOPEX")
      nonFuelOPEXcombinedUCD <- nonFuelOPEXcombinedUCD[univocalName %in% filterEntries$trn_freight_road | univocalName == "Bus"]

      # Inter- and extrapolate all data to model input data years
      data <- list(nonFuelOPEXUCD = nonFuelOPEXUCD, nonFuelOPEXcombinedUCD = nonFuelOPEXcombinedUCD)
      data <- lapply(data, approx_dt, years, "period", "value",
        c("region", "univocalName", "technology",
           "variable", "unit"), extrapolate = TRUE)

      nonFuelOPEXraw <- rbind(data$nonFuelOPEXUCD, data$nonFuelOPEXcombinedUCD)
      nonFuelOPEX <- toolAdjustNonFuelOPEXtrackedFleet(nonFuelOPEXraw, years, completeDataSet, filterEntries)

      # nonFuelOPEXtrackedFleet data only includes data for LDV 4 Wheelers, Trucks and Busses
      completeDataOPFleet <- completeDataSet[univocalName %in% filterEntries$trn_freight_road | univocalName %in% filterEntries$trn_pass_road_LDV_4W |
                                         univocalName == "Bus"]

      nonFuelOPEX <- nonFuelOPEX[, c("region", "period", "univocalName", "technology",
                                     "variable", "unit", "value")]
      setkey(nonFuelOPEX, region, period, univocalName, technology,
             variable, unit)

      # Check whether data is complete
      check <- merge.data.table(completeDataOPFleet, nonFuelOPEX, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("Non fuel OPEX input data for vehicle types that feature fleet tracking is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("Non fuel OPEX input data for vehicle types that feature fleet tracking includes unnecessary data")
      } else if (length(unique(check$unit)) > 1) {
        stop("Something went wrong in generating non fuel OPEX input data for the tracked fleet.
             Data does not have the same unit.")
      } else if (length(unique(check$variable)) > 1) {
        stop("Something went wrong in generating non fuel OPEX input data for the tracked fleet.
             Data does not have the same variable type.")
      } else if (anyNA(nonFuelOPEX) == TRUE) {
        stop("Non fuel OPEX data for the tracked fleet includes NAs")
      }

      quitteobj <- nonFuelOPEX
    },
    "CAPEXother" = {
      unit <- "US$2005/vehkm"
      description <- "CAPEX (purchase costs) for vehicle types that do not feature fleet tracking
                      (all other than cars, trucks and busses). Sources: UCD"
      weight <- calcOutput("GDP", aggregate = FALSE)[, years, paste0("gdp_", SSPscen)]

      # read UCD CAPEX given in US$2005/vkt and US$2005/veh
      CAPEXUCD <- toolPrepareUCD(readSource("UCD", "CAPEX"), "CAPEX")
      # For some modes UCD offers only a combined value for CAPEX and non-fuel OPEX given in US$2005/vehkm
      CAPEXcombinedUCD <- toolPrepareUCD(readSource("UCD", "CAPEXandNonFuelOPEX"), "CAPEXandNonFuelOPEX")

      # Inter- and extrapolate all data to model input data years
      data <- list(CAPEXUCD = CAPEXUCD, CAPEXcombinedUCD = CAPEXcombinedUCD)
      data <- lapply(data, approx_dt, years, "period", "value",
        c("region", "univocalName", "technology",
           "variable", "unit"), extrapolate = TRUE)

      # Includes aviation and two wheelers (used for all vehicle types other than 4 wheelers)
      CAPEXUCD <- data$CAPEXUCD[!univocalName %in% filterEntries$trn_pass_road_LDV_4W]
      # Data for two wheelers is given in US$2005/veh and needs to be converted to US$2005/vehkm
      # with the help of annual mileage
      AMUCD2W <- toolPrepareUCD(readSource("UCD", "annualMileage"), "annualMileage")
      AMUCD2W <- AMUCD2W[univocalName %in% filterEntries$trn_pass_road_LDV_2W]
      AMUCD2W <- AMUCD2W[, c("region", "univocalName", "technology", "period", "value")]
      AMUCD2W <- approx_dt(AMUCD2W, years, "period", "value", c("region", "technology", "univocalName"),
                           extrapolate = TRUE)
      setnames(AMUCD2W, "value", "annualMileage")
      CAPEXUCD <- merge.data.table(CAPEXUCD, AMUCD2W, by = c("region", "univocalName", "technology", "period"),
                                   all.x = TRUE)
      CAPEXUCD[univocalName %in% filterEntries$trn_pass_road_LDV_2W, value := value / annualMileage]
      CAPEXUCD[univocalName %in% filterEntries$trn_pass_road_LDV_2W, unit := "US$2005/vehkm"][, annualMileage := NULL]

      # CAPEX given combined with non-fuel OPEX in the UCD data for shipping and rail (all other than busses
      # and trucks)
      CAPEXcombinedUCD <- data$CAPEXcombinedUCD
      CAPEXcombinedUCD <- CAPEXcombinedUCD[!(univocalName %in% filterEntries$trn_freight_road | univocalName == "Bus")]

      # merge.data.table data
      CAPEXraw <- rbind(CAPEXUCD, CAPEXcombinedUCD)

      GDPpcMERmag <- calcOutput("GDPpc", aggregate = FALSE,
                                unit = "constant 2005 US$MER")[, years, paste0("gdppc_", SSPscen)]
      GDPpcMER <- magpie2dt(GDPpcMERmag, yearcol = "period", regioncol = "region", valcol = "gdppc")[, variable := NULL]
      CAPEX <- toolAdjustCAPEXother(CAPEXraw, ISOcountriesMap, years, completeDataSet, GDPpcMER, filterEntries)

      CAPEX <- CAPEX[, c("region", "period", "univocalName", "technology", "variable", "unit", "value")]
      setkey(CAPEX,  region, period, univocalName, technology, variable, unit)

      # CAPEXother only includes data for all other modes than LDV 4 Wheelers, Trucks and Busses
      # (except cycling and walking)
      completeDataCAP <- completeDataSet[!(univocalName %in% c(filterEntries$trn_freight_road, "Cycle", "Walk") |
                                             univocalName %in% filterEntries$trn_pass_road_LDV_4W | univocalName == "Bus")]

      # Check whether data is complete
      check <- merge.data.table(completeDataCAP, CAPEX, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("CAPEX input data for vehicle types that do not feature fleet tracking is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("CAPEX input data for vehicle types that do not feature fleet tracking includes unnecessary data")
      } else if (length(unique(check$unit)) > 1) {
        stop("Something went wrong in generating CAPEX input data for vehicle types that do not feature fleet tracking.
             Data does not have the same unit.")
      } else if (length(unique(check$variable)) > 1) {
        stop("Something went wrong in generating CAPEX input data for vehicle types that do not feature fleet tracking.
             Data does not have the same variable type.")
      } else if (anyNA(CAPEX) == TRUE) {
        stop("CAPEX data for vehicle types that do not feature fleet tracking includes NAs")
      }

      quitteobj <- CAPEX
    },
    "nonFuelOPEXother" = {
      unit <- "US$2005/vehkm"
      description <- "Non fuel OPEX on technology level for vehicle types that do not feature fleet tracking
                     (other than cars, trucks, busses). Sources: UCD, PSI"
      weight <- calcOutput("GDP", aggregate = FALSE)[, years, paste0("gdp_", SSPscen)]

      nonFuelOPEXUCD <- toolPrepareUCD(readSource("UCD", "nonFuelOPEX"), "nonFuelOPEX")
      nonFuelOPEXUCD <- nonFuelOPEXUCD[!univocalName %in% filterEntries$trn_pass_road_LDV_4W]
      # For some modes UCD offers only a combined value for CAPEX and non-fuel OPEX given in US$2005/vehkm
      nonFuelOPEXcombinedUCD <- toolPrepareUCD(readSource("UCD", "CAPEXandNonFuelOPEX"), "CAPEXandNonFuelOPEX")
      nonFuelOPEXcombinedUCD <- nonFuelOPEXcombinedUCD[!univocalName %in% c(filterEntries$trn_freight_road, filterEntries$trn_pass_road_LDV_4W, "Bus")]

      # Inter- and extrapolate all data to model input data years
      data <- list(nonFuelOPEXUCD = nonFuelOPEXUCD, nonFuelOPEXcombinedUCD = nonFuelOPEXcombinedUCD)
      data <- lapply(data, approx_dt, years, "period", "value",
        c("region", "univocalName", "technology", "variable", "unit"), extrapolate = TRUE)

      # Includes aviation and two wheelers (used for all vehicle types other than 4 wheelers)
      nonFuelOPEXUCD <- data$nonFuelOPEXUCD[!univocalName %in% filterEntries$trn_pass_road_LDV_4W]
      # Data for two wheelers is given in US$2005/veh and needs to be converted to US$2005/vehkm
      # with the help of annual mileage
      AMUCD2W <- toolPrepareUCD(readSource("UCD", "annualMileage"), "annualMileage")
      AMUCD2W <- AMUCD2W[univocalName %in% filterEntries$trn_pass_road_LDV_2W]
      AMUCD2W <- AMUCD2W[, c("region", "univocalName", "technology", "period", "value")]
      AMUCD2W <- approx_dt(AMUCD2W, years, "period", "value", c("region", "univocalName", "technology"),
                           extrapolate = TRUE)
      setnames(AMUCD2W, "value", "annualMileage")
      nonFuelOPEXUCD <- merge.data.table(nonFuelOPEXUCD, AMUCD2W, by = c("region", "period", "univocalName", "technology"), all.x = TRUE)
      nonFuelOPEXUCD[univocalName %in% filterEntries$trn_pass_road_LDV_2W, value := value / annualMileage]
      nonFuelOPEXUCD[univocalName %in% filterEntries$trn_pass_road_LDV_2W, unit := "US$2005/vehkm"][, annualMileage := NULL]

      # CAPEX given combined with non-fuel OPEX in the UCD data for shipping and rail
      # (all other than busses and trucks)
      nonFuelOPEXcombinedUCD <- data$nonFuelOPEXcombinedUCD
      nonFuelOPEXcombinedUCD <- nonFuelOPEXcombinedUCD[!(univocalName %in% filterEntries$trn_freight_road | univocalName == "Bus")]

      # merge.data.table data
      nonFuelOPEXraw <- rbind(nonFuelOPEXUCD, nonFuelOPEXcombinedUCD)

      nonFuelOPEX <- toolAdjustNonFuelOPEXother(nonFuelOPEXraw, ISOcountriesMap, years, completeDataSet, filterEntries)

      nonFuelOPEX <- nonFuelOPEX[, c("region", "period", "univocalName", "technology", "variable", "unit", "value")]
      setkey(nonFuelOPEX,  region, period, univocalName, technology, variable, unit)

      # nonFuelOPEXother only includes data for all other modes than LDV 4 Wheelers, Trucks and Busses
      # (except cycling and walking)
      completeDataOP <- completeDataSet[!(univocalName %in% c(filterEntries$trn_freight_road, "Cycle", "Walk") |
                                             univocalName %in% filterEntries$trn_pass_road_LDV_4W | univocalName == "Bus")]
      # Check whether data is complete
      check <- merge.data.table(completeDataOP, nonFuelOPEX, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("Non fuel OPEX input data for vehicle types that do not feature fleet tracking is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("Non fuel OPEX input data for vehicle types that do not feature fleet tracking includes unnecessary data")
      } else if (length(unique(check$unit)) > 1) {
        stop("Something went wrong in generating non fuel OPEX input data for vehicle types
             that do not feature fleet tracking. Data does not have the same unit.")
      } else if (length(unique(check$variable)) > 1) {
        stop("Something went wrong in generating non fuel OPEX input data for vehicle types
             that do not feature fleet tracking. Data does not have the same variable type.")
      } else if (anyNA(nonFuelOPEX) == TRUE) {
        stop("Non fuel OPEX data for vehicle types that do not feature fleet tracking includes NAs")
      }

      quitteobj <- nonFuelOPEX
    },
    "speedOfModes" = {
      unit <- "km/h"
      description <- "Speed of traveling on vehicle type level (same for all technologies). Changes over time for
                      the motorized modes. Used to calculate the time value costs for passenger transport modes.
                      Sources: GCAM"
      weight <- calcOutput("GDP", aggregate = FALSE)[, years, paste0("gdp_", SSPscen)]

      # read sources
      speedGCAM <- toolPrepareGCAM(readSource("GCAM", "speedMotorized"), "speedMotorized")
      speedNonMotGCAM <- toolPrepareGCAM(readSource("GCAM", "speedNonMotorized"), "speedNonMotorized")

      # Inter- and extrapolate all data to model input data years
      allYears <- data.table(period = years)
      allYears[, all := "All"]
      speedNonMotGCAM[, all := "All"]
      speedNonMotGCAM <- merge.data.table(speedNonMotGCAM, allYears, by = "all", allow.cartesian = TRUE)
      speedNonMotGCAM[, all := NULL]
      speedGCAM <- approx_dt(speedGCAM, years, "period", "value",
        c("region", "univocalName", "variable", "unit"), extrapolate = TRUE)
      # merge.data.table data
      # speed of modes is only featured for passenger small-to-medium distance transport (to calculate time value costs).
      speedOfModesRaw <- rbind(speedGCAM[univocalName %in% filterEntries$trn_pass], speedNonMotGCAM)
      speedOfModes <- toolAdjustSpeedOfModes(speedOfModesRaw, completeDataSet, filterEntries)

      speedOfModes <- speedOfModes[, c("region", "period", "univocalName", "technology",
                                      "variable", "unit", "value")]
      setkey(speedOfModes, region, period, univocalName, technology,
             variable, unit)

      # Check whether data is complete
      completeDataSpeed <- completeDataSet[univocalName %in% filterEntries$trn_pass]
      check <- merge.data.table(completeDataSpeed, speedOfModes, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("Speed of modes input data is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("Unnecessary data is provided for speed of modes")
      } else if (length(unique(check$unit)) > 1) {
        stop("Something went wrong in generating speed input data. Data does not have the same unit.")
      } else if (length(unique(check$variable)) > 1) {
        stop("Something went wrong in generating speed input data. Data does not have the same variable type.")
      } else if (anyNA(speedOfModes) == TRUE) {
        stop("Speed of modes data includes NAs")
      }
      quitteobj <- speedOfModes
    },
    "valueOfTimeMultiplier" = {
      unit <- "-"
      description <- "Cost associated with travel expressed as a multiplier of the wage rate.
                      Data is provided for different passenger transport modes and is equal for all regions and years.
                      Used to calculate the time value costs for passenger
                      transport modes. Sources: GCAM"
      weight <- calcOutput("GDP", aggregate = FALSE)[, years, paste0("gdp_", SSPscen)]

      # read sources
      VOTGCAM <- toolPrepareGCAM(readSource("GCAM", "valueOfTimeMultiplier"), "valueOfTimeMultiplier")

      # Inter- and extrapolate all data to model input data years
      allYears <- data.table(period = years)
      allYears[, all := "All"]
      VOTGCAM[, all := "All"]
      VOTGCAM <- merge.data.table(VOTGCAM, allYears, by = "all", allow.cartesian = TRUE)[, all := NULL]

      VOT <- toolAdjustValueOfTimeMultiplier(VOTGCAM, completeDataSet, filterEntries)

      VOT <- VOT[, c("region", "period", "univocalName", "technology", "variable", "unit", "value")]
      setkey(VOT, region, period, univocalName, technology, variable, unit)

      # Check whether data is complete
      # speed of modes is only featured for passenger transport (to calculate value of time)
      completeDataVOT <- completeDataSet[univocalName %in% filterEntries$trn_pass]
      check <- merge.data.table(completeDataVOT, VOT, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("Value of time multiplier input data is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("Unnecessary data is provided for value of time multiplier")
      } else if (length(unique(check$unit)) > 1) {
        stop("Something went wrong in generating value of time multiplier input data.
             Data does not have the same unit.")
      } else if (length(unique(check$variable)) > 1) {
        stop("Something went wrong in generating value of time multiplier input data.
             Data does not have the same variable type.")
      } else if (anyNA(VOT) == TRUE) {
        stop("Value of time multiplier data includes NAs")
      }
      quitteobj <- VOT
    },
    "timeValueCosts" = {
      unit <- "US$2005/pkm"
      description <- "Time value costs for passenger transport modes.
                      Sources: GCAM"
      weight <- calcOutput("GDP", aggregate = FALSE)[, years, paste0("gdp_", SSPscen)]

      # Speed of modes [km/h]
      speedOfModesMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = FALSE, warnNA = FALSE, subtype = "speedOfModes")
      speedOfModes <- magpie2dt(speedOfModesMagpieobj, valcol = "speed")[, c("variable", "unit") := NULL]
      setkey(speedOfModes, region, period, univocalName, technology)
      # Value of Time multiplier [-]
      valueOfTimeMultiplierMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = FALSE, warnNA = FALSE, subtype = "valueOfTimeMultiplier")
      valueOfTimeMultiplier <- magpie2dt(valueOfTimeMultiplierMagpieobj, valcol = "multiplier")[, c("variable", "unit") := NULL]
      setkey(valueOfTimeMultiplier, region, period, univocalName, technology)
      GDPpcMERmag <- calcOutput("GDPpc", aggregate = FALSE,
                                unit = "constant 2005 US$MER")[, years, paste0("gdppc_", SSPscen)]
      GDPpcMER <- magpie2dt(GDPpcMERmag, yearcol = "period", regioncol = "region", valcol = "gdppc")[, variable := NULL]
      setkey(GDPpcMER, region, period)

      timeValueCosts <- merge(speedOfModes,  valueOfTimeMultiplier)
      timeValueCosts <- merge(timeValueCosts, GDPpcMER)
      weeksPerYear = 50
      hoursPerWeek = 40
      timeValueCosts[, value := gdppc              ## [US$2005/person/year]
               * multiplier                        ## [US$2005/person/year]
               /(hoursPerWeek * weeksPerYear)/     ## [US$2005/h]
                 speed]                            ## [US$2005/pkm]
      timeValueCosts[, variable := "Time value costs"][, unit := "US$2005/pkm"]
      timeValueCosts <- timeValueCosts[, c("region", "univocalName", "technology", "variable", "unit", "period", "value")]
      setkey(timeValueCosts, region, period, univocalName, technology, variable, unit)

      # Check whether data is complete
      # speed of modes is only featured for passenger transport (to calculate value of time)
      completeDataTVC <- completeDataSet[univocalName %in% filterEntries$trn_pass]
      check <- merge.data.table(completeDataTVC, timeValueCosts, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("Time value costs input data is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("Unnecessary data is provided for Time value costs")
      } else if (length(unique(check$unit)) > 1) {
        stop("Something went wrong in generating Time value costs input data.
             Data does not have the same unit.")
      } else if (length(unique(check$variable)) > 1) {
        stop("Something went wrong in generating Time value costs input data.
             Data does not have the same variable type.")
      } else if (anyNA(timeValueCosts) == TRUE) {
        stop("Time value costs data includes NAs")
      }
      quitteobj <- timeValueCosts
    }
  )

  x <- as.magpie(as.data.frame(quitteobj))

  return(list(
    x           = x,
    weight      = weight,
    unit        = unit,
    description = description,
    aggregationFunction = "toolAggregateVehicleTypes"
  ))
}
