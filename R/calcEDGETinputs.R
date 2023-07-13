#' Provide EDGE-Transport input parameters
#' @author Johanna Hoppe
#' @param subtype one of the parameters required for EDGE-T SA
#' @importFrom rmndt approx_dt
#' @import madrat
#' @import mrremind

calcEdgeTransportSAinputs <- function(subtype, adjustments = TRUE) {
  years <- data.table(temporal = "all", period = c(1990,
             seq(2005, 2060, by = 5),
             seq(2070, 2110, by = 10),
             2130, 2150))
  #decisionTree.csv contains all possible branches of the decision tree
  decisionTree <- fread(system.file("extdata/decisionTree.csv", package = "edgeTransport", mustWork = TRUE))[, temporal := "all"][, spatial := "all"]
  #Not all countries feature the same branches of the decision tree - Some vehicleTypes and modes are not available in certain countries
  #Here we create the full structure of the nested decision tree differentiated for all countries to make it testable
  ISOcountriesMap <- system.file("extdata", "regionmapping21EU11.csv",
    package = "mredgetransport", mustWork = TRUE)
  ISOcountriesMap <- fread(ISOcountriesMap, skip = 0)
  setnames(ISOcountriesMap, c("CountryCode"), c("region"))
  ISOcountries <- ISOcountriesMap[, c("region")][, spatial := "all"]

  completeDataSet <- merge(decisionTree, ISOcountries, by = "spatial", allow.cartesian = TRUE)[, spatial := NULL]
  MapCountrySpecificVehicleTypes <- fread(system.file("extdata/MapCountrySpecificVehicleTypes.csv", package = "edgeTransport", mustWork = TRUE))

  completeDataSet <- merge(completeDataSet, MapCountrySpecificVehicleTypes, by = c("region", "univocalName"), all = TRUE)
  completeDataSet <- completeDataSet[present == 1][, present := NULL]
  completeDataSet <- merge(completeDataSet, years, by = "temporal", allow.cartesian = TRUE)[, temporal := NULL]
  completeDataSet[, check := 1]
  years <- years[, c(period)]
  setkey(completeDataSet, region,  sector, subsectorL3, subsectorL2, subsectorL1, vehicleType, technology, period, univocalName)

  #Description of used sources for transport input data:
  #(1) TRACCS:
  #- Includes regionally differentiated data for EUR iso countries except for "ALA" (Aland Islands), "GGY" (Guernsey), "FRO" (Faroe Islands), "GIB" (Gibraltar)
  # "IMN" (Isle of Man) and "JEY" (Jersey) but additionally includes "CHE" (Switzerland)
  #- Includes Truck (0-3.5t), Truck (7.5t), Truck (18t), Truck (26t), Truck (40t), Bus, Moped, Motorcycle (50-250cc), Motorcycle (>250cc), Compact Car, Large Car and SUV, Midsize Car, Subcompact Car
  #- Only conventional technologies (liquids + NG)
  #- For some reason energy service demand for Truck(0-3.5t)/Light commercial vehicles is not reported by TRACCS
  #(2) GCAM:
  #- Includes regionally differentiated data for all iso countries
  #- With few excepetions (e.g. hydrogen aircrafts) all modes and technologies are available
  #(3) PSI:
  #- PSI data has no regional differentation
  #- Conventional and alternative technologies are available
  #- PSI data includes Truck (0-3.5t), Truck (7.5t), Truck (18t), Truck (26t), Truck (40t), Mini Car, Midsize Car, Compact Car, Subcompact Car, Large Car and SUV, Van
  #(4) EUROSTAT:
  #- Final energy demand for bunkers in EU27 is read in

  switch(
    subtype,
    "energyIntensity" ={
      unit <- "MJ/vehkm"
      description <- "Energy intensity on technology level. Sources: TRACCS, PSI, UCD, GCAM"
      weight <- calcOutput("GDP", aggregate = FALSE)[, years, "gdp_SSP2"]

      #calc different source data
      enIntGCAM <- toolPrepareGCAM(readSource("GCAM", subtype), subtype)
      enIntUCD <- toolPrepareUCD(readSource("UCD", subtype), subtype)
      enIntTRACCS <- toolPrepareTRACCS(readSource("TRACCS", subtype), subtype)
      enIntPSI <- toolPreparePSI(readSource("PSI", subtype), subtype)

      #Inter- and extrapolate all data to model input data years
      data <- list(enIntGCAM = enIntGCAM, enIntUCD = enIntUCD, enIntTRACCS = enIntTRACCS, enIntPSI = enIntPSI)
      data <- lapply(data, approx_dt, years, "period", "value",
               c("region", "unit", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName"), extrapolate = TRUE)

      #Merge data
      #TRACCS>PSI>GCAM
      #1: TRACCS data
      #- Used parts of TRACCS energy Intensity: TRACCS data is used completely
      countriesTRACCS <- unique(data$enIntTRACCS$region)

      #2: GCAM data
      #- Used parts of the GCAM energy Intensity:
      #- Conventional cars (Liquids, NG) in non-TRACCS countries
      energyIntensityRawGCAMconventionalCarsnonTRACCS <- data$enIntGCAM[subsectorL1 == "trn_pass_road_LDV_4W" & technology %in% c("Liquids", "NG") & !region %in% countriesTRACCS]
      #- All other data for non-TRACCS countries except for Trucks
      energyIntensityRawGCAMnonCarsnonTRACCS <- data$enIntGCAM[!subsectorL1 == "trn_pass_road_LDV_4W" & !subsectorL3 == "trn_freight_road" & !region %in% countriesTRACCS]
      #- Energy Intensity data for Freight Rail, Passenger Rail, HSR, Domestic Aviation, International Aviation, Domestic Shipping, International Shipping is not provided by TRACCS. Hence GCAM data is used for all countries
      energyIntensityRawGCAMmissingTRACCScat <- data$enIntGCAM[univocalName %in% c("Freight Rail", "Passenger Rail", "HSR", "Domestic Aviation", "International Aviation", "Domestic Ship", "International Ship")
                                                               & region %in% countriesTRACCS]
      #- Alternative technologies for motorcycles are missing in the TRACCS database and are taken from GCAM also for TRACCS countries
      energyIntensityRawGCAMalt2WheelersTRACCSreg <- data$enIntGCAM[subsectorL1 == "trn_pass_road_LDV_2W" & region %in% countriesTRACCS & technology == "BEV"]

      #3: PSI data
      #- Used for Trucks in non-TRACCS countries
      energyIntensityRawPSITrucks <- data$enIntPSI[vehicleType %in% c("Truck (0-3.5t)", "Truck (7.5t)", "Truck (18t)", "Truck (26t)", "Truck (40t)") & !region %in% countriesTRACCS]
      #- TRACCS data does not include NG Truck (7.5t), Truck (18t), Truck (26t), Truck (40t) -> data is taken from PSI
      energyIntensityRawPSItrucksNGTRACCSreg <- data$enIntPSI[vehicleType %in% c("Truck (7.5t)", "Truck (18t)", "Truck (26t)", "Truck (40t)") & technology == "NG" & region %in% countriesTRACCS]
      #- Used for alternative Cars (BEV,FCEV,HEV) in TRACCS countries
      energyIntensityRawPSIalternativeTechTRACCSreg <- data$enIntPSI[technology %in% c("BEV", "FCEV", "Hybrid Electric") & region %in% countriesTRACCS]
      #Use only data for vehicle types that are listed in the TRACCS data base
      TRACCSVehTypes <- copy(data$enIntTRACCS)
      TRACCSVehTypes <- unique(TRACCSVehTypes[, c("value", "technology") := NULL])
      #Apply only on the vehicle types that are in general available from the PSI dataset (2 Wheelers and Busses are not provided by PSI)
      TRACCSVehTypes <- TRACCSVehTypes[vehicleType %in% unique(energyIntensityRawPSIalternativeTechTRACCSreg$vehicleType)]
      energyIntensityRawPSIalternativeTechTRACCSreg <- merge(energyIntensityRawPSIalternativeTechTRACCSreg, TRACCSVehTypes, by = c("region", "period", "unit", "sector", "subsectorL3", "subsectorL2", "subsectorL1",
                                                                                                                                   "vehicleType", "univocalName"), all.y = TRUE)
      #- Used for alternative Cars (BEV,FCEV,HEV) in non-TRACCS countries
      #For non TRACCS iso countries the available vehicle types differ. Use the additional data on alternative Cars only for the existing vehicle types in GCAM
      energyIntensityRawPSIalternativeCarsnonTRACCS <- data$enIntPSI[technology %in% c("BEV", "FCEV", "Hybrid Electric") & !region %in% countriesTRACCS]
      #Create structure for GCAM vehicle types and alternative tech options
      GCAMVehTypes <- energyIntensityRawGCAMconventionalCarsnonTRACCS[subsectorL1 == "trn_pass_road_LDV_4W" & !region %in% countriesTRACCS]
      GCAMVehTypes <- unique(GCAMVehTypes[, c("value", "technology") := NULL])[, altTech := 1]
      AltTechOpt <- data.table(technology = c("BEV", "FCEV", "Hybrid Electric"), altTech = c(1, 1, 1))
      GCAMVehTypes <- merge(GCAMVehTypes, AltTechOpt, by = "altTech", allow.cartesian = TRUE)[, altTech := NULL]
      energyIntensityRawPSIalternativeCarsnonTRACCS <- merge(energyIntensityRawPSIalternativeCarsnonTRACCS, GCAMVehTypes, by = c("region", "period", "unit", "sector", "subsectorL3", "subsectorL2", "subsectorL1",
                                                                                                                           "vehicleType", "technology", "univocalName"), all.y = TRUE)

      energyIntensityRaw <- rbind(data$enIntTRACCS, energyIntensityRawGCAMconventionalCarsnonTRACCS,  energyIntensityRawGCAMmissingTRACCScat, energyIntensityRawGCAMnonCarsnonTRACCS, energyIntensityRawGCAMalt2WheelersTRACCSreg, energyIntensityRawPSITrucks, energyIntensityRawPSItrucksNGTRACCSreg, energyIntensityRawPSIalternativeTechTRACCSreg,
                                  energyIntensityRawPSIalternativeCarsnonTRACCS)

      setkey(energyIntensityRaw, region,  sector, subsectorL3, subsectorL2, subsectorL1, vehicleType, technology, period, univocalName)

      #Include data adjustments: fill gaps and correct data if necessary based on projects, other sources and own assumptions
      energyIntensity <- toolAdjustEnergyIntensity(energyIntensityRaw, countriesTRACCS, data$enIntPSI)

      #Add energy intensity of zero for active modes
      activeModes <- completeDataSet[univocalName %in% c("Cycle", "Walk")]
      activeModes[, unit := "MJ/vehkm"][, value := 0][, check := NULL]
      energyIntensity <- rbind(energyIntensity, activeModes)

      #Check whether data is complete
      check <- merge(completeDataSet, energyIntensity, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("Energy intensity input data is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("Unnecessary data is provided")
      }

      quitteobj <- energyIntensity
    },

    "annualMileage" = {
      unit <- "vehkm/veh/yr"
      description <- "Annual mileage on technology level. Sources: TRACCS, UCD"
      weight <- calcOutput("GDP", aggregate = FALSE)[,  years, "gdp_SSP2"]

      #calc different source data
      AMTRACCS <- toolPrepareTRACCS(readSource("TRACCS", subtype), subtype)
      AMUCD <- toolPrepareUCD(readSource("UCD", subtype), subtype)

      #Inter- and extrapolate all data to model input data years
      data <- list(AMTRACCS = AMTRACCS, AMUCD = AMUCD)
      data <- lapply(data, approx_dt, years, "period", "value",
                     c("region", "unit", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName"), extrapolate = TRUE)

      #Merge data
      #TRACCS>UCD
      #- EU data is used from TRACCS, rest is filled with UCD
      annualMileageRaw <- rbind(data$AMTRACCS, data$AMUCD[!(region %in% unique(data$AMTRACCS$region))])

      setkey(annualMileageRaw, region,  sector, subsectorL3, subsectorL2, subsectorL1, vehicleType, technology, period, univocalName)

      annualMileage <- toolAdjustAnnualMileage(annualMileageRaw, completeDataSet)

      #Add annual mileage of zero for active modes
      activeModes <- completeDataSet[univocalName %in% c("Cycle", "Walk")]
      activeModes[, unit := "MJ/vehkm"][, value := 0][, check := NULL]
      annualMileage <- rbind(annualMileage, activeModes)

      #Check whether data is complete
      check <- merge(completeDataSet, annualMileage, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("Annual mileage input data is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("Unnecessary data is provided")
      }

      quitteobj <- annualMileage
    },

    "histEsDemand" = {
      unit <- "billion (p|t)km/yr"
      description <- "Energy service demand on technology level. Sources: GCAM, TRACCS, EUROSTAT"
      weight <- calcOutput("GDP", aggregate = FALSE)[, years, "gdp_SSP2"]

      #calc different source data
      esDemandGCAM <- toolPrepareGCAM(readSource("GCAM", subtype), subtype)
      esDemandTRACCS <- toolPrepareTRACCS(readSource("TRACCS", subtype), subtype)
      feDemandEUROSTAT <- toolPrepareEUROSTAT(readSource("EUROSTAT", "feDemand"), "feDemand")
      enIntensity <- magpie2dt(calcOutput(type = "EdgeTransportSAinputs", subtype = "energyIntensity", warnNA = FALSE, aggregate = FALSE))[, unit := NULL]
      loadFactor <- magpie2dt(calcOutput(type = "EdgeTransportSAinputs", subtype = "loadFactor", warnNA = FALSE, aggregate = FALSE))[, unit := NULL]

      #Inter- and extrapolate all data to model input data years
      data <- list(esDemandGCAM = esDemandGCAM, esDemandTRACCS = esDemandTRACCS, feDemandEUROSTAT = feDemandEUROSTAT)
      #The historical energy service demand is only used for years <= 2010, future years will be calculated by demand regression in the model
      data <- lapply(data, approx_dt, years[years <= 2010], "period", "value",
                     c("region", "unit", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName"), extrapolate = TRUE)

      #Calc Energy Service demand based on FE data from EUROSTAT for bunkers
      setnames(enIntensity, "value", "enIntensity")
      setnames(loadFactor, "value", "loadFactor")
      esDemandEUROSTAT <- merge(data$feDemandEUROSTAT, enIntensity, by = c("region",  "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology",
                                                                           "period", "univocalName"))
      esDemandEUROSTAT <- merge(esDemandEUROSTAT, loadFactor, by = c("region",  "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology",
                                                                     "period", "univocalName"))
      esDemandEUROSTAT[, value := (value/enIntensity) * loadFactor][, c("enIntensity", "loadFactor") := NULL]
      esDemandEUROSTAT[sector %in% c("trn_pass", "trn_aviation_intl"), unit := "pkm/yr"]
      esDemandEUROSTAT[sector %in% c("trn_freight", "trn_shipping_intl"), unit := "tkm/yr"]

      #Merge data
      #TRACCS + EUROSTAT > GCAM
      #TRACCS data is used completely
      #EUROSTAT data is used completely
      #GCAM data is used for regions that are not included in TRACCS, bunkers for regions that are not included in EUROSTAT (non EU-27) and modes that are not included in TRACCS
      #CHE, GBR, ISL, MKD, NOR, TUR are included in TRACCS but not in EUROSTAT
      missingBunkers <- data$esDemandGCAM[region %in% c("CHE","GBR", "ISL", "MKD", "NOR", "TUR") & univocalName %in% c("International Aviation", "Domestic Aviation", "Domestic Ship", "International Ship")]
      #GCAM is used for modes not provided by TRACCS for TRACCS regions. 4 Wheelers must be excluded as GCAM uses different vehicle types and bunkers are used from EUROSTAT
      #For some reason energy service demand for Truck(0-3.5t)/Light commercial vehicles is not reported by TRACCS -> also taken from GCAM
      missingModes <- data$esDemandGCAM[region %in% unique(data$esDemandTRACCS$region) & !univocalName %in% unique(data$esDemandTRACCS$univocalName) & !univocalName %in% c("International Aviation", "Domestic Aviation", "Domestic Ship", "International Ship")
                                        & !subsectorL1 == "trn_pass_road_LDV_4W"]
      esDemandRaw <- rbind(data$esDemandTRACCS, esDemandEUROSTAT, data$esDemandGCAM[!(region %in% unique(data$esDemandTRACCS$region))],
                           missingBunkers, missingModes)

      esDemand <- toolAdjustEsDemand(esDemandRaw, ISOcountriesMap, completeDataSet)
      setkey(esDemand, region,  sector, subsectorL3, subsectorL2, subsectorL1, vehicleType, technology, period, univocalName)

      #Check whether data is complete
      check <- merge(completeDataSet[period <= 2010], esDemand, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("Historical energy service demand input data is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("Unnecessary data is provided")
      }

      quitteobj <- esDemand
    },

    "loadFactor" = {
     unit <- "(t|p)/veh"
     description <- "Load factor on technology level that states the tons/number of passengers in a vehicle. Sources: TRACCS, GCAM"
     weight <- calcOutput("GDP", aggregate = FALSE)[,  years, "gdp_SSP2"]

     #read different source data
     LFTRACCS <- toolPrepareTRACCS(readSource("TRACCS", subtype), subtype)
     LFGCAM <- toolPrepareGCAM(readSource("GCAM", subtype), subtype)

     #Inter- and extrapolate all data to model input data years
     data <- list(LFTRACCS = LFTRACCS, LFGCAM = LFGCAM)
     data <- lapply(data, approx_dt, years, "period", "value",
                    c("region", "unit", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName"), extrapolate = TRUE)

     #Merge data
     #EUROSTAT>TRACCS>GCAM
     #- EU data is used from TRACCS, rest is filled with GCAM
     countriesTRACCS <- unique(data$LFTRACCS$region)
     loadFactorRaw <- rbind(data$LFTRACCS, data$LFGCAM[!(region %in% countriesTRACCS)])
     loadFactor <- toolAdjustLoadFactor(loadFactorRaw, completeDataSet, countriesTRACCS)

     #Add load factor of zero for active modes
     activeModes <- completeDataSet[univocalName %in% c("Cycle", "Walk")]
     activeModes[, unit := "(t|p)/veh"][, value := 0][, check := NULL]
     loadFactor <- rbind(loadFactor, activeModes)

     #Check whether data is complete
     check <- merge(completeDataSet, loadFactor, all = TRUE)
     if (nrow(check[is.na(value)]) > 0) {
       stop("Load factor input data is incomplete")
     } else if (nrow(check[is.na(check)]) > 0) {
       stop("Unnecessary data is provided")
     }

     quitteobj <- loadFactor
    },

    "CAPEX" = {
      unit <- "US$2005/veh"
      description <- "CAPEX and non-fuel OPEX on technology level. Sources: UCD, PSI"
      weight <- calcOutput("GDP", aggregate = FALSE)[, years, "gdp_SSP2"]

      #read PSI CAPEX
      CAPEXPSI <- toolPreparePSI(readSource("PSI", subtype), subtype)

      #read UCD CAPEX given in 2005$/vkt and 2005$/veh
      CAPEXUCD <- toolPrepareUCD(readSource("UCD", subtype), subtype)
      #For some modes UCD offers only a combined value for CAPEX and non-fuel OPEX given in US$2005/vehkm
      CAPEXcombinedUCD <- toolPrepareUCD(readSource("UCD", "CAPEXandNonFuelOPEX"), "CAPEXandNonFuelOPEX")

      #Values given in US$2005/vehkm need to be transferred to US$2005/yr with the help of annual mileage and annuity factor
      annualMileageUCD <- toolPrepareUCD(readSource("UCD", "annualMileage"), "annualMileage")
      setnames(annualMileageUCD, "value", "annualMileage")
      annualMileageUCD[, unit:= NULL]

      #UCD applied interest rate of 10% and uniform vehicle lifetime of 15 yrs (https://itspubs.ucdavis.edu/publication_detail.php?id=1884)
      #Calc annuity factor
      discountRate = 0.1   #discount rate for vehicle purchases
      lifeTime = 15    #Number of years over which vehicle capital payments are amortized
      annuityFactor = (discountRate * (1 + discountRate)^lifeTime)/((1 + discountRate)^lifeTime - 1)

      #Divide by Annual Mileage to get [unit = US$2005/veh/yr]
      CAPEXUCD <- merge(CAPEXUCD, annualMileageUCD, all.x = TRUE)
      CAPEXUCD[unit == "2005$/vkt", value := value/annualMileage][, unit := "US$2005/veh/yr"]
      #Divide by annuity factor to get CAPEX per veh
      CAPEXUCD[unit == "US$2005/veh/yr", value := value/annuityFactor][, unit := "US$2005/veh"][, variable := "CAPEX"][, annualMileage := NULL]

      #CAPEX for Busses, Trucks, Trains and Ships is given combined with non-fuel OPEX in the UCD data
      #Apply assumptions on CAPEX share
      ## Busses
      ## https://mdpi-res.com/d_attachment/wevj/wevj-11-00056/article_deploy/wevj-11-00056.pdf?version=1597829235
      ## electric busses: veh + batt. = 25% of TCO
      CAPEXcombinedUCD[subsectorL2 == "Bus" & technology %in% c("Electric", "FCEV"), value := value * 0.25]
      ## diesel busses: 15% of TCO
      CAPEXcombinedUCD[subsectorL2 == "Bus" & technology %in% c("Liquids", "NG"), value := value * 0.15]
      ## Trucks
      ## https://theicct.org/sites/default/files/publications/TCO-BETs-Europe-white-paper-v4-nov21.pdf
      ## p. 11: retail price = 150k for diesel, 500 - 200k for BEV
      ## p. 22: TCO 550 for diesel, TCO = 850 - 500k for BEV
      ## CAPEX share diesel = 27%, 60-40% for BEV -> 50%
      CAPEXcombinedUCD[subsectorL3 == "trn_freight_road" & technology %in% c("Liquids", "NG"), value := value * 0.3]
      CAPEXcombinedUCD[subsectorL3 == "trn_freight_road" & technology %in% c("Electric", "FCEV"), value := value * 0.5]
      ## Trains
      ## https://www.unescap.org/sites/default/files/1.%20Part%20A.%20Point%20to%20point%20railway%20traffic%20costing%20model.pdf
      ## O&M 80% for low traffic lines
      ## 50% for high traffic lines
      ## -> 60% O&M -> CAPEX share = 40%
      CAPEXcombinedUCD[subsectorL3 %in% c("Freight Rail", "Passenger Rail", "HSR"), value := value * 0.4]
      ## Ships
      ## CCS ships doi:10.1016/j.egypro.2014.11.285
      ## CAPEX ~ 30%
      CAPEXcombinedUCD[subsectorL3 %in% c("Domestic Ship", "International Ship"), value := value * 0.3]
      #Divide by Annual Mileage to get [unit = US$2005/veh/yr]
      CAPEXcombinedUCD <- merge(CAPEXcombinedUCD, annualMileageUCD, all.x = TRUE)
      CAPEXcombinedUCD[, value := value/annualMileage][, unit := "US$2005/veh/yr"]
      #Divide by annuity factor to get CAPEX per veh
      CAPEXcombinedUCD[, value := value/annuityFactor][, unit := "US$2005/veh"][, variable := "CAPEX"][, annualMileage := NULL]
      CAPEXUCD <- rbind(CAPEXUCD, CAPEXcombinedUCD)

      #Inter- and extrapolate all data to model input data years
      data <- list(CAPEXUCD = CAPEXUCD, CAPEXPSI = CAPEXPSI)
      data <- lapply(data, approx_dt, years, "period", "value",
                     c("region", "unit", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName"), extrapolate = TRUE)
      #Merge data
      #PSI > UCD
      #PSI vehicle purchase costs are used for LDV 4 Wheelers in EUR
      PSIcarsEUR <- data$CAPEXPSI[region %in% ISOcountriesMap[Aggregate21to12Reg == "EUR"]$region][, variable := "CAPEX"]
      CAPEXraw <- rbind(PSIcarsEUR, data$CAPEXUCD[!(region %in% ISOcountriesMap[Aggregate21to12Reg == "EUR"]$region & subsectorL1 == "trn_pass_road_LDV_4W")])

      #Check whether data is complete
      check <- merge(completeDataSet, energyIntensity, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("CAPEX input data is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("Unnecessary data is provided")
      }

      quitteobj <- CAPEX
    },
    "SpeedOfModes" = {
      unit <- "km/h"
      description <- "Speed of traveling on technology level. Sources: GCAM"
      weight <- calcOutput("GDP", aggregate = FALSE)[, years, "gdp_SSP2"]

      #read sources
      SpeedGCAM <- toolPrepareGCAM(readSource("GCAM", "speedMotorized"), "speedMotorized")
      SpeedNonMotGCAM <- toolPrepareGCAM(readSource("GCAM", "speedNonMotorized"), "speedNonMotorized")

      #Inter- and extrapolate all data to model input data years
      data <- list(SpeedGCAM = SpeedGCAM, SpeedNonMotGCAM = SpeedNonMotGCAM)
      data <- lapply(data, approx_dt, years, "period", "value",
                     c("region", "unit", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName"), extrapolate = TRUE)

      #Merge data
      SpeedOfModesRaw <- rbind(data$SpeedGCAM, data$LSpeedNonMotGCAM)
      SpeedOfModes <- toolAdjustSpeedOfModes(SpeedOfModesRaw)

      #Check whether data is complete
      check <- merge(completeDataSet, energyIntensity, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("Spped of modes input data is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("Unnecessary data is provided")
      }
      quitteobj <- SpeedOfModes
    }

    )

  return(list(
    x           = as.magpie(as.data.frame(quitteobj)),
    weight      = weight,
    unit        = unit,
    description = description
  ))
}
