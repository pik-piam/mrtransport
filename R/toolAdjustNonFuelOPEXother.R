#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @importFrom rmndt magpie2dt
#' @import data.table
#' @return a quitte object

toolAdjustNonFuelOPEXother <- function(dt, ISOcountries, yrs, completeData) {
  
  #1: Aggregate different non fuel OPEX types
  dt <- dt[, .(value = sum(value)), by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "univocalName", "unit", "period")]
  dt[, variable := "Non fuel OPEX"]
  
  #2: Non fuel OPEX are given combined with CAPEX for shipping and rail. Apply shares
  ## Trains
  ## https://www.unescap.org/sites/default/files/1.%20Part%20A.%20Point%20to%20point%20railway%20traffic%20costing%20model.pdf
  ## O&M 80% for low traffic lines
  ## 50% for high traffic lines
  ## -> 60% O&M -> CAPEX share = 40%
  dt[subsectorL1 %in% c("Freight Rail", "Passenger Rail", "HSR"), value := value * (1 - 0.4)]
  dt[subsectorL1 %in% c("Freight Rail", "Passenger Rail", "HSR"), variable := "Non fuel OPEX"]
  ## Ships
  ## CCS ships doi:10.1016/j.egypro.2014.11.285
  ## CAPEX ~ 30%
  dt[subsectorL1 %in% c("Domestic Ship", "International Ship"), value := value * (1 - 0.3)]
  dt[subsectorL1 %in% c("Domestic Ship", "International Ship"), variable := "Non fuel OPEX"]
  
  #3: Add hydrogen airplanes
  h2Air <- dt[vehicleType == "Domestic Aviation_tmp_vehicletype" & technology == "Liquids"][, technology := "Hydrogen"]
  ## following https://www.fch.europa.eu/sites/default/files/FCH%20Docs/20200507_Hydrogen%20Powered%20Aviation%20report_FINAL%20web%20%28ID%208706035%29.pdf
  ## maintenance costs are 50% higher than than a liquids fuelled airplane
  h2Air[vehicleType =="Domestic Aviation_tmp_vehicletype" & period <= 2020,
        value := 1.5 * value]
  ## for hydrogen airplanes, in between 2020 and 2040 the cost follows a linear trend, and reaches a value 30% higher than a liquids fuelled airplane
  h2Air[vehicleType == "Domestic Aviation_tmp_vehicletype" & period >= 2020,
        value := ifelse(period <= 2040, value[period == 2020] + (1.3 * value[period == 2100] - value[period == 2020]) * (period - 2020) / (2100 - 2020), 1.3 * value[period == 2100])]
  dt <- rbind(dt, h2Air)
  
  #4: Some two wheeler classes are missing and are replaced by other vehicle classes
  #Find missing values
  completeData <- completeData[!(subsectorL1 %in% c("trn_freight_road", "Cycle", "Walk") | subsectorL3 == "trn_pass_road_LDV_4W" | subsectorL2 == "Bus")]
  dt <- merge(dt, completeData, all.y = TRUE)
  
  missing50 <- dt[is.na(value) & vehicleType == "Motorcycle (50-250cc)"]
  missing250 <- dt[is.na(value) & vehicleType == "Motorcycle (>250cc)"]
  missingMoped <- dt[is.na(value) & vehicleType == "Moped"]
  
  #Get values of other vehicle types
  twoW50 <- dt[!is.na(value) & vehicleType == "Motorcycle (50-250cc)"]
  twoW50 <- twoW50[, c("region", "technology", "period", "value")]
  setnames(twoW50, "value", "twoW50")
  
  twoW250 <- dt[!is.na(value) & vehicleType == "Motorcycle (>250cc)"]
  twoW250 <- twoW250[, c("region", "technology", "period", "value")]
  setnames(twoW250, "value", "twoW250")
  
  twoWmoped <- dt[!is.na(value) & vehicleType == "Moped"]
  twoWmoped <- twoWmoped[, c("region", "technology", "period", "value")]
  setnames(twoWmoped, "value", "twoWmoped")
  
  #Assign values of other vehicle types (step by step)
  missing50 <- merge(missing50, twoW250, by = c("region", "technology", "period"), all.x = TRUE)
  missing50[, value := twoW250][, twoW250 := NULL]
  
  missing250 <- merge(missing250, twoW50, by = c("region", "technology", "period"), all.x = TRUE)
  missing250[, value := twoW50][, twoW50 := NULL]
  missing250 <- missing250[!region == "UMI"]
  #United States Minor Outlying Islands (UMI) is a special case -> UCD assigns it to south east asia, but GCAM to the USA. USA only features motorcycles > 250cc.
  #As the map for country specific vehicle types is based on the energy service demand that comes from GCAM, UMI only features motorcycles > 250cc. However, the costs are supplied by UCD
  #where south east asia only features mopeds and motorcycles > 250cc. We assign UMI to OAS
  #1st approach: take mean value of OAS for UMI
  # missingUMI <- dt[!is.na(value) & vehicleType == "Motorcycle (>250cc)"]
  # missingUMI <- merge(missingUMI, ISOcountries[, c("region", "RegionCode")], by = "region")
  # missingUMI <- missingUMI[RegionCode == "OAS"]
  # missingUMI <- missingUMI[, .(value = mean(value)), by = c("sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "variable", "univocalName", "unit", "period")]
  #BEV > 250 is featured only by a small number of countries where prices are overall much higher -> this would lead to unrealistic prices for BEV compared to ICEs
  #2nd approach: take prices of Tuvalu (TUV) from lower category (costs are somewhat similar) (UMI is anyway really small and will add little to OAS overall)
  
  missingUMI <- dt[region == "TUV" & vehicleType == "Motorcycle (50-250cc)"][, region := "UMI"]
  missingUMI[, vehicleType := "Motorcycle (>250cc)"][, univocalName := "Motorcycle (>250cc)"]
  missing250 <- rbind(missing250, missingUMI)
  
  missingMoped <- merge(missingMoped, twoW50, by = c("region", "technology", "period"), all.x = TRUE)
  missingMoped <- merge(missingMoped, twoW250, by = c("region", "technology", "period"), all.x = TRUE)
  missingMoped[, value := twoW50][, twoW50 := NULL]
  missingMoped[is.na(value), value := twoW250][, twoW250 := NULL]
  
  missing2W <- rbind(missing50, missing250, missingMoped)
  missing2W[, unit := "US$2005/vehkm"][, variable := "Non fuel OPEX"]
  
  dt <- rbind(dt[!(is.na(value) & subsectorL3 == "trn_pass_road_LDV_2W")], missing2W)
  dt[, check := NULL]
  
  return(dt)
  
}
