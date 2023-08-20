#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @importFrom rmndt magpie2dt
#' @importFrom data.table data.table merge
#' @return a quitte object

toolAdjustNonFuelOPEXtrackedFleet <- function(dt, yrs, completeData) {
  BEV <- FCEV <- altCost <- targetYearEarly <- targetYearLate <- NULL

  #1: Aggregate different non Fuel OPEX types
  dt <- dt[, .(value = sum(value)), by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "univocalName", "unit", "period")]
  dt[subsectorL3 == "trn_pass_road_LDV_4W", variable := "Non fuel OPEX"]

  #1: Alternative trucks and busses
  #Data for alternative trucks and busses is missing
  BEV <- dt[(subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus") & technology == "Liquids"][, technology := "BEV"]
  FCEV <- dt[(subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus") & technology == "Liquids"][, technology := "FCEV"]
  altCost <- rbind(BEV, FCEV)
  targetYearEarly <- 2035  ## target year for electric trucks and electric and FCEV buses
  targetYearLate <- 2150  ## target year for FCEV trucks

  ## cost of electric truck is 60% more than a as conventional truck today
  altCost[subsectorL1 == "trn_freight_road" & period <= 2020 & technology == "BEV", value := 1.6 * value]
  ## cost of a FCEV truck is 80% more than a as conventional truck today
  altCost[subsectorL1 == "trn_freight_road" & period <= 2020 & technology == "FCEV", value :=  1.8 * value]
  ## cost of electric and H2 buses is 40% more of a conventional bus today
  altCost[subsectorL2 == "Bus" & period <= 2020, value := 1.4 * value]

  altCost <- altCost[period <= 2020 | (subsectorL2 == "Bus" & period >= targetYearEarly) | (subsectorL1 == "trn_freight_road" & technology == "BEV" & period >= targetYearEarly) |
                       (subsectorL1 == "trn_freight_road" & technology == "FCEV" & period >= targetYearLate)]
  #follow linear trends until target years/cost parity with ICE cost -> after the target years we assume no further cost decline. This is somehow odd and should be checked
  altCost <- approx_dt(altCost, yrs, "period", "value",
                       c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "univocalName", "variable", "unit"), extrapolate = TRUE)
  dt <- rbind(altCost, dt)

  #2:non fuel OPEX are given combined with CAPEX for trucks and busses: Apply assumptions on CAPEX share
  #3a: Busses
  ## https://mdpi-res.com/d_attachment/wevj/wevj-11-00056/article_deploy/wevj-11-00056.pdf?version=1597829235
  ## BEV busses: veh + batt. = CAPEX 25% of TCO
  dt[subsectorL2 == "Bus" & technology %in% c("BEV", "FCEV"), value := value * (1 - 0.25)]
  ## diesel busses: CAPEX 15% of TCO
  dt[subsectorL2 == "Bus" & technology %in% c("Liquids", "NG"), value := value * (1- 0.15)]
  dt[subsectorL2 == "Bus", variable := "Non fuel OPEX"]
  ## Trucks
  ## https://theicct.org/sites/default/files/publications/TCO-BETs-Europe-white-paper-v4-nov21.pdf
  ## p. 11: retail price = 150k for diesel, 500 - 200k for BEV
  ## p. 22: TCO 550 for diesel, TCO = 850 - 500k for BEV
  ## CAPEX share diesel = 27%, 60-40% for BEV -> 50%
  dt[subsectorL1 == "trn_freight_road" & technology %in% c("Liquids", "NG"), value := value * (1 - 0.3)]
  dt[subsectorL1 == "trn_freight_road" & technology %in% c("BEV", "FCEV"), value := value * (1 - 0.5)]
  dt[subsectorL1 == "trn_freight_road", variable := "Non fuel OPEX"]
  #Values given in US$2005/vehkm need to be transferred to US$2005/veh/yr with the help of annual mileage
  annualMileage <-  magpie2dt(calcOutput(type = "EdgeTransportSAinputs", subtype = "annualMileage",  warnNA = FALSE, aggregate = FALSE))[, c("unit", "variable") := NULL]
  setnames(annualMileage, "value", "annualMileage")
  #magclass converts "." in vehicle types to "_" (e.g. Truck (0-3.5t))
  annualMileage[subsectorL1 == "trn_freight_road", univocalName := gsub("_", ".", univocalName)]
  annualMileage[subsectorL1 == "trn_freight_road", vehicleType := gsub("_", ".", vehicleType)]
  setkey(annualMileage, region,  sector, subsectorL1, subsectorL2, subsectorL3, vehicleType, technology, univocalName, period)

  #Divide by Annual Mileage to get [unit = US$2005/veh/yr]
  dt <- merge(dt, annualMileage, all.x = TRUE)
  dt[subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus" , value := value * annualMileage]
  dt[subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus" , unit := "US$2005/veh/yr"][, annualMileage := NULL]

  #3: Missing vehicle types in certain countries
  completeData <- completeData[subsectorL1 == "trn_freight_road" | subsectorL3 == "trn_pass_road_LDV_4W" | subsectorL2 == "Bus"]
  dt <- merge(dt, completeData, all.y = TRUE)

  #3a: Some Truck types are missing for many countries in the UCD database

  #Replacing the missing values with values from other countries in the same region does not work at the moment,
  #2nd approach: Replace with other vehicle types in the same country
  #Find NAs
  #KOR does not feature truck (0-3.5t) and truck (7.5t) in the UCD database
  missing3t <-  dt[is.na(value) & vehicleType == "Truck (0-3.5t)"]
  missing7t <-  dt[is.na(value) & vehicleType == "Truck (7.5t)"]
  #NEU, EUR and REF do not feature truck (18t) in the UCD database
  missing18t <- dt[is.na(value) & vehicleType == "Truck (18t)"]
  #OAS, SSA and MEA do not feature truck (26t) in the UCD database
  missing26t <- dt[is.na(value) & vehicleType == "Truck (26t)"]
  #OAS, SSA, MEA and CAZ do not feature truck(40t) in the UCD database
  missing40t <- dt[is.na(value) & vehicleType == "Truck (40t)"]

  #Get values for other truck types
  truck26t <- dt[!is.na(value) & vehicleType == "Truck (26t)"]
  truck26t <- truck26t[, c("region", "technology", "period", "value")]
  setnames(truck26t, "value", "truck26t")

  truck7t <- dt[!is.na(value) & vehicleType == "Truck (7.5t)"]
  truck7t <- truck7t[, c("region", "technology", "period", "value")]
  setnames(truck7t, "value", "truck7t")

  truck18t <- dt[!is.na(value) & vehicleType == "Truck (18t)"]
  truck18t <- truck18t[, c("region", "technology", "period", "value")]
  setnames(truck18t, "value", "truck18t")

  #Replace NAs step by step with values of other truck types until data is complete
  missing18t <- merge(missing18t, truck26t, by = c("region", "technology", "period"), all.x = TRUE)
  missing18t <- merge(missing18t, truck7t, by = c("region", "technology", "period"), all.x = TRUE)
  missing18t[, value := truck26t][, truck26t := NULL]
  missing18t[is.na(value), value := truck7t][, truck7t := NULL]

  missing26t <- merge(missing26t, truck18t, by = c("region", "technology", "period"), all.x = TRUE)
  missing26t <- merge(missing26t, truck7t, by = c("region", "technology", "period"), all.x = TRUE)
  missing26t[, value := truck18t][, truck18t := NULL]
  missing26t[is.na(value), value := truck7t][, truck7t := NULL]

  missing40t <- merge(missing40t, truck26t, by = c("region", "technology", "period"), all.x = TRUE)
  missing40t <- merge(missing40t, truck18t, by = c("region", "technology", "period"), all.x = TRUE)
  missing40t <- merge(missing40t, truck7t, by = c("region", "technology", "period"), all.x = TRUE)
  missing40t[, value := truck26t][, truck26t := NULL]
  missing40t[is.na(value), value := truck18t][, truck18t := NULL]
  #JPN gets 7.5t assigned -> this should be fixed
  missing40t[is.na(value), value := truck7t][, truck7t := NULL]

  missingTrucks <- rbind(missing18t, missing26t, missing40t)
  #Korea (KOR) is missing all truck types and gets assigned the values of Taiwan (TWN)
  missingTrucks <- missingTrucks[!region == "KOR"][, variable := "CAPEX"][, unit := "US$2005/veh"]

  dt <- rbind(dt[!(is.na(value) & subsectorL1 == "trn_freight_road")], missingTrucks)
  trucksKOR <- dt[region == "TWN" & subsectorL1 == "trn_freight_road"][, region := "KOR"]
  dt <- rbind(dt, trucksKOR)

  #3b: Some car vehicle classes are missing as well and are replaced by other vehicle classes
  #Find missing values
  missingVan <- dt[is.na(value) & vehicleType == "Van"]
  missingMini <- dt[is.na(value) & vehicleType == "Mini Car"]
  missingMid <- dt[is.na(value) & vehicleType == "Midsize Car"]
  missingSub <- dt[is.na(value) & vehicleType == "Subcompact Car"]
  missingCom <- dt[is.na(value) & vehicleType == "Compact Car"]
  missingLar <- dt[is.na(value) & vehicleType == "Large Car"]

  #Get values of other vehicle types
  SUV <- dt[!is.na(value) & vehicleType == "Large Car and SUV"]
  SUV <- SUV[, c("region", "technology", "period", "value")]
  setnames(SUV, "value", "SUV")

  large <- dt[!is.na(value) & vehicleType == "Large Car"]
  large <- large[, c("region", "technology", "period", "value")]
  setnames(large, "value", "large")

  mid <- dt[!is.na(value) & vehicleType == "Midsize Car"]
  mid <- mid[, c("region", "technology", "period", "value")]
  setnames(mid, "value", "mid")

  sub <- dt[!is.na(value) & vehicleType == "Subcompact Car"]
  sub <- sub[, c("region", "technology", "period", "value")]
  setnames(sub, "value", "sub")

  com <- dt[!is.na(value) & vehicleType == "Compact Car"]
  com <- com[, c("region", "technology", "period", "value")]
  setnames(com, "value", "com")

  mini <- dt[!is.na(value) & vehicleType == "Mini Car"]
  mini <- mini[, c("region", "technology", "period", "value")]
  setnames(mini, "value", "mini")

  #Assign values of other vehicle types (step by step)
  missingVan <- merge(missingVan, SUV, by = c("region", "technology", "period"), all.x = TRUE)
  missingVan[, value := SUV][, SUV := NULL]

  missingMini <- merge(missingMini, sub, by = c("region", "technology", "period"), all.x = TRUE)
  missingMini <- merge(missingMini, com, by = c("region", "technology", "period"), all.x = TRUE)
  browser()
  missingMini <- merge(missingMini, mid, by = c("region", "technology", "period"), all.x = TRUE)
  missingMini <- merge(missingMini, SUV, by = c("region", "technology", "period"), all.x = TRUE)
  missingMini[, value := sub][, sub := NULL]
  missingMini[is.na(value), value := com][, com := NULL]
  missingMini[is.na(value), value := mid][, mid := NULL]
  #this applies only to Saint Pierre and Miquelon (SPM) -> fairly small (SPM has data for midsize cars in the UCD database, but is listed in EDGE-T as a country
  #without midsize cars) -> after a data update, the country specific vehicle map should be checked again
  missingMini[is.na(value), value := SUV][, SUV := NULL]

  missingMid <- merge(missingMid, com, by = c("region", "technology", "period"), all.x = TRUE)
  missingMid[, value := com][, com := NULL]

  missingSub <- merge(missingSub, com, by = c("region", "technology", "period"), all.x = TRUE)
  missingSub <- merge(missingSub, SUV, by = c("region", "technology", "period"), all.x = TRUE)
  missingSub[, value := com][, com := NULL]
  #This again only applies to SPM
  missingSub[is.na(value), value := SUV][, SUV := NULL]

  missingCom <- merge(missingCom, SUV, by = c("region", "technology", "period"), all.x = TRUE)
  #This again only applies to SPM
  missingCom[, value := SUV][, SUV := NULL]

  missingLar <- merge(missingLar, SUV, by = c("region", "technology", "period"), all.x = TRUE)
  missingLar[, value := SUV][, SUV := NULL]

  missing4W <- rbind(missingVan, missingMini, missingMid, missingSub, missingCom, missingLar)
  missing4W[is.na(variable), variable := "Non fuel OPEX"]
  missing4W[is.na(unit), unit := "US$2005/veh/yr"]
  dt <- rbind(dt[!(is.na(value) & subsectorL3 == "trn_pass_road_LDV_4W")], missing4W)

  return(dt)
}
