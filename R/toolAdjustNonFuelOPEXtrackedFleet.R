#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @param yrs temporal resolution of EDGE-T model
#' @param completeData All combinations of region, period, univocalName and technology in EDGE-T decision tree
#' @param filter list of filters for specific branches in the upper decision tree, containing all associated univocalNames
#' @import data.table
#' @importFrom rmndt magpie2dt
#' @return a quitte object

toolAdjustNonFuelOPEXtrackedFleet <- function(dt, yrs, completeData, filter) {
  variable <- . <- technology <- period <- value <-
    univocalName <- unit <- region <- sector <- check <- NULL

  # 1: Aggregate different non Fuel OPEX types
  dt[univocalName %in% filter$trn_pass_road_LDV_4W, variable := "Operating costs (total non-fuel)"]
  dt <- dt[, .(value = sum(value)), by = c("region", "univocalName", "technology",
                                           "variable", "unit", "period")]
  # 1: Alternative trucks and busses
  # Data for alternative trucks and busses is missing
  BEV <- dt[(univocalName %in% filter$trn_freight_road | univocalName == "Bus") & technology == "Liquids"]
  BEV[, technology := "BEV"]
  FCEV <- dt[(univocalName %in% filter$trn_freight_road | univocalName == "Bus") & technology == "Liquids"]
  FCEV[, technology := "FCEV"]
  altCost <- rbind(BEV, FCEV)
  # Operating subsidies are kept as they are in liquids
  altCostCAPEXnonFuelOPEX <- altCost[variable == "CAPEX and non-fuel OPEX"]
  targetYearEarly <- 2035  # target year for electric trucks and electric and FCEV buses
  targetYearLate <- 2150  # target year for FCEV trucks

  # cost of electric truck is 60% more than a as conventional truck today
  altCostCAPEXnonFuelOPEX[univocalName %in% filter$trn_freight_road & period <= 2020 & technology == "BEV", value := 1.6 * value]
  # cost of a FCEV truck is 80% more than a as conventional truck today
  altCostCAPEXnonFuelOPEX[univocalName %in% filter$trn_freight_road & period <= 2020 & technology == "FCEV", value :=  1.8 * value]
  # cost of electric and H2 buses is 40% more of a conventional bus today
  altCostCAPEXnonFuelOPEX[univocalName == "Bus" & period <= 2020, value := 1.4 * value]

  altCostCAPEXnonFuelOPEX <- altCostCAPEXnonFuelOPEX[period <= 2020 | (univocalName == "Bus" & period >= targetYearEarly) |
                       (univocalName %in% filter$trn_freight_road & technology == "BEV" & period >= targetYearEarly) |
                       (univocalName %in% filter$trn_freight_road & technology == "FCEV" & period >= targetYearLate)]
  # follow linear trends until target years/cost parity with ICE cost -> after the target years we assume no
  # further cost decline. This is somehow odd and should be checked
  altCostCAPEXnonFuelOPEX <- approx_dt(altCostCAPEXnonFuelOPEX, yrs, "period", "value",
                       c("region", "univocalName", "technology", "variable", "unit"), extrapolate = TRUE)
  dt <- rbind(altCostCAPEXnonFuelOPEX, altCost[!variable == "CAPEX and non-fuel OPEX"], dt)

  # 2: Non fuel OPEX are given combined with CAPEX for trucks and busses: Apply assumptions on CAPEX share
  # 2a: Busses
  # https://mdpi-res.com/d_attachment/wevj/wevj-11-00056/article_deploy/wevj-11-00056.pdf?version=1597829235
  # BEV busses: veh + batt. = CAPEX 25% of TCO
  dt[univocalName == "Bus" & technology %in% c("BEV", "FCEV"), value := value * (1 - 0.25)]
  # diesel busses: CAPEX 15% of TCO
  dt[univocalName == "Bus" & technology %in% c("Liquids", "Gases"), value := value * (1 - 0.15)]
  dt[univocalName == "Bus", variable := "Operating costs (total non-fuel)"]


  # 2b: Trucks
  # https://theicct.org/sites/default/files/publications/TCO-BETs-Europe-white-paper-v4-nov21.pdf
  # p. 11: retail price = 150k for diesel, 500 - 200k for BEV
  # p. 22: TCO 550 for diesel, TCO = 850 - 500k for BEV
  # CAPEX share diesel = 27%, 60-40% for BEV -> 50%
  dt[univocalName %in% filter$trn_freight_road & technology %in% c("Liquids", "Gases"), value := value * (1 - 0.3)]
  dt[univocalName %in% filter$trn_freight_road & technology %in% c("BEV", "FCEV"), value := value * (1 - 0.5)]
  dt[univocalName %in% filter$trn_freight_road, variable := "Operating costs (total non-fuel)"]
  #Values given in US$/vehkm need to be transferred to US$/veh/yr with the help of annual mileage
  annualMileage <-  magpie2dt(calcOutput(type = "EdgeTransportSAinputs", subtype = "annualMileage",
                                         warnNA = FALSE, aggregate = FALSE))[, c("unit", "variable") := NULL]
  setnames(annualMileage, "value", "annualMileage")
  #magclass converts "." in vehicle types to "_" (e.g. Truck (0-3_5t))
  setkey(annualMileage, region, univocalName, technology, period)

  #Divide by Annual Mileage to get [unit = US$/veh/yr]
  dt <- merge.data.table(dt, annualMileage, all.x = TRUE)
  dt[univocalName %in% filter$trn_freight_road | univocalName == "Bus", value := value * annualMileage]
  monUnit <- gsub(".*?(\\d{4}).*", "US$\\1", mrdrivers::toolGetUnitDollar())
  dt[univocalName %in% filter$trn_freight_road | univocalName == "Bus", unit := paste0(monUnit, "/veh yr")]
  dt[, annualMileage := NULL]
  dt <- dt[, .(value = sum(value)), by = c("region", "univocalName", "technology",
                                           "variable", "unit", "period")]

  #3: Missing vehicle types in certain countries
  completeData <- completeData[univocalName %in% filter$trn_freight_road | univocalName %in% filter$trn_pass_road_LDV_4W |
                                 univocalName == "Bus"]
  dt <- merge.data.table(dt, completeData, all.y = TRUE)
  dt[, check := NULL]

  # 3a: Some Truck types are missing for many countries in the UCD database

  # Replacing the missing values with values from other countries in the same region does not work at the moment,
  # 2nd approach: Replace with other vehicle types in the same country
  # Find NAs
  # KOR does not feature truck (0-3_5t) and truck (7_5t) in the UCD database
  # NEU, EUR and REF do not feature truck (18t) in the UCD database
  missing18t <- dt[is.na(value) & univocalName == "Truck (18t)"]
  # OAS, SSA and MEA do not feature truck (26t) in the UCD database
  missing26t <- dt[is.na(value) & univocalName == "Truck (26t)"]
  # OAS, SSA, MEA and CAZ do not feature truck(40t) in the UCD database
  missing40t <- dt[is.na(value) & univocalName == "Truck (40t)"]

  # Get values for other truck types
  truck26t <- dt[!is.na(value) & univocalName == "Truck (26t)"]
  truck26t <- truck26t[, c("region", "technology", "period", "value")]
  setnames(truck26t, "value", "truck26t")
  truck7t <- dt[!is.na(value) & univocalName == "Truck (7_5t)"]
  truck7t <- truck7t[, c("region", "technology", "period", "value")]
  setnames(truck7t, "value", "truck7t")
  truck18t <- dt[!is.na(value) & univocalName == "Truck (18t)"]
  truck18t <- truck18t[, c("region", "technology", "period", "value")]
  setnames(truck18t, "value", "truck18t")

  # Replace NAs step by step with values of other truck types until data is complete
  missing18t <- merge.data.table(missing18t, truck26t, by = c("region", "technology", "period"), all.x = TRUE)
  missing18t <- merge.data.table(missing18t, truck7t, by = c("region", "technology", "period"), all.x = TRUE)
  missing18t[, value := truck26t][, truck26t := NULL]
  missing18t[is.na(value), value := truck7t][, truck7t := NULL]
  missing26t <- merge.data.table(missing26t, truck18t, by = c("region", "technology", "period"), all.x = TRUE)
  missing26t <- merge.data.table(missing26t, truck7t, by = c("region", "technology", "period"), all.x = TRUE)
  missing26t[, value := truck18t][, truck18t := NULL]
  missing26t[is.na(value), value := truck7t][, truck7t := NULL]
  missing40t <- merge.data.table(missing40t, truck26t, by = c("region", "technology", "period"), all.x = TRUE)
  missing40t <- merge.data.table(missing40t, truck18t, by = c("region", "technology", "period"), all.x = TRUE)
  missing40t <- merge.data.table(missing40t, truck7t, by = c("region", "technology", "period"), all.x = TRUE)
  missing40t[, value := truck26t][, truck26t := NULL]
  missing40t[is.na(value), value := truck18t][, truck18t := NULL]
  # JPN gets 7_5t assigned -> this should be fixed
  missing40t[is.na(value), value := truck7t][, truck7t := NULL]

  missingTrucks <- rbind(missing18t, missing26t, missing40t)
  # Korea (KOR) is missing all truck types and gets assigned the values of Taiwan (TWN)
  missingTrucks <- missingTrucks[!region == "KOR"][, variable := "Operating costs (total non-fuel)"][, unit := paste0(monUnit, "/veh yr")]

  dt <- rbind(dt[!(is.na(value) & univocalName %in% filter$trn_freight_road)], missingTrucks)
  trucksKOR <- dt[region == "TWN" & univocalName %in% filter$trn_freight_road][, region := "KOR"]
  dt <- rbind(dt, trucksKOR)

  # 3b: Some car vehicle classes are missing as well and are replaced by other vehicle classes
  # Find missing values
  missingVan <- dt[is.na(value) & univocalName == "Van"]
  missingMini <- dt[is.na(value) & univocalName == "Mini Car"]
  missingMid <- dt[is.na(value) & univocalName == "Midsize Car"]
  missingSub <- dt[is.na(value) & univocalName == "Subcompact Car"]
  missingCom <- dt[is.na(value) & univocalName == "Compact Car"]
  missingLar <- dt[is.na(value) & univocalName == "Large Car"]

  # Get values of other vehicle types
  SUV <- dt[!is.na(value) & univocalName == "Large Car and SUV"]
  SUV <- SUV[, c("region", "technology", "period", "value")]
  setnames(SUV, "value", "SUV")
  large <- dt[!is.na(value) & univocalName == "Large Car"]
  large <- large[, c("region", "technology", "period", "value")]
  setnames(large, "value", "large")
  mid <- dt[!is.na(value) & univocalName == "Midsize Car"]
  mid <- mid[, c("region", "technology", "period", "value")]
  setnames(mid, "value", "mid")
  sub <- dt[!is.na(value) & univocalName == "Subcompact Car"]
  sub <- sub[, c("region", "technology", "period", "value")]
  setnames(sub, "value", "sub")
  com <- dt[!is.na(value) & univocalName == "Compact Car"]
  com <- com[, c("region", "technology", "period", "value")]
  setnames(com, "value", "com")
  mini <- dt[!is.na(value) & univocalName == "Mini Car"]
  mini <- mini[, c("region", "technology", "period", "value")]
  setnames(mini, "value", "mini")

  #Assign values of other vehicle types (step by step)
  missingVan <- merge.data.table(missingVan, SUV, by = c("region", "technology", "period"), all.x = TRUE)
  missingVan[, value := SUV][, SUV := NULL]
  missingMini <- merge.data.table(missingMini, sub, by = c("region", "technology", "period"), all.x = TRUE)
  missingMini <- merge.data.table(missingMini, com, by = c("region", "technology", "period"), all.x = TRUE)
  missingMini <- merge.data.table(missingMini, mid, by = c("region", "technology", "period"), all.x = TRUE)
  missingMini <- merge.data.table(missingMini, SUV, by = c("region", "technology", "period"), all.x = TRUE)
  missingMini[, value := sub][, sub := NULL]
  missingMini[is.na(value), value := com][, com := NULL]
  missingMini[is.na(value), value := mid][, mid := NULL]
  # this applies only to Saint Pierre and Miquelon (SPM) -> fairly small
  # (SPM has data for midsize cars in the UCD database, but is listed in EDGE-T as a country
  # without midsize cars) -> after a data update, the country specific vehicle map should be checked again
  missingMini[is.na(value), value := SUV][, SUV := NULL]
  missingMid <- merge.data.table(missingMid, com, by = c("region", "technology", "period"), all.x = TRUE)
  missingMid[, value := com][, com := NULL]
  missingSub <- merge.data.table(missingSub, com, by = c("region", "technology", "period"), all.x = TRUE)
  missingSub <- merge.data.table(missingSub, SUV, by = c("region", "technology", "period"), all.x = TRUE)
  missingSub[, value := com][, com := NULL]
  # This again only applies to SPM
  missingSub[is.na(value), value := SUV][, SUV := NULL]
  missingCom <- merge.data.table(missingCom, SUV, by = c("region", "technology", "period"), all.x = TRUE)
  # This again only applies to SPM
  missingCom[, value := SUV][, SUV := NULL]
  missingLar <- merge.data.table(missingLar, SUV, by = c("region", "technology", "period"), all.x = TRUE)
  missingLar[, value := SUV][, SUV := NULL]

  missing4W <- rbind(missingVan, missingMini, missingMid, missingSub, missingCom, missingLar)
  missing4W[is.na(variable), variable := "Operating costs (total non-fuel)"]
  missing4W[is.na(unit), unit := paste0(monUnit, "/veh yr")]
  dt <- rbind(dt[!(is.na(value) & univocalName %in% filter$trn_pass_road_LDV_4W)], missing4W)

  return(dt)
}
