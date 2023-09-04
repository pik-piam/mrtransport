#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @param ISOcountries list of iso countries
#' @param yrs temporal resolution of EDGE-T model
#' @param completeData complete EDGE-T decision tree
#' @importFrom rmndt magpie2dt
#' @importFrom data.table data.table merge.data.table
#' @return a quitte object

toolAdjustCAPEXtrackedFleet <- function(dt, ISOcountries, yrs, completeData) {
 subsectorL3 <- variable <- . <- value <- region <- regionCode12 <- period <-
   technology <- subsectorL1 <- subsectorL2 <- vehicleType <- markup <- univocalName <-
     sector <- unit <- check <- NULL

  # 1: LDV 4 Wheeler adjustments
  # 1a: Delete Capital costs other, as it is unclear what it repesents and aggregate all CAPEX types
  dt <- dt[!(subsectorL3 == "trn_pass_road_LDV_4W" & variable == "Capital costs (other)")]
  dt <- dt[, .(value = sum(value)), by = c("region", "sector", "subsectorL1", "subsectorL2",
        "subsectorL3", "vehicleType", "technology", "univocalName", "unit", "period")]
  dt[, variable := "CAPEX"]

  # 1b: CAPEX data for LDV 4 Wheelers in EUR from PSI is not region specific
  # and real data is only available for 2015 and 2040 -> rest is interpolated
  # -> define markups on alternative techs based on the percentage difference in EU countries
  LDV4WEUR <- copy(dt[subsectorL3 == "trn_pass_road_LDV_4W"
                      & region %in% ISOcountries[regionCode12 == "EUR"]$region])
  LDV4WEUR[period == 2040 & technology %in% c("Hybrid electric", "BEV"), value := 0.8 * value]
  LDV4WEUR[period == 2040 & technology %in% c("FCEV"), value := 0.8 * value]
  # in 2100, purchase price for BEVs is 0.8 * purchase price, for Hybrid electric is 0.7, for FCEVs is 0.9
  decr <- data.table(technology = c("BEV", "Hybrid electric", "FCEV", "Liquids", "NG"),
                     factor = c(0.8, 0.7, 0.9, 1, 1))
  LDV4WEUR <- merge.data.table(LDV4WEUR, decr, by = "technology")
  LDV4WEUR[period == 2100, value := value[technology == "Liquids"] * factor, by = c("region", "vehicleType")]
  LDV4WEUR[, factor := NULL]
  # add "Large Car"and "Light Truck and SUV" taking the same values as for "Large Car and SUV"
  LDV4WEUR <- rbind(LDV4WEUR,
                    LDV4WEUR[vehicleType == "Large Car and SUV"][, vehicleType := "Light Truck and SUV"],
                    LDV4WEUR[vehicleType == "Large Car and SUV"][, vehicleType := "Large Car"])
  LDV4WEUR <- LDV4WEUR[period %in% c(2015, 2040, 2100)]
  LDV4WEUR <- approx_dt(LDV4WEUR, yrs, "period", "value",
                        c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType",
                          "technology", "univocalName", "variable", "unit"), extrapolate = TRUE)
  LDV4WEUR[, markup := value / value[technology == "Liquids"], by = c("vehicleType", "period")]
  # Apply cost ratio of ICEs vs alternative technologies also on non EUR countries
  LDV4WnonEUR <- copy(dt[subsectorL3 == "trn_pass_road_LDV_4W" &
                           !region %in% ISOcountries[regionCode12 == "EUR"]$region])
  LDV4WnonEUR <- merge.data.table(LDV4WnonEUR, unique(LDV4WEUR[, c("vehicleType", "technology", "period", "markup")],
                                                      by = c("vehicleType", "technology", "period"), all.x = TRUE))
  LDV4WnonEUR[technology %in% c("BEV", "Hybrid electric", "FCEV"), value := value * markup][, markup := NULL]
  LDV4WEUR[, markup := NULL]
  dt <- rbind(dt[!subsectorL3 == "trn_pass_road_LDV_4W"], LDV4WEUR, LDV4WnonEUR)

  #2: Alternative trucks and busses
  # Data for alternative trucks and busses is missing
  BEV <- dt[(subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus") & technology == "Liquids"]
  BEV[, technology := "BEV"]
  FCEV <- dt[(subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus") & technology == "Liquids"]
  FCEV[, technology := "FCEV"]
  altCost <- rbind(BEV, FCEV)
  targetYearEarly <- 2035  ## target year for electric trucks and electric and FCEV buses
  targetYearLate <- 2150  ## target year for FCEV trucks

  # cost of electric truck is 60% more than a as conventional truck today
  altCost[subsectorL1 == "trn_freight_road" & period <= 2020 & technology == "BEV", value := 1.6 * value]
  # cost of a FCEV truck is 80% more than a as conventional truck today
  altCost[subsectorL1 == "trn_freight_road" & period <= 2020 & technology == "FCEV", value :=  1.8 * value]
  # cost of electric and H2 buses is 40% more of a conventional bus today
  altCost[subsectorL2 == "Bus" & period <= 2020, value := 1.4 * value]

  altCost <- altCost[period <= 2020 | (subsectorL2 == "Bus" & period >= targetYearEarly) |
                       (subsectorL1 == "trn_freight_road" & technology == "BEV" & period >= targetYearEarly) |
                       (subsectorL1 == "trn_freight_road" & technology == "FCEV" & period >= targetYearLate)]
  # follow linear trends until target years/cost parity with ICE cost -> after the target years we assume no
  # further cost decline. This is somehow odd and should be checked
  altCost <- approx_dt(altCost, yrs, "period", "value",
                       c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType",
                         "technology", "univocalName", "variable", "unit"), extrapolate = TRUE)
  dt <- rbind(altCost, dt)

  #3: CAPEX are given combined with non fuel OPEX for trucks and busses: Apply assumptions on CAPEX share
  #3a: Busses
  # https://mdpi-res.com/d_attachment/wevj/wevj-11-00056/article_deploy/wevj-11-00056.pdf?version=1597829235
  # BEV busses: veh + batt. = 25% of TCO
  dt[subsectorL2 == "Bus" & technology %in% c("BEV", "FCEV"), value := value * 0.25]
  #3b: diesel busses: 15% of TCO
  dt[subsectorL2 == "Bus" & technology %in% c("Liquids", "NG"), value := value * 0.15]
  dt[subsectorL2 == "Bus", variable := "CAPEX"]
  #3c: Trucks
  # https://theicct.org/sites/default/files/publications/TCO-BETs-Europe-white-paper-v4-nov21.pdf
  # p. 11: retail price = 150k for diesel, 500 - 200k for BEV
  # p. 22: TCO 550 for diesel, TCO = 850 - 500k for BEV
  # CAPEX share diesel = 27%, 60-40% for BEV -> 50%
  dt[subsectorL1 == "trn_freight_road" & technology %in% c("Liquids", "NG"), value := value * 0.3]
  dt[subsectorL1 == "trn_freight_road" & technology %in% c("BEV", "FCEV"), value := value * 0.5]
  dt[subsectorL1 == "trn_freight_road", variable := "CAPEX"]
  # Values given in US$2005/vehkm need to be transferred to US$2005/veh with the help of annual mileage
  # and annuity factor
  annualMileage <-  magpie2dt(calcOutput(type = "EdgeTransportSAinputs", subtype = "annualMileage",
                                         warnNA = FALSE, aggregate = FALSE))[, c("unit", "variable") := NULL]
  setnames(annualMileage, "value", "annualMileage")
  # magclass converts "." in vehicle types to "_" (e.g. Truck (0-3.5t))
  annualMileage[subsectorL1 == "trn_freight_road", univocalName := gsub("_", ".", univocalName)]
  annualMileage[subsectorL1 == "trn_freight_road", vehicleType := gsub("_", ".", vehicleType)]
  setkey(annualMileage, region,  sector, subsectorL1, subsectorL2, subsectorL3, vehicleType, technology,
         univocalName, period)

  # UCD applied interest rate of 10% and uniform vehicle lifetime of 15 yrs
  # (https://itspubs.ucdavis.edu/publication_detail.php?id=1884)
  # Calc annuity factor
  discountRate <- 0.1   #discount rate for vehicle purchases
  lifeTime <- 15    #Number of years over which vehicle capital payments are amortized
  annuityFactor <- (discountRate * (1 + discountRate) ^ lifeTime) / ((1 + discountRate) ^ lifeTime - 1)
  # Divide by Annual Mileage to get [unit = US$2005/veh/yr]
  dt <- merge.data.table(dt, annualMileage, all.x = TRUE)
  dt[subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus", value := value * annualMileage]
  dt[subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus", unit := "US$2005/veh/yr"]
  # Divide by annuity factor to get CAPEX per veh
  dt[subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus", value := value / annuityFactor]
  dt[, annualMileage := NULL]
  dt[subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus",  unit := "US$2005/veh"]

  #4: Missing vehicle types in certain countries
  completeData <- completeData[subsectorL1 == "trn_freight_road" | subsectorL3 == "trn_pass_road_LDV_4W" |
                                 subsectorL2 == "Bus"]
  dt <- merge.data.table(dt, completeData, all.y = TRUE)

  #5a: Some Truck types are missing for many countries in the UCD database

  # First approach: If other countries of the same region feature these truck types, assign mean values of them
  # missingTrucks <- dt[subsectorL1 == "trn_freight_road"] # nolint: commented_code_linter
  # missingTrucks <- missingTrucks[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", # nolint: commented_code_linter
  # "technology", "period", "value")] # nolint: commented_code_linter
  # missingTrucks <- dcast(missingTrucks, region + sector + subsectorL1 + subsectorL2 + subsectorL3 + # nolint: commented_code_linter
  # technology + period ~ vehicleType, value.var = "value") # nolint: commented_code_linter
  # missingTrucks <- merge.data.table(missingTrucks, ISOcountries[, c("region", "regionCode21", "regionCode12")], by = "region") # nolint: commented_code_linter
  # #Calculating means for every truck type (using regionCode12 instead does not lead to more available values) # nolint: commented_code_linter
  # missingTrucks[, `mean Truck (0-3.5t)` := mean(`Truck (0-3.5t)`, na.rm = TRUE), by = c("technology", "period", "regionCode21")] # nolint: commented_code_linter
  # missingTrucks[, `mean Truck (7.5t)` := mean(`Truck (7.5t)`, na.rm = TRUE), by = c("technology", "period", "regionCode21")] # nolint: commented_code_linter
  # missingTrucks[, `mean Truck (18t)` := mean(`Truck (18t)`, na.rm = TRUE), by = c("technology", "period", "regionCode21")] # nolint: commented_code_linter
  # missingTrucks[, `mean Truck (26t)` := mean(`Truck (26t)`, na.rm = TRUE), by = c("technology", "period", "regionCode21")] # nolint: commented_code_linter
  # missingTrucks[, `mean Truck (40t)` := mean(`Truck (40t)`, na.rm = TRUE), by = c("technology", "period", "regionCode21")] # nolint: commented_code_linter
  #
  # missingTrucks[is.na(`Truck (0-3.5t)`), `Truck (0-3.5t)` := `mean Truck (0-3.5t)`] # nolint: commented_code_linter
  # missingTrucks[is.na(`Truck (7.5t)`), `Truck (7.5t)` := `mean Truck (7.5t)`] # nolint: commented_code_linter
  # missingTrucks[is.na(`Truck (18t)`), `Truck (18t)` := `mean Truck (18t)`] # nolint: commented_code_linter
  # missingTrucks[is.na(`Truck (26t)`), `Truck (26t)` := `mean Truck (26t)`] # nolint: commented_code_linter
  # missingTrucks[is.na(`Truck (40t)`), `Truck (40t)` := `mean Truck (40t)`] # nolint: commented_code_linter
  # meanTrucks <-  unique(missingTrucks[, c("regionCode21", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "technology", "period", # nolint: commented_code_linter
  # "mean Truck (0-3.5t)", "mean Truck (7.5t)", "mean Truck (18t)", "mean Truck (26t)", "mean Truck (40t)")]) # nolint: commented_code_linter
  # missingTrucks <- missingTrucks[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "technology", "period", # nolint: commented_code_linter
  # "Truck (0-3.5t)", "Truck (7.5t)", "Truck (18t)", "Truck (26t)", "Truck (40t)")] # nolint: commented_code_linter
  # missingTrucks <- melt(missingTrucks,  id.vars = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", # nolint: commented_code_linter
  # "technology", "period"), variable.name = "vehicleType") # nolint: commented_code_linter
  # missingTrucks[is.nan(value), value := NA] # nolint: commented_code_linter
  #
  # #this is a good example, that our current truck data is somewhat odd, mean Truck 18t
  # are nearly double the price of truck 26t
  # #that is a result of small countries setting the price due to missing data.
  # Here, Puerto Rico (PRI) is setting the price for 18t and
  # #is assigned to the USA region in UCD (therefore prices are much higher)
  # meanTrucks <- meanTrucks[regionCode21 == "LAM"]   # nolint: commented_code_linter
  # meanTrucks[, fac1 := `mean Truck (0-3.5t)` / `mean Truck (7.5t)`] # nolint: commented_code_linter
  # meanTrucks[, fac2 := `mean Truck (7.5t)` / `mean Truck (18t)`] # nolint: commented_code_linter
  # meanTrucks[, fac3 := `mean Truck (18t)` / `mean Truck (26t)`] # nolint: commented_code_linter
  # meanTrucks[, fac4 := `mean Truck (26t)` / `mean Truck (40t)`] # nolint: commented_code_linter
  # meanTrucks <- meanTrucks[, .(fac1 = mean(fac1), fac2 = mean(fac2), fac3 = mean(fac3), fac4 = mean(fac4))] # nolint: commented_code_linter

  #Replacing the missing values with values from other countries in the same region does not work at the moment,
  #2nd approach: Replace with other vehicle types in the same country

  #Find NAs
  #KOR does not feature truck (0-3.5t) and truck (7.5t) in the UCD database
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
  #JPN gets 7.5t assigned -> this should be fixed
  missing40t[is.na(value), value := truck7t][, truck7t := NULL]

  missingTrucks <- rbind(missing18t, missing26t, missing40t)
  #Korea (KOR) is missing all truck types and gets assigned the values of Taiwan (TWN)
  missingTrucks <- missingTrucks[!region == "KOR"][, variable := "CAPEX"][, unit := "US$2005/veh"]

  dt <- rbind(dt[!(is.na(value) & subsectorL1 == "trn_freight_road")], missingTrucks)
  trucksKOR <- dt[region == "TWN" & subsectorL1 == "trn_freight_road"][, region := "KOR"]
  dt <- rbind(dt, trucksKOR)

  #5b: Some car vehicle classes are missing as well and are replaced by other vehicle classes
  #Find missing values
  missingVan <- dt[is.na(value) & vehicleType == "Van"]
  missingMini <- dt[is.na(value) & vehicleType == "Mini Car"]
  missingMid <- dt[is.na(value) & vehicleType == "Midsize Car"]
  missingSub <- dt[is.na(value) & vehicleType == "Subcompact Car"]
  missingCom <- dt[is.na(value) & vehicleType == "Compact Car"]
  missingLar <- dt[is.na(value) & vehicleType == "Large Car"]

  # Get values of other vehicle types
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

  # Assign values of other vehicle types (step by step)
  missingVan <- merge.data.table(missingVan, SUV, by = c("region", "technology", "period"), all.x = TRUE)
  missingVan[, value := SUV][, SUV := NULL]

  missingMini <- merge.data.table(missingMini, sub, by = c("region", "technology", "period"), all.x = TRUE)
  missingMini <- merge.data.table(missingMini, com, by = c("region", "technology", "period"), all.x = TRUE)
  missingMini <- merge.data.table(missingMini, mid, by = c("region", "technology", "period"), all.x = TRUE)
  missingMini <- merge.data.table(missingMini, SUV, by = c("region", "technology", "period"), all.x = TRUE)
  missingMini[, value := sub][, sub := NULL]
  missingMini[is.na(value), value := com][, com := NULL]
  missingMini[is.na(value), value := mid][, mid := NULL]
  # this applies only to Saint Pierre and Miquelon (SPM)
  # -> fairly small (SPM has data for midsize cars in the UCD database, but is listed in EDGE-T as a country
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
  missing4W[is.na(variable), variable := "CAPEX"]
  missing4W[is.na(unit), unit := "US$2005/veh"]
  dt <- rbind(dt[!(is.na(value) & subsectorL3 == "trn_pass_road_LDV_4W")], missing4W)[, check := NULL]

  dt <- dt[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType",
               "technology", "univocalName", "variable", "unit", "period", "value")]
  setkey(dt,  region, sector, subsectorL1, subsectorL2, subsectorL3, vehicleType, technology,
         univocalName, variable, unit, period)

return(dt)
}
