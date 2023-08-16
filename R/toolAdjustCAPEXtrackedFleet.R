#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @importFrom rmndt magpie2dt
#' @importFrom data.table data.table merge
#' @return a quitte object

toolAdjustCAPEXtrackedFleet <- function(dt, ISOcountries, yrs, completeData) {
  LDV4WEUR <- decr <- LDV4WnonEUR <- markup <- BEV <- FCEV <- altCost <- targetYearEarly <- targetYearLate <- NULL

  ## CAPEX data for LDV 4 Wheelers in EUR from PSI is not region specific
  #+ real data is only available for 2015 and 2040 -> rest is interpolated
  # -> define markups on alternative techs based on the percentage difference in EU countries
  LDV4WEUR <- copy(dt[subsectorL3 == "trn_pass_road_LDV_4W" & region %in% ISOcountries[Aggregate21to12Reg == "EUR"]$region])
  LDV4WEUR[period == 2040 & technology %in% c("Hybrid electric", "BEV"), value := 0.8 * value]
  LDV4WEUR[period == 2040 & technology %in% c("FCEV"), value := 0.8 * value]
  ## in 2100, purchase price for BEVs is 0.8 * purchase price, for Hybrid electric is 0.7, for FCEVs is 0.9
  decr <- data.table(technology = c("BEV", "Hybrid electric", "FCEV", "Liquids", "NG"), factor = c(0.8, 0.7, 0.9, 1, 1))
  LDV4WEUR <- merge(LDV4WEUR, decr, by = "technology")
  LDV4WEUR[period == 2100, value := value[technology == "Liquids"] * factor, by = c("region", "vehicleType")][, factor := NULL]
  ## add "Large Car"and "Light Truck and SUV" taking the same values as for "Large Car and SUV"
  LDV4WEUR <- rbind(LDV4WEUR,
                    LDV4WEUR[vehicleType == "Large Car and SUV"][, vehicleType := "Light Truck and SUV"],
                    LDV4WEUR[vehicleType == "Large Car and SUV"][, vehicleType := "Large Car"])
  LDV4WEUR <- LDV4WEUR[period %in% c(2015, 2040, 2100)]
  LDV4WEUR <- approx_dt(LDV4WEUR, yrs, "period", "value",
                        c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "univocalName", "variable", "unit"), extrapolate = TRUE)
  LDV4WEUR[, markup := value / value[technology == "Liquids"], by = c("vehicleType", "period")]
  #Apply cost ratio of ICEs vs alternative technologies also on non EUR countries
  LDV4WnonEUR <- copy(dt[subsectorL3 == "trn_pass_road_LDV_4W" & !region %in% ISOcountries[Aggregate21to12Reg == "EUR"]$region])
  LDV4WnonEUR <- merge(LDV4WnonEUR, unique(LDV4WEUR[, c("vehicleType", "technology", "period", "markup")], by = c("vehicleType", "technology", "period"), all.x = TRUE))
  LDV4WnonEUR[technology %in% c("BEV", "Hybrid electric", "FCEV"), value := value * markup][, markup := NULL]
  LDV4WEUR[, markup := NULL]
  dt <- rbind(dt[!subsectorL3 == "trn_pass_road_LDV_4W"], LDV4WEUR, LDV4WnonEUR)

  #Data for alternative trucks and busses is missing
  BEV <- dt[subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus" & technology == "Liquids" ][, technology := "BEV"]
  FCEV <- dt[subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus" & technology == "Liquids" ][, technology := "FCEV"]
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

  browser()
  completeData <- completeData[subsectorL1 == "trn_freight_road" | subsectorL3 == "trn_pass_road_LDV_4W" | subsectorL2 == "Bus"]
  dt <- merge(dt, completeData, all.y = TRUE)
  dt <- rbind(dt[!(region %in% c("KOR") & vehicleType %in% c("Truck (0-3.5t)", "Truck (7.5t)"))], dt[region %in% c("JPN") & vehicleType %in% c("Truck (0-3.5t)", "Truck (7.5t)")][, region := "KOR"])
  #Some Truck types are missing for many countries, use those from the lower category (this fix should be removed by updateing the database in the future)
  missing18t <- dt[is.na(value) & vehicleType == "Truck (18t)"]

  dt[is.na(value) & vehicleType == "Truck (40t)", value := value[vehicleType == "Truck (26t)"], by = c("region", "technology", "period")]
  dt <- rbind(dt,
                dt[region == "JPN" & vehicleType %in% c("Truck (0-3.5t)", "Truck (7.5t)")][, region := "KOR"], # Truck (0-3.5t) is missing in KOR (South Korea), choose same as in Japan
                dt[region %in% unique(EU_data$dem_eurostat$region) & vehicle_type == "Truck (0-3.5t)"][, vehicle_type := "Truck (7.5t)"],
                dt[region %in% unique(EU_data$dem_eurostat$region) & vehicle_type == "Truck (26t)"][, vehicle_type := "Truck (18t)"],
                dt[region %in% unique(REMIND2region_MAPPING[region=="REF", region]) & vehicle_type == "Truck (26t)"][, vehicle_type := "Truck (18t)"],      ## REF has missing 18t
                dt[region %in% unique(REMIND2region_MAPPING[region=="REF", region]) & vehicle_type == "Truck (0-3.5t)"][, vehicle_type := "Truck (7.5t)"],  ## REF has missing 7.5t
                dt[region %in% unique(REMIND2region_MAPPING[region=="CAZ", region]) & vehicle_type == "Truck (26t)"][, vehicle_type := "Truck (40t)"],      ## CAZ has missing 40t
                dt[region %in% unique(REMIND2region_MAPPING[region %in% c("OAS", "SSA"), region]) & vehicle_type == "Truck (18t)"][, vehicle_type := "Truck (26t)"],  ## OAS and SSAhas missing 26t
                dt[region %in% unique(REMIND2region_MAPPING[region %in% c("OAS", "SSA"), region]) & vehicle_type == "Truck (18t)"][, vehicle_type := "Truck (40t)"],  ## OAS has missing 40t
                dt[region %in% unique(REMIND2region_MAPPING[region %in% c("CAZ"), region]) & vehicle_type == "Large Car"][, vehicle_type := "Large Car and SUV"],  ## OAS has missing 40t
                dt[region %in% unique(REMIND2region_MAPPING[region %in% c("OAS"), region]) & vehicle_type == "Large Car and SUV"][, vehicle_type := "Van"],  ## OAS has missing 40t
                dt[region %in% unique(REMIND2region_MAPPING[region %in% c("CAZ"), region]) & vehicle_type == "Motorcycle (>250cc)"][, vehicle_type := "Motorcycle (50-250cc)"],  ## OAS has missing 40t
                dt[region %in% unique(REMIND2region_MAPPING[region %in% c("CAZ"), region]) & vehicle_type == "Motorcycle (>250cc)"][, vehicle_type := "Moped"],  ## OAS has missing 40t
                dt[region %in% unique(REMIND2region_MAPPING[region %in% c("CAZ"), region]) & vehicle_type == "Compact Car"][, vehicle_type := "Mini Car"],
                dt[region %in% unique(REMIND2region_MAPPING[region %in% c("CAZ"), region]) & vehicle_type == "Compact Car"][, vehicle_type := "Subcompact Car"],
                dt[region %in% unique(REMIND2region_MAPPING[region %in% c("SSA"), region]) & vehicle_type == "Compact Car"][, vehicle_type := "Large Car"],
                dt[region %in% unique(REMIND2region_MAPPING[region %in% c("SSA"), region]) & vehicle_type == "Compact Car"][, vehicle_type := "Large Car and SUV"],
                dt[region %in% unique(REMIND2region_MAPPING[region %in% c("SSA"), region]) & vehicle_type == "Compact Car"][, vehicle_type := "Van"],
                dt[region %in% unique(REMIND2region_MAPPING[region %in% c("REF"), region]) & vehicle_type == "Compact Car"][, vehicle_type := "Mini Car"],
                dt[region %in% unique(REMIND2region_MAPPING[region %in% c("SSA"), region]) & vehicle_type == "Motorcycle (50-250cc)"][, vehicle_type := "Moped"],
                dt[region %in% unique(REMIND2region_MAPPING[region %in% c("SSA"), region]) & vehicle_type == "Motorcycle (50-250cc)"][, vehicle_type := "Motorcycle (>250cc)"],
                dt[region %in% unique(REMIND2region_MAPPING[region %in% c("ENC", "NEN", "NES", "UKI"), region]) & vehicle_type == "Compact Car"][, vehicle_type := "Mini Car"])

return(dt)
}
