#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @return a quitte object

toolAdjustEsDemand <- function(dt, mapIso2region, completeData) {

  dt <- merge(dt, completeData[period <= 2010], by = c("region", "period", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName"), all = TRUE)
  #completeData does not contain unit so it needs to be added
  dt[sector %in% c("trn_pass", "trn_aviation_intl"), unit := "pkm/yr"]
  dt[sector %in% c("trn_freight", "trn_shipping_intl"), unit := "tkm/yr"]
  dt <- merge(dt, mapIso2region, by = "region", all.x = TRUE, allow.cartesian = TRUE)

  ## Some Truck types, Rail, alternative technologies and active modes are lacking energy service demand data
  ## The missing modes get a zero demand for now. After the regional aggregation, the zero demand remains only for alternative technologies
  missingData <- dt[is.na(value)]
  #The fossil technologies should in theory be all there
  missingFossil <- missingData[!technology %in% c("BEV", "Electric", "FCEV", "Hybrid Electric", "Cycle_tmp_technology", "Walk_tmp_technology", "Hydrogen")]
  dt[is.na(value), value := 0]

  ## add some base demand for Cycle & Walk (2%)
  dt[, demldv := sum(value), by = c("period", "region")]
  dt[subsectorL3 == "Cycle" & value == 0, value := demldv * 0.01]
  dt[subsectorL3 == "Walk" & value == 0, value := demldv * 0.002]
  dt[subsectorL3 == "Cycle" & value == 0 & RegionCode %in% c("USA", "AUS", "CAN"), value := demldv * 0.006]
  dt[subsectorL3 == "Cycle" & value == 0 & RegionCode %in% c("IND", "CHN"), value := demldv * 0.02]
  dt[, demldv := NULL]

  ## from https://www.iea.org/reports/tracking-rail-2020-2
  dt[period <= 2010 & RegionCode == "CHN" & subsectorL3 == "HSR", value := 70000]
  ## from https://theicct.org/sites/default/files/China_Freight_Assessment_English_20181022.pdf
  ## total road freight demand seems to be around 5 billion tkm * 0.8, a factor 3 roughly
  dt[period <= 2010 & RegionCode == "CHN" & subsectorL3 == "trn_freight_road", value := value * 3]

  ## Demand level corrections, adjusting to ETP demands
  dt[RegionCode == "CHA" & subsectorL2 == "Bus", value := value/2.5]
  dt[RegionCode == "IND" & subsectorL2 == "Bus", value := value/2]
  dt[RegionCode == "OAS" & subsectorL2 == "Bus", value := value/5]
  dt[Aggregate21to12Reg == "NEU" & subsectorL2 == "Bus", value := value/2]
  dt[RegionCode == "MEA" & subsectorL2 == "Bus", value := value/2]

  ## Adjust GER Truck size shares according to KBA data (calculated from stocks via AM and LF)
  dt[region == "DEU" & vehicleType == "Truck (0-3.5t)", value := value * 2]
  dt[region == "DEU" & vehicleType == "Truck (7.5t)", value := value * 0.25]
  dt[region == "DEU" & vehicleType == "Truck (18t)", value := value * 0.65]
  dt[region == "DEU" & vehicleType == "Truck (40t)", value := value * 1.4]

  ## Total 2010 Freight demands, from ViZ 2010
  ## (the shares are roughly OK)
  dt[region == "DEU" & sector == "trn_freight", value := value * 620 / 587]
  dt[, c("CountryName", "RegionCode", "Aggregate21to12Reg", "check") := NULL]
  return(dt)
}
