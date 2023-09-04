#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @param mapIso2region map iso countries to regions
#' @param completeData complete EDGE-T decision tree
#' @return a quitte object

toolAdjustEsDemand <- function(dt, mapIso2region, completeData) {
  variable <- period <- sector <- unit <- value <-  demldv <- subsectorL1 <- regionCode21 <-
    subsectorL2 <- regionCode12 <- region <- vehicleType <- NULL

  dt <- merge.data.table(dt, completeData[period <= 2010],
                         by = c("region", "period", "sector", "subsectorL1", "subsectorL2",
                                "subsectorL3", "vehicleType", "technology", "univocalName"), all = TRUE)
  # completeData does not contain unit so it needs to be added
  dt[sector %in% c("trn_pass", "trn_aviation_intl"), unit := "pkm/yr"]
  dt[sector %in% c("trn_freight", "trn_shipping_intl"), unit := "tkm/yr"]
  dt[is.na(value), variable := "Energy service demand"]
  dt <- merge.data.table(dt, mapIso2region, by = "region", all.x = TRUE, allow.cartesian = TRUE)

  # 1: Some Truck types, Rail, alternative technologies and active modes are lacking energy service demand data
  # The missing modes get a zero demand for now. After the regional aggregation, the zero demand remains only
  # for alternative technologies
  dt[is.na(value), value := 0]

  #2: Add some base demand for Cycle & Walk (2%)
  dt[, demldv := sum(value), by = c("period", "region")]
  dt[subsectorL1 == "Cycle" & value == 0, value := demldv * 0.01]
  dt[subsectorL1 == "Walk" & value == 0, value := demldv * 0.002]
  dt[subsectorL1 == "Cycle" & value == 0 & regionCode21 %in% c("USA", "AUS", "CAN"), value := demldv * 0.006]
  dt[subsectorL1 == "Cycle" & value == 0 & regionCode21 %in% c("IND", "CHN"), value := demldv * 0.02]
  dt[, demldv := NULL]

  #3: Correct demand for CHN
  #from https://www.iea.org/reports/tracking-rail-2020-2
  dt[period <= 2010 & regionCode21 == "CHN" & subsectorL1 == "HSR", value := 70000]
  # from https://theicct.org/sites/default/files/China_Freight_Assessment_English_20181022.pdf
  # total road freight demand seems to be around 5 billion tkm * 0.8, a factor 3 roughly
  dt[period <= 2010 & regionCode21 == "CHN" & subsectorL1 == "trn_freight_road", value := value * 3]

  #4: Demand level corrections, adjusting to ETP demands
  dt[regionCode21 == "CHA" & subsectorL2 == "Bus", value := value / 2.5]
  dt[regionCode21 == "IND" & subsectorL2 == "Bus", value := value / 2]
  dt[regionCode21 == "OAS" & subsectorL2 == "Bus", value := value / 5]
  dt[regionCode12 == "NEU" & subsectorL2 == "Bus", value := value / 2]
  dt[regionCode21 == "MEA" & subsectorL2 == "Bus", value := value / 2]

  #5: Adjust GER Truck size shares according to KBA data (calculated from stocks via AM and LF)
  dt[region == "DEU" & vehicleType == "Truck (0-3.5t)", value := value * 2]
  dt[region == "DEU" & vehicleType == "Truck (7.5t)", value := value * 0.25]
  dt[region == "DEU" & vehicleType == "Truck (18t)", value := value * 0.65]
  dt[region == "DEU" & vehicleType == "Truck (40t)", value := value * 1.4]

  #6: Total 2010 Freight demands, from ViZ 2010
  # (the shares are roughly OK)
  dt[region == "DEU" & sector == "trn_freight", value := value * 620 / 587]
  dt[, c("countryName", "regionCode21", "regionCode12", "check") := NULL]
  return(dt)
}
