#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @param TRACCScountries countries included in TRACCS dataset
#' @return a quitte object

toolAdjustLoadFactor <- function(dt, completeData, TRACCScountries) {

#1: Correct unrealisitc data
#a) 3.5t load factor as provided by GCAM is unrealistically high
  dt[!(region %in% TRACCScountries) & vehicleType == "Truck (0-3.5t)", value := 0.4]

#b) 40t dt in TRACCS is too low, using KBA data (2019 dataset)
#   https://www.kba.de/DE/Statistik/Produktkatalog/produkte/Kraftverkehr/vd3_uebersicht.html?nn=3514348
  dt[region == "DEU" & vehicleType == "Truck (40t)", value := 11.5]

#2: Assume missing data
  #Non-motorized modes do not get a loadFactor
  completeData <- completeData[!univocalName %in% c("Cycle", "Walk")]
  dt <- merge(completeData, dt, all = TRUE)
  #CompleteData is missing a unit
  dt[, unit := "(t|p)/veh"][, check := NULL]
  #Average first within regions over technologies -> e.g. BEV gets the same value as other technologies
  dt[, value := ifelse(is.na(value), mean(value, na.rm = TRUE), value), by = c("region", "period", "vehicleType")]
  #If there are still NAs, average over regions -> e.g. ICE in FRA gets the same value as ICE in DEU
  dt[, value := ifelse(is.na(value), mean(value, na.rm = TRUE), value), by = c("period", "vehicleType", "technology")]

return(dt)
}
