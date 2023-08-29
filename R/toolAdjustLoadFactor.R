#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @param completeData complete EDGE-T decision tree
#' @param TRACCScountries countries included in TRACCS database
#' @return a quitte object

toolAdjustLoadFactor <- function(dt, completeData, TRACCScountries) {
 value <- region <- vehicleType <- univocalName <- check <- unit <- sector <- variable <- NULL

#1: Correct unrealisitc data
#a) 3.5t load factor as provided by GCAM is unrealistically high
  dt[!(region %in% TRACCScountries) & vehicleType == "Truck (0-3.5t)", value := 0.4]

#b) 40t dt in TRACCS is too low, using KBA data (2019 dataset)
  # https://www.kba.de/DE/Statistik/Produktkatalog/produkte/Kraftverkehr/vd3_uebersicht.html?nn=3514348
  dt[region == "DEU" & vehicleType == "Truck (40t)", value := 11.5]

#2: Assume missing data
  # Non-motorized modes do not get a loadFactor
  completeData <- completeData[!univocalName %in% c("Cycle", "Walk")]
  dt <- merge.data.table(completeData, dt, all = TRUE)
  dt[, check := NULL]
  #Average first within regions over technologies -> e.g. BEV gets the same value as other technologies
  dt[, value := ifelse(is.na(value), mean(value, na.rm = TRUE), value), by = c("region", "period", "vehicleType")]
  #If there are still NAs, average over regions -> e.g. ICE in FRA gets the same value as ICE in DEU
  dt[, value := ifelse(is.na(value), mean(value, na.rm = TRUE), value), by = c("period", "vehicleType", "technology")]
  #Add unit and variable for filled values
  dt[is.na(unit), unit := ifelse(sector == "trn_pass", "p/veh", "t/veh")]
  dt[is.na(variable), variable := "Load factor"]
return(dt)
}
