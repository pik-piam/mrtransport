#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @param sourcetype one of the different EDGE-T inputdata sources
#' @param ariadneAdjustments optional parameter adjustements according to ARIADNE model intercomparison in 2022
#' @return a quitte object

toolAdjustAnnualMileage <- function(dt, completeData, ariadneAdjustments = TRUE) {

#1: Adjustments made by Alois in consequence of the ARIADNE model intercomparison in 2022
  if(ariadneAdjustments){
    ## according to ViZ data from 2020 there has been a 10% reduction wrt 2010 values
    ## (from 14 kkm to 13.6 kkm per vehicle and year)
    dt[region == "DEU" & subsectorL1 == "trn_pass_road_LDV_4W", value := value * 0.9]
  }

#2: Assume missing data
#a) Some modes and technologies are missing an annual mileage
  #TRACCS sets annual mileage for not available technologies in certain countries (e.g. NG or BEVs in early years) to zero.
  #This is not helpful for us. We therefore assign a "hypothetical" annual mileage that can be used for fleet calculation once the technologies
  #get into the mix in later years
  dt[value == 0, value := NA]
  #Non-motorized modes do not get an annual mileage (no fleet tracking possible for walking/not planned for non-motorized cycling)
  completeData <- completeData[!univocalName %in% c("Cycle", "Walk")]
  dt <- merge(completeData, dt, all = TRUE)
  #CompleteData is missing a unit
  dt[, unit := "vehkm/veh/yr"][, check := NULL]
  #Average first within regions over technologies -> e.g. BEV gets the same value as other technologies
  dt[, value := ifelse(is.na(value), mean(value, na.rm = TRUE), value), by = c("region", "period", "vehicleType")]
  #If there are still NAs, average over regions -> e.g. ICE in FRA gets the same value as ICE in DEU
  dt[, value := ifelse(is.na(value), mean(value, na.rm = TRUE), value), by = c("period", "vehicleType", "technology")]

#b) Annual Mileage for Trucks is missing completely - insert assumptions made by Alois in 2022 (probably from ARIADNE)
  annualMileageTrucks <- fread(
    text = "univocalName, annualMileage
              Truck (0-3.5t), 21500
              Truck (7.5t), 34500
              Truck (18t), 53000
              Truck (26t), 74000
              Truck (40t), 136500")
  dt <- merge(dt, annualMileageTrucks, by = "univocalName", all.x = TRUE, allow.cartesian = TRUE)
  dt[, value := ifelse(!is.na(annualMileage), annualMileage, value)][, annualMileage := NULL]

#c) We do not have vintage tracking for the rest of the modes -> insert zeros
   #Later on it would be great to top up this data
  missingAnnualMileageData <- fread(
    text = "univocalName, annualMileage
              International Aviation, 0
              Domestic Aviation, 0
              Passenger Rail, 0
              HSR, 0
              Domestic Ship, 0
              International Ship, 0
              Freight Rail, 0")
  dt <- merge(dt, missingAnnualMileageData, by = "univocalName", all.x = TRUE, allow.cartesian = TRUE)
  dt[, value := ifelse(!is.na(annualMileage), annualMileage, value)][, annualMileage := NULL]

return(dt)
}
