#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @return a quitte object

toolAdjustValueOfTimeMultiplier <- function(dt, completeData) {
  
  #1: Apply convergence in time to the fastest vehicle across regions
  dt[, maxSpeed := max(value[period == 2100]), by = c("vehicleType", "technology")]
  dt[period >= 2020 & period <= 2100, value := value[period == 2020] * (2100 - period)/(2100 - 2020) + maxSpeed * (period - 2020) / (2100 - 2020), by = c("vehicleType", "technology", "region")]
  dt[period >= 2100, value := maxSpeed]
  dt[, maxSpeed := NULL]
  
  #2: Speed correction to enhance influence of VOT for 2W (Robert's idea)
  dt[subsectorL3 == "trn_pass_road_LDV_2W", value := value * 0.75]
  
  #3: Add hydrogen aircrafts
  h2aircrafts <- dt[subsectorL1 == "Domestic Aviation"][, technology := "Hydrogen"]
  dt <- rbind(h2aircrafts, dt)
  
  #4: Add alternative Busses
  busFCEV <- dt[subsectorL2 == "Bus"][, technology := "FCEV"]
  busBEV <- dt[subsectorL2 == "Bus"][, technology := "BEV"]
  dt <- rbind(dt, busFCEV, busBEV)
  
  #5: Fill missing data
  completeData <- completeData[sector == "trn_pass"]
  dt <- merge(dt, completeData, all.y = TRUE)
  #5a: Add alternative motorcycles that are missing in some countries
  missingMotBEV <- dt[is.na(value) & vehicleType == "Motorcycle (>250cc)"]
  MotBEV <- dt[vehicleType == "Motorcycle (>250cc)" & technology == "Liquids" & region %in% unique(missingMotBEV$region)][, technology := "BEV"]
  dt <- rbind(dt[!is.na(value) & vehicleType == "Motorcycle (>250cc)"], MotBEV)
  #5b: Add midsize car for missing regions
  missingMid <- dt[is.na(value) & vehicleType == "Midsize Car"]
  mid <- dt[vehicleType == "Subcompact Car" & region %in% unique(missingMid$region)][, vehicleType := "Midsize Car"]
  dt <- rbind(dt[!(is.na(value) & vehicleType == "Midsize Car")], mid)
  
  return(dt)
}