#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @return a quitte object

toolAdjustSpeedOfModes <- function(dt, completeData) {

  #1: Map data on EDGE-T technologies (for handlability in the model)
  completeData <- completeData[sector == "trn_pass"]
  dt <- merge(dt, completeData, by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "univocalName", "period"), all.y = TRUE, allow.cartesian = TRUE)[, check := NULL]

  #2: Add midsize car for missing regions
  missingMid <- dt[is.na(value) & vehicleType == "Midsize Car"]
  mid <- dt[vehicleType == "Subcompact Car" & region %in% unique(missingMid$region)][, vehicleType := "Midsize Car"][, univocalName := "Midsize Car"]
  dt <- rbind(dt[!(is.na(value) & vehicleType == "Midsize Car")], mid)

  #3: Apply convergence in time to the fastest vehicle across regions
  dt[, maxSpeed := max(value[period == 2100]), by = c("vehicleType", "technology")]
  dt[period >= 2020 & period <= 2100, value := value[period == 2020] * (2100 - period)/(2100 - 2020) + maxSpeed * (period - 2020) / (2100 - 2020), by = c("vehicleType", "technology", "region")]
  dt[period >= 2100, value := maxSpeed]
  dt[, maxSpeed := NULL]

  #4: Speed correction to enhance influence of VOT for 2W (Robert's idea)
  dt[subsectorL3 == "trn_pass_road_LDV_2W", value := value * 0.75]

  dt <- dt[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "univocalName", "variable", "unit", "period", "value")]
  setkey(dt,  region, sector, subsectorL1, subsectorL2, subsectorL3, vehicleType, technology, univocalName, variable, unit, period)

  return(dt)
}
