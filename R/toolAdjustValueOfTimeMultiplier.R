#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @return a quitte object

toolAdjustValueOfTimeMultiplier <- function(dt, completeData) {

  #1: Move from a market exchange rate (MER) based expression of the time value multiplier to a purchase power parity (PPP) based one
  PPPtoMER <- magpie2dt(readSource("GCAM", "PPPtoMERfactor"))[, c("region", "value")]
  setnames(PPPtoMER, "value", "PPPtoMER")
  dt <- merge(dt, PPPtoMER, by = "region", allow.cartesian = TRUE)
  dt[, value := value/PPPtoMER][, PPPtoMER := NULL]

  #2: Map data on EDGE-T technologies (for handlability in the model)
  completeData <- completeData[sector == "trn_pass"]
  dt <- merge(dt, completeData, by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "univocalName", "period"), all.y = TRUE,
              allow.cartesian = TRUE) [, check := NULL]

  dt <- dt[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "univocalName", "variable", "unit", "period", "value")]
  setkey(dt,  region, sector, subsectorL1, subsectorL2, subsectorL3, vehicleType, technology, univocalName, variable, unit, period)
  
  return(dt)
}
