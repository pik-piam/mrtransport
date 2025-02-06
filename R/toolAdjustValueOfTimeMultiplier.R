#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @param completeData All combinations of region, period, univocalName and technology in EDGE-T decision tree
#' @param filter list of filters for specific branches in the upper decision tree, containing all associated
#' univocalNames
#' @return a quitte object

toolAdjustValueOfTimeMultiplier <- function(dt, completeData, filter) {
  check <- PPPtoMER <- value <- univocalName <- NULL

  #1: Move from a market exchange rate (MER) based expression of the time value multiplier
  # to a purchase power parity (PPP) based one
  PPPtoMER <- magpie2dt(readSource("GCAM", "PPPtoMERfactor"))[, c("region", "value")]
  setnames(PPPtoMER, "value", "PPPtoMER")
  dt <- merge.data.table(dt, PPPtoMER, by = "region", allow.cartesian = TRUE)
  dt[, value := value / PPPtoMER][, PPPtoMER := NULL]

  #2: Map data on EDGE-T structure (to rule out vehicle types that are not present in all countries)
  completeData <- completeData[univocalName %in% filter$trn_pass]
  completeData <- unique(completeData)
  dt <- merge.data.table(dt, completeData, by = c("region", "univocalName", "period"),
                         all.y = TRUE, allow.cartesian = TRUE)
  dt[, check := NULL]

  return(dt)
}
