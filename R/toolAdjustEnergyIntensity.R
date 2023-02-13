#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param magpieobj the input data read via readSource, a magpie object
#' @param sourcetype one of the different EDGE-T inputdata sources
#' @return a quitte object
#'
#' @importFrom rmndt magpie2dt
#' @importFrom data.table fread

toolAdjustEnergyIntensity <- function(dt, isoEUR, TrendsEnIntPSI) {

#To complete dataset and ensure consistency and actuality a number of fixes are applied on the raw source data
browser()
#1: PSI trends in energyIntensity are applied to TRACCS car and trucks data for EUR counrties [TRACCS data only available until 2010]



   TrendsEnIntPSI <- TrendsEnIntPSI[region %in% isoEUR & technology %in% c("Liquids", "NG") &
     vehicleType %in% c("Large Car and SUV", "Compact Car", "Subcompact Car","Midsize Car") & period >= 2010]
   TrendsEnIntPSI[, ratio := value/value[period == 2010], by = c("region", "unit", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName")]
   TrendsEnIntPSI[, value := NULL]
   dt <- merge(dt, TrendsEnIntPSI, all = TRUE, by = c("region", "unit", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName", "period"))
   dt[, value := ifelse(!is.na(ratio), value[period == 2010] * ratio, value)]


}
