#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @importFrom rmndt magpie2dt
#' @import data.table
#' @return a quitte object

toolAdjustNonVintCAPEX <- function(dt, completeData, ISOcountries, yrs) {

  ## add hydrogen airplanes
  h2Air <- dt[vehicleType == "Domestic Aviation_tmp_vehicletype" & technology == "Liquids"][, technology := "Hydrogen"]
  ## CAPEX of hydrogen airplanes is assumed today 5 times more expensive than a conventional airplane (i.e. not present in the market)
  h2Air[vehicleType %in% "Domestic Aviation_tmp_vehicletype" & period <= 2020,
        value := 5 * value]
  ## following https://www.fch.europa.eu/sites/default/files/FCH%20Docs/20200507_Hydrogen%20Powered%20Aviation%20report_FINAL%20web%20%28ID%208706035%29.pdf
  ## maintenance costs are 50% higher than than a liquids fuelled airplane
  h2Air[vehicleType =="Domestic Aviation_tmp_vehicletype" & period == 2020,
        value := 1.5 * value]
  ## for hydrogen airplanes, in between 2020 and 2040 the cost follows a linear trend, and reaches a value 30% higher than a liquids fuelled airplane
  h2Air[vehicleType == "Domestic Aviation_tmp_vehicletype" & period >= 2020,
        value := ifelse(period <= 2040, value[period == 2020] + (1.3 * value[period == 2100] - value[period == 2020]) * (period - 2020) / (2100 - 2020), 1.3 * value[period == 2100])]
  dt <- rbind(dt, h2Air)

  return(dt)

}
