#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @importFrom rmndt magpie2dt
#' @importFrom data.table data.table merge
#' @return a quitte object

toolAdjustNonFuelOPEXtrackedFleet <- function(dt, yrs) {
  BEV <- FCEV <- altCost <- targetYearEarly <- targetYearLate <- NULL
  
  #Data for alternative trucks and busses is missing
  BEV <- dt[subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus" & technology == "Liquids" ][, technology := "BEV"]
  FCEV <- dt[subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus" & technology == "Liquids" ][, technology := "FCEV"]
  altCost <- rbind(BEV, FCEV)
  targetYearEarly <- 2035  ## target year for electric trucks and electric and FCEV buses
  targetYearLate <- 2150  ## target year for FCEV trucks
  
  ## cost of electric truck is 60% more than a as conventional truck today
  altCost[subsectorL1 == "trn_freight_road" & period <= 2020 & technology == "BEV", value := 1.6 * value]
  ## cost of a FCEV truck is 80% more than a as conventional truck today
  altCost[subsectorL1 == "trn_freight_road" & period <= 2020 & technology == "FCEV", value :=  1.8 * value]
  ## cost of electric and H2 buses is 40% more of a conventional bus today
  altCost[subsectorL2 == "Bus" & period <= 2020, value := 1.4 * value]
  
  altCost <- altCost[period <= 2020 | (subsectorL2 == "Bus" & period >= targetYearEarly) | (subsectorL1 == "trn_freight_road" & technology == "BEV" & period >= targetYearEarly) |
                       (subsectorL1 == "trn_freight_road" & technology == "FCEV" & period >= targetYearLate)]
  #follow linear trends until target years/cost parity with ICE cost -> after the target years we assume no further cost decline. This is somehow odd and should be checked
  altCost <- approx_dt(altCost, yrs, "period", "value",
                       c("region", "unit", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "univocalName"), extrapolate = TRUE)
  dt <- rbind(altCost, dt)
  
  return(dt)
}