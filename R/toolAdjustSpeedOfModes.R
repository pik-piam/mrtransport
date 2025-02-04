#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @param completeData All combinations of region, period, univocalName and technology in EDGE-T decision tree
#' @param filter list of filters for specific branches in the upper decision tree, containing all associated
#' univocalNames
#' @return a quitte object

toolAdjustSpeedOfModes <- function(dt, completeData, filter) {
  check <- value  <- region <- univocalName <- maxSpeed <- period <- NULL

  #1: Map data on EDGE-T structure to identify missing values
  # speed only used for passenger transport modes
  completeData <- completeData[univocalName %in% filter$trn_pass]
  completeData <- unique(completeData)
  dt <- merge.data.table(dt, completeData,
                         by = c("region", "univocalName", "period"),
                         all.y = TRUE, allow.cartesian = TRUE)
  dt[, check := NULL]

  #2: Add midsize car for missing regions
  missingMid <- dt[is.na(value) & univocalName == "Midsize Car"]
  mid <- dt[univocalName == "Subcompact Car" & region %in% unique(missingMid$region)]
  mid[, univocalName := "Midsize Car"]
  dt <- rbind(dt[!(is.na(value) & univocalName == "Midsize Car")], mid)

  #3: Apply convergence in time to the fastest vehicle across regions
  dt[, maxSpeed := max(value[period == 2100]), by = c("univocalName")]
  dt[period >= 2020 & period <= 2100, value := value[period == 2020] * (2100 - period) / (2100 - 2020) + maxSpeed *
       (period - 2020) / (2100 - 2020), by = c("univocalName", "region")]
  dt[period >= 2100, value := maxSpeed]
  dt[, maxSpeed := NULL]

  #4: Speed correction to enhance influence of VOT for 2W (Robert's idea)
  dt[univocalName %in% filter$trn_pass_road_LDV_2W, value := value * 0.75]

  return(dt)
}
