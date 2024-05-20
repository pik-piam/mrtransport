#' Merge source data for historical energy service demand
#' @author Johanna Hoppe
#' @param data source data
#' @param filterEntries helper to filter for univocalNames
#' @param countriesTRACCS countries included in the TRACCS database

toolMergeHistESdemand <- function(data, filterEntries, countriesTRACCS) {
  
  # Calc Energy Service demand based on FE data from EUROSTAT for bunkers
  data$enIntensity[, unit := NULL][, variable := NULL]
  data$loadFactor[, unit := NULL][, variable := NULL]
  setnames(data$enIntensity, "value", "enIntensity")
  setnames(data$loadFactor, "value", "loadFactor")
  esDemandEUROSTAT <- merge.data.table(data$feDemandEUROSTAT, data$enIntensity,
                                       by = c("region", "univocalName", "technology", "period"))
  esDemandEUROSTAT <- merge.data.table(esDemandEUROSTAT, data$loadFactor, by = c("region", "period",
                                                                            "univocalName", "technology"))
  toBillion <- 1e-09
  esDemandEUROSTAT[, value := (value / enIntensity) * loadFactor * toBillion][, c("enIntensity",
                                                                                  "loadFactor") := NULL]
  esDemandEUROSTAT[univocalName %in% c(filterEntries$trn_pass, "International Aviation"), 
                   unit := "billion pkm/yr"]
  esDemandEUROSTAT[univocalName %in% c(filterEntries$trn_freight, "International Ship"), 
                   unit := "billion tkm/yr"]
  esDemandEUROSTAT[, variable := "ES"]
  
  # merge.data.table data
  # TRACCS data is used completely
  # EUROSTAT data is used completely
  # GCAM data is used for regions that are not included in TRACCS, bunkers for regions that are not included
  # in EUROSTAT (non EU-27) and modes that are not included in TRACCS
  # CHE, GBR, ISL, MKD, NOR, TUR are included in TRACCS but not in EUROSTAT
  missingBunkers <- data$esDemandGCAM[region %in% c("CHE", "ISL", "MKD", "NOR", "TUR") &
                                        univocalName %in% c("International Aviation", "Domestic Aviation",
                                                            "Domestic Ship", "International Ship")]
  # GCAM is used for modes not provided by TRACCS for TRACCS regions. 4 Wheelers must be excluded as GCAM
  # uses different vehicle types and bunkers are used from EUROSTAT
  # For some reason energy service demand for Truck(0-3_5t)/Light commercial vehicles 
  # is not reported by TRACCS
  # -> also taken from GCAM
  missingModes <- data$esDemandGCAM[region %in% unique(countriesTRACCS) &
                                      !univocalName %in% unique(data$esDemandTRACCS$univocalName) &
                                      !univocalName %in% c("International Aviation", "Domestic Aviation",
                                                           "Domestic Ship", "International Ship") &
                                      !univocalName %in% filterEntries$trn_pass_road_LDV_4W]
  esDemandRaw <- rbind(
    data$esDemandTRACCS, 
    esDemandEUROSTAT, 
    data$esDemandGCAM[!(region %in% countriesTRACCS)],
    missingBunkers, 
    missingModes
  )
  return(esDemandRaw)
}