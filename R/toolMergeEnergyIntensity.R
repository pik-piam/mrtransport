#' Merge source data for energy intensity
#' @author Johanna Hoppe
#' @param data source data
#' @param filterEntries helper to filter for univocalNames
#' @param countriesTRACCS countries included in the TRACCS database

toolMergeEnergyIntensity <- function(data, filterEntries, countriesTRACCS) {

  # merge.data.table data
  # TRACCS>PSI>GCAM
  # 1: TRACCS data
  # Used parts of TRACCS energy Intensity: TRACCS data is used completely except for two wheelers

  # 2: GCAM data
  # Used parts of the GCAM energy Intensity:
  # Conventional cars (Liquids, NG) in non-TRACCS countries
  energyIntensityRawGCAMconventionalCarsnonTRACCS <- data$enIntGCAM[univocalName %in% filterEntries$trn_pass_road_LDV_4W &
                                                                      technology %in% c("Liquids", "Gases") &
                                                                      !region %in% countriesTRACCS]

   # All other data for non-TRACCS countries except for Trucks
  energyIntensityRawGCAMnonCarsnonTRACCS <- data$enIntGCAM[!univocalName %in% filterEntries$trn_pass_road_LDV_4W &
                                                             !univocalName %in% filterEntries$trn_freight_road &
                                                             !region %in% countriesTRACCS]
  # Energy Intensity data for Freight Rail, Passenger Rail, HSR, Domestic Aviation, International Aviation,
  # Domestic Shipping, International Shipping is not provided by TRACCS. Hence GCAM data is used for all countries
  energyIntensityRawGCAMmissingTRACCScat <- data$enIntGCAM[univocalName %in% c("Freight Rail", "Passenger Rail",
                                                                               "HSR", "Domestic Aviation",
                                                                               "International Aviation",
                                                                               "Domestic Ship",
                                                                               "International Ship") &
                                                             region %in% countriesTRACCS]
  # Alternative technologies for motorcycles are missing in the TRACCS database and are taken from GCAM.
  # Furthermore, in TRACCS energy intensity for motorcycles and mopeds are only reported until 2010
  # and would be constant when interpolating later years.
  # Hence we use GCAM data for mopeds and motorcyclse adn there alternatives also for TRACCS countries
  energyIntensityRawGCAM2WheelersTRACCSreg <- data$enIntGCAM[univocalName %in% filterEntries$trn_pass_road_LDV_2W &
                                                               region %in% countriesTRACCS]


  #3: PSI data
  # Used for Trucks in non-TRACCS countries
  energyIntensityRawPSITrucks <- data$enIntPSI[univocalName %in% filterEntries$trn_freight_road
                                               & !region %in% countriesTRACCS]
  # TRACCS data does not include NG Truck (7.5t), Truck (18t), Truck (26t), Truck (40t) -> data is taken from PSI
  # TRACCS data does not include NG Truck (7_5t), Truck (18t), Truck (26t), Truck (40t) -> data is taken from PSI
  energyIntensityRawPSItrucksNGTRACCSreg <- data$enIntPSI[univocalName %in% c("Truck (7_5t)", "Truck (18t)",
                                                                              "Truck (26t)", "Truck (40t)") &
                                                            technology == "Gases" & region %in% countriesTRACCS]
  # Used for alternative Cars (BEV,FCEV,HEV) in TRACCS countries
  energyIntensityRawPSIalternativeTechTRACCSreg <- data$enIntPSI[technology %in% c("BEV", "FCEV", "Hybrid electric")
                                                                 & region %in% countriesTRACCS]
  # Use only data for vehicle types that are listed in the TRACCS data base
  TRACCSVehTypes <- copy(data$enIntTRACCS)
  TRACCSVehTypes <- unique(TRACCSVehTypes[, c("value", "technology") := NULL])
  # Apply only on the vehicle types that are in general available from the PSI dataset
  # (2 Wheelers and Busses are not provided by PSI)
  TRACCSVehTypes <- TRACCSVehTypes[univocalName %in% unique(energyIntensityRawPSIalternativeTechTRACCSreg
                                                            $univocalName)]
  energyIntensityRawPSIalternativeTechTRACCSreg <- merge.data.table(energyIntensityRawPSIalternativeTechTRACCSreg,
                                                                    TRACCSVehTypes, all.y = TRUE,
                                                                    by = c("region", "univocalName",
                                                                           "variable", "unit", "period"))
  # Used for alternative Cars (BEV,FCEV,HEV) in non-TRACCS countries
  # For non TRACCS iso countries the available vehicle types differ.
  # Use the additional data on alternative Cars only for the existing vehicle types in GCAM
  energyIntensityRawPSIalternativeCarsnonTRACCS <- data$enIntPSI[technology %in% c("BEV", "FCEV",
                                                                                   "Hybrid electric") &
                                                                   !region %in% countriesTRACCS]
  # Create structure for GCAM vehicle types and alternative tech options
  GCAMVehTypes <- energyIntensityRawGCAMconventionalCarsnonTRACCS[univocalName
                                                                  %in% filterEntries$trn_pass_road_LDV_4W &
                                                                    !region %in% countriesTRACCS]
  GCAMVehTypes <- unique(GCAMVehTypes[, c("value", "technology", "variable", "unit") := NULL])[, altTech := 1]
  AltTechOpt <- data.table(technology = c("BEV", "FCEV", "Hybrid electric"), altTech = c(1, 1, 1))
  GCAMVehTypes <- merge.data.table(GCAMVehTypes, AltTechOpt, by = "altTech", allow.cartesian = TRUE)
  GCAMVehTypes[, altTech := NULL]
  energyIntensityRawPSIalternativeCarsnonTRACCS <-
    merge.data.table(energyIntensityRawPSIalternativeCarsnonTRACCS,
                     GCAMVehTypes, all.y = TRUE, by = c("region",
                                                        "period",
                                                        "univocalName",
                                                        "technology"))
  # Large Car is missing for some nonTRACCS regions, filled in toolAdjustEnergyIntensity
  # -> NAs are introduced for unit & variable
  energyIntensityRawPSIalternativeCarsnonTRACCS[, variable := "Energy intensity"][, unit := "MJ/vehkm"]

  energyIntensityRaw <- rbind(data$enIntTRACCS[!univocalName %in% filterEntries$trn_pass_road_LDV_2W],
                              energyIntensityRawGCAMconventionalCarsnonTRACCS,
                              energyIntensityRawGCAMmissingTRACCScat,
                              energyIntensityRawGCAMnonCarsnonTRACCS,
                              energyIntensityRawGCAM2WheelersTRACCSreg,
                              energyIntensityRawPSITrucks,
                              energyIntensityRawPSItrucksNGTRACCSreg,
                              energyIntensityRawPSIalternativeTechTRACCSreg,
                              energyIntensityRawPSIalternativeCarsnonTRACCS)
  return(energyIntensityRaw)
}
