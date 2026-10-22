#' Perform parameter specific adjustments on the input data
#'
#' Applies trends, corrections and gap fills to the raw energy intensities (TRACCS, GCAM, PSI) and sets values for
#' single regions and modes.
#'
#' @details India (IND)
#' - Trucks (step 2b): Ratios between classes from published Indian fleet values replace PSI's European ratios
#' - Domestic aviation (step 2c): factor 0.53, so that the harmonized 2010 fuel use equals the IEA's
#' - Cars, two-, three-wheelers and buses (step 2d): Liquids scaled to Indian survey fuel economy in 2010
#' - Code comments give the source and status of each number, with local copies in
#'   /p/projects/edget/adjustmentDataFiles/IND_validation/.
#' - All India-specific pdfs are in .../Input/papers/
#'
#' @author Johanna Hoppe, Jarusch Muessel
#' @param dt calculated raw data without adjustments
#' @param regionTRACCS iso countries included in TRACCS database
#' @param TrendsEnIntPSI Energy intensity trends from PSI
#' @param filter list of filters for specific branches in the upper decision tree, containing all associated
#' univocalNames
#' @param ariadneAdjustments optional parameter adjustements according to ARIADNE model intercomparison in 2022
#'
#' @return a quitte object with the adjusted raw energy intensities in MJ/vehkm, from 2005 on (earlier years are
#' set to the 2005 values in step 5)
#'
#' @importFrom rmndt magpie2dt
#' @import data.table

toolAdjustEnergyIntensity <- function(dt, regionTRACCS, TrendsEnIntPSI, filter, ariadneAdjustments = TRUE) {
  region <- technology <- period <- value  <- region   <- TRACCS <-
    enIntLargeCarSUV <- univocalName <- meanValue <- regionCode21 <- target <- vkmShare <- psi <- scaling <- raw <-
    . <- NULL

  # To complete dataset and ensure consistency and actuality a number of fixes are applied on the raw source data
  # 1: PSI trends in energyitensity are applied to TRACCS car and trucks data [TRACCS data only available until 2010]
  # Calculate PSI factors for the energy intensity improvement compared to 2010
  TrendsEnIntPSI <- TrendsEnIntPSI[region %in% regionTRACCS & !(region == "ALA") &
                                     technology %in% c("Liquids", "Gases") & period >= 2010]
  TrendsEnIntPSI[, factor := value / value[period == 2010],
                 by = c("region", "univocalName", "technology", "variable", "unit")]
  TrendsEnIntPSI[, value := NULL]
  dt <- merge.data.table(dt, TrendsEnIntPSI, all.x = TRUE,
                         by = c("region", "period", "univocalName", "technology", "variable", "unit"))
  # Multiply constant extrapolated 2010 TRACCS values with calculated factor
  dt[, value := ifelse(!is.na(factor), value * factor, value)][, factor := NULL]

  # 2: Adjustments made by Alois in consequence of the ARIADNE model intercomparison in 2022
  if (ariadneAdjustments) {
    dt[region == "DEU" & univocalName %in% filter$trn_pass_road_LDV_4W & technology == "Liquids",
       value := value * 2.4 / 2.85]
    dt[region == "DEU" & univocalName %in% filter$trn_pass_road_LDV_4W & technology == "BEV",
       value := value * 0.77 / 0.94]
    dt[region == "DEU" & univocalName %in% filter$trn_pass_road_LDV_4W & technology == "FCEV",
       value := value * 1.41 / 1.75]
    dt[region == "DEU" & univocalName %in% filter$trn_pass_road_LDV_4W & technology == "Hybrid electric",
       value := value * 1.86 / 1.3]

    ## we apply all constant from 2020 and adjust for DLR improvements (PSI improvements
    ## are calculated on sales only and are thus too ambitious)
    dt[region == "DEU" & univocalName %in% filter$trn_pass_road_LDV_4W & period >= 2020,
       value := .SD[period == 2020]$value, by = c("univocalName", "technology")]

    improvements <- fread("technology, period, factor
        Liquids, 2030, 0.94
        Liquids, 2045, 0.89
        BEV, 2030, 0.94
        BEV, 2045, 0.86
        FCEV, 2030, 0.83
        FCEV, 2045, 0.75")

    dt[region == "DEU" & univocalName %in% filter$trn_pass_road_LDV_4W,
       factor := improvements[.SD, factor, on = c("technology", "period")]]
    dt[region == "DEU" & univocalName %in% filter$trn_pass_road_LDV_4W & period <= 2020, factor := 1]
    dt[region == "DEU" & univocalName %in% filter$trn_pass_road_LDV_4W,
       factor := zoo::na.approx(factor, x = period, rule = 2), by = c("region", "univocalName", "technology")]
    dt[region == "DEU" & univocalName %in% filter$trn_pass_road_LDV_4W, value := value * factor]
    dt[, factor := NULL]

    ## the intensity deviation is likely coming from a deviation in LF and size shares
    dt[region == "DEU" & univocalName %in% filter$trn_freight_road & technology == "Liquids",
       value := value * 1.4 / 1.6]
    dt[region == "DEU" & univocalName %in% filter$trn_freight_road & technology == "BEV",
       value := value * 2.94 / 2.3]
    dt[region == "DEU" & univocalName %in% filter$trn_freight_road & technology == "FCEV",
       value := value * 4.7 / 4.4]
  }

  # 2b: Ratios between the truck classes in India #plausibilityFix_IND #plausibilityFix_Truck #plausibilityFix_Freight
  # 2020 ratios used for all years, Liquids and Gases (keeps PSI's Gases/Liquids ratio); BEV and FCEV unchanged.
  # PSI simulates European trucks, with the wrong ratios for India (0-3_5t / 7_5t: 0.75 against 0.54).
  # Derived: one factor per class, so that the 2010 values have the target ratios while the vkm-weighted mean over
  # the classes stays at the PSI value (assumption; the level is set by the IEA harmonization)
  # Targets in MJ/vehkm, mean of published fleet values for the respective class, one per source:
  # 0-3_5t  2.6: CATF (2022) 1.87 and 2.87 (manufacturer claims de-rated by 20 %), CSTEP (2022) 2.76 and 2.99
  # 7_5t    4.8: TERI (2024) survey 3.99, NITI Aayog and RMI (2021) 6.21, CATF (2022) 4.54, CSTEP (2022) 4.49
  # 18t     6.4: TERI (2024) survey 5.98, CATF (2022) 6.84
  # 26t    10.5: CATF (2022) 8.98, CSTEP (2022) 11.97
  # 40t    11.6: CATF (2022) 11.22, CSTEP (2022) 11.97
  # Weights: 2020 vkm shares 39.1 / 12.6 / 7.4 / 29.2 / 11.7 %, derived (stock share x annual mileage); equal to
  # the model's 2010 vkm shares, as stock shares, load factors and mileages are the same 2020 values
  # Values correspond to: B-EI-01 to 05 in .../energyLoadMileage_bounds.csv and B-VS-01 to 05 in
  # .../fleetComposition_bounds.csv, both in .../IND_validation/data/truck_validation/estimates/
  # Sources, in /p/projects/edget/adjustmentDataFiles/IND_validation/Input/papers/:
  #   TERI (2024), Roadmap for India's Energy Transition in the Transport Sector, Table 4.3 (field survey of goods
  #     vehicles, 2023); SourceID: S01; src07_TERI_2024_energyTransitionRoadmap.pdf
  #   NITI Aayog and RMI (2021), Fast Tracking Freight in India; SourceID: S02;
  #     src02_NITI_RMI_2021_fastTrackingFreight.pdf
  #   CATF (2022), Transport Sector GHG Reduction for India; SourceID: S11;
  #     trucks/T11_CATF_2022_indiaTransportGHG.pdf
  #   CSTEP (2022), The Potential to Electrify Freight Transportation in India; SourceID: S15;
  #     trucks/T15_CSTEP_2022_electrifyFreight.pdf
  trucksIND <- data.table(univocalName = c("Truck (0-3_5t)", "Truck (7_5t)", "Truck (18t)", "Truck (26t)",
                                           "Truck (40t)"),
                          target   = c(2.6, 4.8, 6.4, 10.5, 11.6),
                          vkmShare = c(39.1, 12.6, 7.4, 29.2, 11.7))
  trucksIND <- merge(trucksIND, dt[region == "IND" & period == 2010 & technology == "Liquids",
                                   .(univocalName, psi = value)], by = "univocalName")
  if (nrow(trucksIND) != 5) stop("IND truck energy intensities (PSI, Liquids, 2010) are incomplete")
  trucksIND[, scaling := target * sum(vkmShare * psi) / sum(vkmShare * target) / psi]
  scalingIND <- setNames(trucksIND$scaling, trucksIND$univocalName)
  dt[region == "IND" & univocalName %in% names(scalingIND) & technology %in% c("Liquids", "Gases"),
     value := value * scalingIND[univocalName]]

  # 2c: Domestic aviation in India #plausibilityFix_IND #plausibilityFix_Pass
  # Derived factor: 0.53 for all years, Liquids (before 3a, so Liquids & hydrogen aircrafts keep their ratio)
  # GCAM's intensity (3.23 MJ/pkm in 2010) multiplied with 44 billion pkm (published: DGCA via TERI (2024),
  # Figure 2.4; set in toolAdjustEsDemand()) gives 1.56 times the IEA's fuel use, 1.8 times after the harmonization,
  # which took fuel from road.
  # The harmonization scales all IND liquids modes by one factor, so it keeps each mode's share of the
  # FE; domestic aviation's share must therefore equal its IEA share (2010):
  # factor = (IEA_domAv / FE_domAv) x (FE_other / (IEA_total - IEA_domAv)) = (0.091 / 0.142) x (2.091 / 2.523) = 0.53
  # IEA_domAv 0.091 EJ (flow DOMESAIR), IEA_total 2.614 EJ (liquids without bunkers)
  # FE_domAv = 3.23 MJ/pkm x 44 billion pkm
  # FE_other: raw FE of all other IND liquids modes sum(ES x intensity / LF), also built in
  # toolAdjustEsDemand() (step 7d) and used for the harmonization
  # -> harmonization factor 1.21, harmonized intensity 2.1 MJ/pkm
  # Sources:
  #   TERI (2024), Roadmap for India's Energy Transition in the Transport Sector; SourceID: S01;
  #     /p/projects/edget/adjustmentDataFiles/IND_validation/Input/papers/src07_TERI_2024_energyTransitionRoadmap.pdf
  #   IEA World Energy Balances 2024; /p/projects/rd3mod/inputdata/sources/IEA/IEA-Energy-Balances-2024/worldbig.csv
  #     (via calcOutput("IEAOutputTransport"))
  dt[region == "IND" & univocalName == "Domestic Aviation" & technology == "Liquids", value := value * 0.53]

  # 2d: Cars, two-wheelers, three-wheelers and buses in India #plausibilityFix_IND #plausibilityFix_Pass
  # 2023 survey values used for all years, Liquids only (CNG, hybrids and BEV unchanged)
  # GCAM's 2010 values (vkm-weighted) are above Indian fuel economy: cars 2.9 MJ/vehkm, 2W 0.72, 3W 1.48; buses
  # 4.97 are below.
  # Derived: one factor per group, so that 2010 vkm-weighted mean equal = target value; ratios between car classes
  # and the trend over time come from GCAM. Weights: 2010 vkm shares.
  # Targets in MJ/vehkm = energy density (MJ/l) / fuel economy (km/l)
  # Energy density = net calorific value (published: IPCC (2006)) x density (published: IS 2796, IS 1460):
  # petrol 32.0 MJ/l = 44.3 MJ/kg x 0.72 kg/l
  # diesel 35.9 MJ/l = 43.0 MJ/kg x 0.835 kg/l
  # Fuel economy published: TERI (2024), Table 4.2 (self-reported, possibly optimistic):
  # Cars  1.9:  petrol 19 km/l (1.68), diesel 17 km/l (2.11), equal weights (assumption)
  # 2W    0.63: petrol 51 km/l
  # 3W    1.39: petrol 23, diesel 26 km/l
  # Bus   7.7:  4.67 km/l (published: LBNL and ICCT (2017), Table 9, 16.2 t transit bus)
  # Bus cross-check: APSRTC fleet 5.2 km/l (6.9 MJ/vehkm; Shakti (2017)); 25 t buses 9.7 MJ/vehkm
  # Sources, in /p/projects/edget/adjustmentDataFiles/IND_validation/Input/papers/:
  #   TERI (2024), Roadmap for India's Energy Transition in the Transport Sector, Table 4.2 (field survey of passenger
  #     vehicles, 2023); SourceID: S01; src07_TERI_2024_energyTransitionRoadmap.pdf
  #   LBNL and ICCT (2017), Improved heavy-duty vehicle fuel efficiency in India; SourceID: S24;
  #     trucks/T24_LBNL_ICCT_2017_HDVfuelEfficiency.pdf
  #   Shakti (2017), Improving bus fuel efficiency through fleet inspection and maintenance practices;
  #     SourceID: S25; trucks/T25_Shakti_2017_busFuelEfficiency.pdf
  #   No local copy: IPCC (2006), 2006 IPCC Guidelines for National Greenhouse Gas Inventories, Vol. 2, Ch. 1,
  #     Table 1.2 (p. 1.18); BIS, IS 2796:2017 (petrol) and IS 1460:2017 (diesel), BS VI, density limits as listed
  #     in HPCL's MS/HSD BS-VI specification sheet
  passIND <- data.table(univocalName = c("Mini Car", "Subcompact Car", "Compact Car", "Large Car and SUV", "Moped",
                                         "Motorcycle (50-250cc)", "Rickshaw", "Bus"),
                        group    = c("cars", "cars", "cars", "cars", "2W", "2W", "3W", "bus"),
                        target   = c(1.9, 1.9, 1.9, 1.9, 0.63, 0.63, 1.39, 7.7),
                        vkmShare = c(23.9, 51.7, 9.9, 14.5, 16.6, 83.4, 100, 100))
  passIND <- merge(passIND, dt[region == "IND" & period == 2010 & technology == "Liquids",
                               .(univocalName, raw = value)], by = "univocalName")
  if (nrow(passIND) != 8) stop("IND passenger road energy intensities (Liquids, 2010) are incomplete")
  passIND[, scaling := target * sum(vkmShare) / sum(vkmShare * raw), by = "group"]
  scalingPassIND <- setNames(passIND$scaling, passIND$univocalName)
  dt[region == "IND" & univocalName %in% names(scalingPassIND) & technology == "Liquids",
     value := value * scalingPassIND[univocalName]]

  # 3: Assume missing data
  # a) Hydrogen airplanes are not covered by our datasources
  # based on "A review on potential use of hydrogen in aviation applications", Dincer, 2016:
  # the energy intensity of a hydrogen airplane is around 1MJ/pkm. The range of energy intensity of a
  # fossil-based airplane is here around 3-2 MJ/pkm-> a factor of 0.5 is assumed
  dt <- rbind(dt, dt[univocalName == "Domestic Aviation"][, c("value", "technology") :=
                                                            list(0.5 * value, "Hydrogen")])

  # b) Alternative technologies for Busses are not covered by our sources
  ## Buses are assumed to have the same energy intensity as 18 tons truck
  # IND cross-check: BEV 3.4 MJ/vehkm; CEEW (2025c) Table 2: electric bus 1.0 km/kWh = 3.6 MJ/vehkm (see 3g).
  alternativeBusTech <- dt[univocalName == "Truck (18t)" & technology %in% c("BEV", "FCEV")]
  alternativeBusTech[, univocalName := "Bus"]
  dt <- rbind(dt, alternativeBusTech)

  # c) Some regions are missing electric motorcycles (>250CC)
  BEVmot <- dt[univocalName == "Motorcycle (>250cc)" & technology == "BEV"]
  BEVmotAll <- dt[univocalName == "Motorcycle (>250cc)"]
  BEVmotAll <- unique(BEVmotAll[, c("technology", "value") := NULL])[, technology := "BEV"]
  BEVmotAll <- merge.data.table(BEVmotAll, BEVmot, by = c("region", "period", "univocalName", "technology",
                                                          "variable", "unit"), all = TRUE)
  BEVmotAll[, TRACCS := ifelse(region %in% regionTRACCS, "TRACCS", "nonTRACCS")]
  # NAs (missing data) is filled by averages of other countries
  BEVmotAll <- BEVmotAll[, mean := lapply(.SD, mean, na.rm = TRUE), .SDcols = "value",
                         by = c("period", "univocalName", "technology", "TRACCS")]
  BEVmotAll[is.na(value), value := mean][, c("TRACCS", "mean") := NULL]
  dt <- rbind(dt[!(univocalName == "Motorcycle (>250cc)" & technology == "BEV")], BEVmotAll)

  # d) Energy intensity for large cars is not provided by PSI, hence some nonTRACCS countries are missing
  # alternative technologies for Large cars
  enIntLargeCar <- unique(dt[univocalName == "Large Car and SUV",
                             c("region", "period", "technology", "value")])
  setnames(enIntLargeCar, "value", "enIntLargeCarSUV")
  dt4W <- merge.data.table(dt[univocalName %in% filter$trn_pass_road_LDV_4W], enIntLargeCar,
                           by = c("region", "period", "technology"), all.x = TRUE)
  dt4W[is.na(value) & univocalName == "Large Car", value := enIntLargeCarSUV * 0.9][, enIntLargeCarSUV := NULL]
  dt <- rbind(dt[!univocalName %in% filter$trn_pass_road_LDV_4W], dt4W)

  # e) Some non TRACCS countries ("ARE" "BHR" "IRN" "IRQ" "ISR" "JOR" "KWT" "LBN" "OMN" "PSE" "QAT" "SAU" "SYR" "YEM")
  # are missing data for Freight and Passenger Rail running on liquids. Average of other non TRACCS countries
  # is taken
  missingRailData <- dt[region %in% c("ARE", "BHR", "IRN", "IRQ", "ISR", "JOR", "KWT", "LBN", "OMN", "PSE", "QAT",
                                      "SAU", "SYR", "YEM")                        &
                          univocalName %in% c("Freight Rail", "Passenger Rail") & technology == "Electric"]
  missingRailData[, technology := "Liquids"][, value := NA]
  dt <- rbind(dt, missingRailData)
  dt[, TRACCS := ifelse(region %in% regionTRACCS, "TRACCS", "nonTRACCS")]
  dt[univocalName %in% c("Freight Rail", "Passenger Rail") & technology == "Liquids",
     mean := lapply(.SD, mean, na.rm = TRUE), .SDcols = "value",
     by = c("period", "univocalName", "technology", "TRACCS")]
  dt[univocalName %in% c("Freight Rail", "Passenger Rail") & technology == "Liquids" & is.na(value), value := mean]
  dt[, c("TRACCS", "mean") := NULL]

  # f) IDN (Indonesia) is missing electric Passenger Rail. It is assumed that it is the same as in Malaysia (MYS)
  missingPassRailDataIDN <- dt[region %in% c("MYS") & univocalName == "Passenger Rail" & technology == "Electric"]
  missingPassRailDataIDN[, region := "IDN"]
  dt <- rbind(dt, missingPassRailDataIDN)

  # g) Alternative technologies for Rickshaws are not covered by our sources
  # Rickshaws are assumed to have the same as Motorcycle (50-250cc) BEV
  # IND cross-check: 0.21 MJ/vehkm (2010); CEEW (2025c) Table 2: electric passenger three-wheeler 15 km/kWh =
  # 0.24 MJ/vehkm; 14% difference,kept for now
  # SourceID: S26; .../Input/papers/trucks/T26_CEEW_2025_vehicleOwnershipCosts.pdf)
  alternativeRickshawTech <- dt[univocalName == "Motorcycle (50-250cc)" &
                                  technology %in% c("BEV") & region %in% dt[univocalName == "Rickshaw"]$region]
  alternativeRickshawTech[, univocalName := "Rickshaw"]
  dt <- rbind(dt, alternativeRickshawTech)

  # 4: adjustments for scenarioMIP validation: adjust outliers to global mean
  ISOcountriesMap <- system.file("extdata", "regionmappingISOto21to12.csv", package = "mrtransport", mustWork = TRUE)
  ISOcountriesMap <- fread(ISOcountriesMap, skip = 0)
  dt[, meanValue := mean(value, na.rm = TRUE), by = c("univocalName", "technology", "period")]
  dt[region %in% ISOcountriesMap[regionCode21 == "LAM"]$countryCode & univocalName %in% c("Compact Car") &
       technology == "Gases", value := meanValue]

  dt[, meanValue := NULL]

  # 5: data until 2005 is fixed to not impact changes in lifetime in the fleet calculation (in edgeT)
  xdata <- unique(dt$period)
  dt <- dt[period >= 2005]
  dt <- rmndt::approx_dt(dt, xdata, "period", "value",   extrapolate = TRUE)

  return(dt)
}
