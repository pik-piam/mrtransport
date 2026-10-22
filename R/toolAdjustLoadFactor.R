#' Perform parameter specific adjustments on the input data
#'
#' Corrects and fills gaps in the raw load factors (TRACCS, GCAM) and sets values for single regions and modes.
#'
#' @details India (IND)
#' - Trucks (step 4): Payloads net of empty running from Indian survey data replace the global values
#' - Passenger three-wheelers ("Rickshaw", step 5): 2.5 passengers, survey occupancy of TERI minus an assumed share
#'   of empty driving
#' - Code comments give the source and status of each number, with local copies in
#'   /p/projects/edget/adjustmentDataFiles/IND_validation/.
#' - All India-specific pdfs are in .../Input/papers/
#'
#' @author Johanna Hoppe, Jarusch Muessel
#' @param dt calculated raw data without adjustments
#' @param completeData All combinations of region, period, univocalName and technology in EDGE-T decision tree
#' @param TRACCScountries countries included in TRACCS database
#' @param filter list of filters for specific branches in the upper decision tree, containing all associated
#' univocalNames
#' @return a quitte object with the adjusted load factors in p/veh (passenger modes) and t/veh (freight modes)

toolAdjustLoadFactor <- function(dt, completeData, TRACCScountries, filter) {
  period <- value <- region <- univocalName <- univocalName <- check <- unit  <- variable <-
    meanValue <- regionCode21 <- NULL

  #1: Correct unrealisitc data
  #a) 3_5t load factor as provided by GCAM is unrealistically high
  # IND: overwritten in step 4 (all technologies and years).
  dt[!(region %in% TRACCScountries) & univocalName == "Truck (0-3_5t)", value := 0.4]

  #b) 40t dt in TRACCS is too low, using KBA data (2019 dataset)
  # https://www.kba.de/DE/Statistik/Produktkatalog/produkte/Kraftverkehr/vd3_uebersicht.html?nn=3514348
  dt[region == "DEU" & univocalName == "Truck (40t)", value := 11.5]

  #2: Assume missing data
  # Non-motorized modes do not get a loadFactor
  completeData <- completeData[!univocalName %in% c("Cycle", "Walk")]
  dt <- merge.data.table(completeData, dt, all = TRUE)
  dt[, check := NULL]
  #Average first within regions over technologies -> e.g. BEV gets the same value as other technologies
  dt[, value := ifelse(is.na(value), mean(value, na.rm = TRUE), value), by = c("region", "period", "univocalName")]
  #If there are still NAs, average over regions -> e.g. ICE in FRA gets the same value as ICE in DEU
  dt[, value := ifelse(is.na(value), mean(value, na.rm = TRUE), value), by = c("period", "univocalName", "technology")]
  #Add unit and variable for filled values
  dt[is.na(unit), unit := ifelse(univocalName %in% filter$trn_pass, "p/veh", "t/veh")]
  dt[is.na(variable), variable := "Load factor"]

  # data until 2010 has weird spikes for some regions
  # -> remove data between 1991 and 2009 and interpolate afterward to remove the spikes
  xdata <- unique(dt$period)
  dt <- dt[period == 1990 | period > 2010]
  dt <- rmndt::approx_dt(dt, xdata, "period", "value")

  # 3: adjustments for scenarioMIP validation adjust outliers to global mean
  ISOcountriesMap <- system.file("extdata", "regionmappingISOto21to12.csv", package = "mrtransport", mustWork = TRUE)
  ISOcountriesMap <- fread(ISOcountriesMap, skip = 0)
  dt[, meanValue := mean(value, na.rm = TRUE), by = c("univocalName", "technology", "period")]
  dt[region %in% ISOcountriesMap[regionCode21 == "CAZ"]$countryCode & univocalName %in% c("Truck (7_5t)"),
     value := meanValue]
  dt[, meanValue := NULL]

  # 4: Trucks in India #plausibilityFix_IND #plausibilityFix_Truck #plausibilityFix_Freight
  # Set after the regional means in step 3, so that they do not change other regions.
  # 2020 values used for all years and technologies.
  # Derived: payloads net of empty running, loaded payload x (1 - empty running share); empty running 28.5 % for
  # TERI's LGV, 26.5 % in GIZ and TERI (2022), so x 0.735:
  # 0-3_5t  0.7 t: TERI (2024) intra-city loaded LGV payload (1 t) x (1 - 0.285)
  # 7_5t    5.0 t: TERI (2024) survey MGV 7.5-12 t (6 t net), lowered for the 3.5-7.5 t trucks in the class
  #                (assumption)
  # 18t     8.7 t: mean of TERI (2024) survey 12-16.2 t (10 t net) and GIZ and TERI (2022) 12-16 t (10 t x 0.735)
  # 26t    15.0 t: mean of TERI (2024) survey (16.2-25 t: 13 t; above 25 t: 19 t, net) and GIZ and TERI (2022)
  #                (16-25 t: 10.3 t; 25-40 t: 18.4 t, net)
  # 40t    19.0 t: TERI (2024) survey above 25 t (19 t net) and GIZ and TERI (2022) above 40 t (27 t x 0.735)
  # Values correspond to: B-LF-01 to 05 in
  # .../IND_validation/data/truck_validation/estimates/energyLoadMileage_bounds.csv
  # Sources, in /p/projects/edget/adjustmentDataFiles/IND_validation/Input/papers/:
  #   TERI (2024), Roadmap for India's Energy Transition in the Transport Sector, Table 4.3 (field survey of goods
  #     vehicles, 2023); SourceID: S01; src07_TERI_2024_energyTransitionRoadmap.pdf
  #   GIZ and TERI (2022), Freight Greenhouse Gas Calculator, methodology report; SourceID: S14;
  #     trucks/T14_GIZ_TERI_2022_freightGHGcalculator.pdf
  loadFactorIND <- c("Truck (0-3_5t)" = 0.7, "Truck (7_5t)" = 5.0, "Truck (18t)" = 8.7,
                     "Truck (26t)" = 15.0, "Truck (40t)" = 19.0)
  dt[region == "IND" & univocalName %in% names(loadFactorIND), value := loadFactorIND[univocalName]]

  # 5: Passenger three-wheelers in India #plausibilityFix_IND #plausibilityFix_Pass
  # Set after the regional means in step 3, so that they do not change other regions.
  # 2.5 passengers for all technologies and years
  # The survey counts passengers on trips with passengers; the LF counts passengers per vkm over all vkm.
  # Derived: LF = survey occupancy x (1 - empty driving share)
  # 3.3 passengers (published: TERI (2024), Table 4.2, survey 2023; 3 within cities, 4 between cities)
  # 25 % empty driving between fares (assumption: 20-30 %, to be confirmed)
  # stock = pkm / (LF x AM) in 2010 is ca. 3.3 million with 2.5, 4.5 million with GCAM's 1.88 (published: 2.5
  # million, TERI (2024); 4 million, CEEW (2025b))
  # Sources, in /p/projects/edget/adjustmentDataFiles/IND_validation/Input/papers/:
  #   TERI (2024), Roadmap for India's Energy Transition in the Transport Sector; SourceID: S01;
  #     src07_TERI_2024_energyTransitionRoadmap.pdf
  #   CEEW (2025b), How Will India's Vehicle Ownership Grow? A District-level Outlook to 2050; SourceID: S20;
  #     src01_CEEW_2025_vehicleOwnership.pdf
  dt[region == "IND" & univocalName == "Rickshaw", value := 2.5]

  return(dt)
}
