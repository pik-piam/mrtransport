#' Perform parameter specific adjustments on the input data
#'
#' Fills gaps in the raw annual mileage (TRACCS, UCD) and sets values for single regions and modes.
#'
#' @details India (IND)
#' - Trucks (step 2c): Means of published Indian fleet values replace previous specific assumptions and global
#'   benchmarks (2b)
#' - Cars (after step 3a): 13,500 km/yr for all car classes, derived from 36,500 pkm per car and year and 2.7
#'   passengers per car. Previous estimates resulted in 2010 car stock well above literature values.
#' - Passenger three-wheelers ("Rickshaw", step 2a): 26,200 km/yr, mean of TERI & CEEW
#' - Code comments give the source of each number, with multiple resources in
#'   /p/projects/edget/adjustmentDataFiles/IND_validation/.
#' - and local copies of pdfs in .../Input/papers/
#'
#' @author Johanna Hoppe, Jarusch Muessel
#' @param dt calculated raw data without adjustments
#' @param completeData All combinations of region, period, univocalName and technology in EDGE-T decision tree
#' @param filter list of filters for specific branches in the upper decision tree, containing all associated
#' univocalNames
#' @param ariadneAdjustments switch on and off adjustments according to ARIADNE model intercomparison in 2022
#' @return a quitte object with the adjusted annual mileage in vehicle-km per vehicle and year

toolAdjustAnnualMileage <- function(dt, completeData, filter, ariadneAdjustments = TRUE) {
  region <- value <- univocalName <- check <- unit <- variable <- annualMileage <- period <- technology <- meanValue <-
    regionCode21 <- regionCode12 <- . <- NULL

  ISOcountriesMap <- system.file("extdata", "regionmappingISOto21to12.csv", package = "mrtransport", mustWork = TRUE)
  ISOcountriesMap <- fread(ISOcountriesMap, skip = 0)

  # 1: Adjustments made by Johanna in consequence of the ARIADNE model intercomparison in 2026:
  # Introducing an annual mileage reduction due to the covid pandemic for EUR countries to match rising vehicle stock
  # reported by EU pocketbook data even with demand dip.
  # source that documents annual mileage dip due to covid-pandemic: Odyssee-Mure "after a sharp decrease in 2020 in
  # most countries (-13% at EU level)"
  # For now we do not assume a mileage recovery in the years after 2020 (as reported by Odyssee-Mure), because we
  # do no cover the energy service demand
  # dynamics yet sufficiently (increases again after 2022).
  # To get closer to the reported vehicle stock increase (EU pocket book data) we keep the annual mileage reduction
  if (ariadneAdjustments) {
    dt[period >= 2020 & region %in% ISOcountriesMap[regionCode12 == "EUR"
       ]$countryCode & univocalName %in% filter$trn_pass_road_LDV_4W, value := value * 0.87]
  }
  # 2: Assume missing data
  # a) Some modes and technologies are missing an annual mileage
  # TRACCS sets annual mileage for not available technologies in certain countries
  # (e.g. NG or BEVs in early years) to zero.
  # This is not helpful for us. We therefore assign a "hypothetical" annual mileage that
  # can be used for fleet calculation once the technologies get into the mix in later years
  dt[value == 0, value := NA]
  # Non-motorized modes do not get an annual mileage (no fleet tracking possible for walking/
  # not planned for non-motorized cycling)
  completeData <- completeData[!univocalName %in% c("Cycle", "Walk")]
  mileageUnit <- unique(dt$unit)
  dt <- merge.data.table(completeData, dt, all = TRUE)
  # For some regions an annual mileage is provided for certain vehicle types, but no demand.
  # These values need to be deleted
  dt <- dt[!is.na(check)]
  # update variable and unit for introduced NAs
  dt[, unit := mileageUnit][, variable := "Annual mileage"][, check := NULL]
  # By averaging the annual mileage over gases and liquids vehicles to fill gaps for BEVs, BEVs get a higher
  # annual mileage than the ICE cars
  # Fixing that only for EUR countries for now
  dt[region %in% ISOcountriesMap[regionCode12 == "EUR"]$countryCode,
     value := ifelse(is.na(value), value[technology == "Liquids"], value), by = c("period", "univocalName", "region")]

  dt[, value := ifelse(is.na(value), mean(value, na.rm = TRUE), value),
     by = c("period", "univocalName", "region")]

  # If there are NAs take mean over regions by technology
  dt[, value := ifelse(is.na(value), mean(value, na.rm = TRUE), value),
     by = c("period", "technology", "univocalName")]

  # Rickshaw (passenger three-wheelers, IND only) #plausibilityFix_IND #plausibilityFix_Pass
  # 26,200 km/yr for all technologies and years
  # UCD's 8,478 km/yr are copied from its two-wheeler value.
  # Derived: Mean of published values, for 330 operating days per year (CEEW's convention):
  # 1. TERI (2024), Table 4.2: 59 km per day -> 19,500 km/yr
  # (the report's text says more than 60 km between and 70 km within cities);
  # 2. CEEW (2025c), Table 3: 100 km per day, 33,000 km/yr
  # stock = pkm / (LF x AM) in 2010 is ca. 3.1 million (published: 2.5 million, TERI (2024))
  # Sources, in /p/projects/edget/adjustmentDataFiles/IND_validation/Input/papers/:
  #   TERI (2024), Roadmap for India's Energy Transition in the Transport Sector; SourceID: S01;
  #     src07_TERI_2024_energyTransitionRoadmap.pdf
  #   CEEW (2025c), What Drives Vehicle Ownership Costs in India? A Segment-wise Analysis for India's Road
  #     Transport; SourceID: S26; trucks/T26_CEEW_2025_vehicleOwnershipCosts.pdf
  dt[region == "IND" & univocalName == "Rickshaw", value := 26200]
  dt <- dt[period <= 2010, value := value[period == 2010], by = .(region, univocalName, variable, technology)]

  # b) Annual Mileage for Trucks is missing completely - insert assumptions made by Alois in 2022
  # (probably from ARIADNE). IND: overwritten in c).
  annualMileageTrucks <- fread(
                               text = "univocalName, annualMileage
              Truck (0-3_5t), 21500
              Truck (7_5t), 34500
              Truck (18t), 53000
              Truck (26t), 74000
              Truck (40t), 136500")


  dt <- merge.data.table(dt, annualMileageTrucks, by = "univocalName", all.x = TRUE, allow.cartesian = TRUE)
  dt[, value := ifelse(!is.na(annualMileage), annualMileage, value)][, annualMileage := NULL]

  # c) Trucks in India #plausibilityFix_IND #plausibilityFix_Truck #plausibilityFix_Freight
  # 2020 values used for all years and technologies.
  # Derived: Mean of published fleet values for the respective class:
  # 0-3_5t 25,000: mean of CSTEP (2022) Fig. 2 (2 t: 18,000; 3.5 t: 27,000) and Table 1 (LGV: 30,000)
  # 7_5t   37,100: mean of TERI (2024): (MGV: 37,227), CSTEP (2022) Fig. 2 and Table 1 (12 t: 37,000, MGV: 37,000)
  # 18t    48,600: mean of TERI (2024) survey (12-16.2 t: 42,208) and CSTEP (2022) Fig. 2 (16 t: 55,000)
  # 26t    64,200: mean of CSTEP (2022) Fig. 2 (25 t: 65,500) and TERI (2024) survey (HGV classes: 62,199, 64,906)
  # 40t    63,700: mean of CSTEP (2022) Fig. 2 (40.2 t: 62,500) and TERI (2024) survey (above 25 t: 64,906)
  # Values correspond to: B-AM-01 to 05 in
  # .../IND_validation/data/truck_validation/estimates/energyLoadMileage_bounds.csv
  # Sources, in /p/projects/edget/adjustmentDataFiles/IND_validation/Input/papers/:
  #   TERI (2024), Roadmap for India's Energy Transition in the Transport Sector, Table 4.3 (field survey of goods
  #     vehicles, 2023); SourceID: S01; src07_TERI_2024_energyTransitionRoadmap.pdf
  #   CSTEP (2022), The Potential to Electrify Freight Transportation
  #     in India; SourceID: S15; trucks/T15_CSTEP_2022_electrifyFreight.pdf
  # Note: These AM estimates set the 2010 truck stock and the tkm split between classes (toolAdjustEsDemand, step 3)
  # stock = tkm / (LF x AM), with AM from here and LF from toolAdjustLoadFactor
  AMChangesIND <- c("Truck (0-3_5t)" = 25000, "Truck (7_5t)" = 37100, "Truck (18t)" = 48600,
                    "Truck (26t)" = 64200, "Truck (40t)" = 63700)
  dt[region == "IND" & univocalName %in% names(AMChangesIND), value := AMChangesIND[univocalName]]

  # d) We do not have vintage tracking for the rest of the modes -> insert zeros
  # Later on it would be great to top up this data
  missingAnnualMileageData <- fread(
                                    text = "univocalName, annualMileage
              International Aviation, 0
              Domestic Aviation, 0
              Passenger Rail, 0
              HSR, 0
              Domestic Ship, 0
              International Ship, 0
              Freight Rail, 0")
  dt <- merge.data.table(dt, missingAnnualMileageData, by = "univocalName", all.x = TRUE, allow.cartesian = TRUE)
  dt[, value := ifelse(!is.na(annualMileage), annualMileage, value)][, annualMileage := NULL]

  # 3 adjustments for scenarioMIP validation
  # a) adjust outliers to global mean
  dt[, meanValue := mean(value, na.rm = TRUE), by = c("univocalName", "technology", "period")]
  dt[region %in% ISOcountriesMap[regionCode21 %in% c("NES", "CHA")]$countryCode &
       univocalName %in% filter$trn_pass_road_LDV_4W, value := meanValue]
  dt[region %in% ISOcountriesMap[regionCode21 %in% c("EWN", "ENC", "UKI", "NES")]$countryCode &
       grepl("Bus", univocalName), value := meanValue]

  dt[, meanValue := NULL]

  # Cars in India #plausibilityFix_IND #plausibilityFix_Pass
  # Set after the outlier means in 3a), so that they do not change other regions.
  # 13,500 km/yr for all car classes and technologies.
  # Derived: AM in vkm = AM in pkm / LF
  # 36,500 pkm per car and year (derived in note A12, sheet "DifferencesToLiterature" in
  # .../IND_validation/data/validationEstimates.xlsx)
  # 2.7 passengers per car (published: TERI (2024), Table 4.2)
  # Road demand of toolAdjustEsDemand() gave 16 million cars in 2010 (published: 10 million, CEEW (2025b)).
  # Sources, in /p/projects/edget/adjustmentDataFiles/IND_validation/Input/papers/:
  #   TERI (2024), Roadmap for India's Energy Transition in the Transport Sector; SourceID: S01;
  #     src07_TERI_2024_energyTransitionRoadmap.pdf
  #   CEEW (2025b), How Will India's Vehicle Ownership Grow? A District-level Outlook to 2050; SourceID: S20;
  #     src01_CEEW_2025_vehicleOwnership.pdf
  dt[region == "IND" & univocalName %in% filter$trn_pass_road_LDV_4W, value := 13500]

  # b) data until 2010 has weird spikes for some regions -> remove data between 1991 and 2009
  # and interpolate afterward to remove the spikes
  xdata <- unique(dt$period)
  dt <- dt[period == 1990 | period > 2010]
  dt <- rmndt::approx_dt(dt, xdata, "period", "value")

  # c) Until we have better data, we keep the values konstant after 2030
  dt <- dt[period >= 2030, value := value[period == 2030], by = setdiff(names(dt), c("value", "period"))]
  return(dt)
}
