#' Perform parameter specific adjustments on the input data
#'
#' Fills gaps in the raw historical energy service demand (GCAM, TRACCS, Eurostat) and corrects implausible values
#' for single regions and modes.
#'
#' @details India (IND)
#' - Truck size split (step 3): Vehicle shares derived from Indian stock and sales data replace previous assumptions
#' - Bus share (step 7b): 54.5 % of passenger road pkm in 2010, derived from TERI; replaces a factor 1/2 in step 4
#' - Road demand (step 7b): published 2010 totals (TERI, IEA) for passenger road and road freight, applied to
#'   2005-2010; replace a factor 1.4 on all IND demand (step 7a)
#' - Domestic shipping and aviation (step 7b): published 2010 activity (RITES, DGCA)
#' - Road demand 1990 (step 7c): computed so that the 1990 IEA harmonization factor equals the 2005 one; not an
#'   estimate of the 1990 activity
#' - Check (step 7d): warns when the aviation intensity factor of toolAdjustEnergyIntensity() no longer fits
#' - Code comments give the source and status of each number, with local copies in
#'   /p/projects/edget/adjustmentDataFiles/IND_validation/.
#' - All India-specific pdfs are in .../Input/papers/
#'
#' @author Johanna Hoppe, Jarusch Muessel
#' @param dt calculated raw data without adjustments
#' @param mapIso2region map iso countries to regions
#' @param completeData All combinations of region, period, univocalName and technology in EDGE-T decision tree
#' @param filter list of filters for specific branches in the upper decision tree, containing all associated
#' univocalNames
#' @param histSourceData the full source data containing the annual mileage and load factor; their 2010 Liquids
#' values turn the target truck vehicle shares of step 3 into tkm shares
#' @return a quitte object with the adjusted energy service demand in billion pkm/yr and billion tkm/yr

toolAdjustEsDemand <- function(dt, mapIso2region, completeData, filter, histSourceData) {
  variable <- period  <- unit <- value <-  demldv <- regionCode21 <- technology <- valueNew <- . <-
    regionCode12 <- region <- univocalName <- oldES <- totalES <- targetES <- ESshare <- ESperVeh <-
    loadFactor <- allZero <- annualMileage <- scaling <- te <- flow <- fe <- enIntensity <- NULL

  dt <- merge.data.table(dt, completeData[period <= 2010],
                         by = c("region", "period", "univocalName", "technology"), all = TRUE)
  # completeData does not contain unit so it needs to be added
  dt[univocalName %in% c(filter$trn_pass, "International Aviation"), unit := "billion pkm/yr"]
  dt[univocalName %in% c(filter$trn_freight, "International Ship"), unit := "billion tkm/yr"]
  dt[is.na(value), variable := "ES"]
  dt <- merge.data.table(dt, mapIso2region, by = "region", all.x = TRUE, allow.cartesian = TRUE)




  # 1: Some Truck types, Rail, alternative technologies and active modes are lacking energy service demand data
  # The missing modes get a zero demand for now. After the regional aggregation, the zero demand remains only
  # for alternative technologies
  dt[is.na(value), value := 0]



  # 2: Add some base demand for Cycle & Walk (2%)
  # IND: not applied, as GCAM's walking and cycling are not zero (1,203 billion pkm in 2010; published: 1,200,
  # CEEW (2022), source 10 in .../IND_validation/data/validationEstimates.xlsx)
  dt[, demldv := sum(value), by = c("period", "region")]
  dt[univocalName == "Cycle" & value == 0, value := demldv * 0.01]
  dt[univocalName == "Walk" & value == 0, value := demldv * 0.002]
  dt[univocalName == "Cycle" & value == 0 & regionCode21 %in% c("USA", "AUS", "CAN"), value := demldv * 0.006]
  dt[univocalName == "Cycle" & value == 0 & regionCode21 %in% c("IND", "CHN"), value := demldv * 0.02]
  dt[, demldv := NULL]


  # 3: Correct truck size ES splits for CHN/HKG/MAC, JPN and IND
  # #plausibilityFix_CHN #plausibilityFix_JPN #plausibilityFix_IND #plausibilityFix_Truck #plausibilityFix_Freight
  #
  # Original GCAM input data has implausible truck size distributions:
  #   - CHN/HKG/MAC: 95% of ES in 0-3.5t trucks (should be ~64% per CEIC data)
  #   - JPN: 99.999% in 0-3.5t trucks (heavy-duty trucks essentially missing)
  #   - IND: 55% in 0-3.5t trucks and 0% for Trucks between 7.5t and 40t
  #
  # Real-world target vehicle share (% of vehicle count, not % of tonne-km) used here:
  #   CHN/HKG/MAC: 0-3.5t=76%, 7.5t=13%, 18t=5%, 26t=3%, 40t=3%
  #   JPN:         0-3.5t=88%, 7.5t=9%,  18t=1%, 26t=1%, 40t=1%
  #   IND:         0-3.5t=58.0%, 7.5t=12.7%, 18t=5.6%, 26t=16.9%, 40t=6.8%
  #
  # CHN/HKG/MAC and JPN: these are compromise values: closer to real-world than GCAM defaults, but
  # still producing reasonable 2010/2015 truck numbers given other input data
  # constraints. Should be updated to real-world values once other inputs improve.
  # Source can be found in:
  #   CEIC: https://www.ceicdata.com/en/china/no-of-motor-vehicle/cn-no-of-motor-vehicle-truck-heavy
  #   Data/RegionalData/compiling_CHA_data_heavy_duty_vehicles.xlsx (cells V17:V21, CHA)
  #   Also for Data/RegionalData/compiling_JPN_data_heavy_duty_vehicles.xlsx
  #
  # IND: 2020 values used for 2010 (no stock split by class is published before 2020)
  # The previous assumption for shares gave too few light and too many 7_5t trucks.
  # Derived:
  # 0-3.5t share = N1 stock / (N1 stock + stock above 3.5 t)
  #   N1 trucks 4.7 million (published: TERI (2020))
  #   trucks above 3.5 t 3.4 million (derived: back-cast from CEEW (2025) and NITI Aayog and RMI (2022))
  # Split above 3.5 t: cumulative sales FY 2013-14 to 2019-20 by segment (published: CEEW (2025), Fig. 1), with
  #   12-20 t at 0.41 of the 12-25 t segment (mean of ICCT's FY 2013-14, 2017-18 and 2019-20 values)
  # Cross-check: with IND LF and AM these shares give 3.4 % of tkm in 0-3.5t trucks (estimate B-TS-01)
  # Values correspond to: B-ST-01 to 05 in
  # .../IND_validation/data/truck_validation/estimates/fleetComposition_bounds.csv
  # Sources, in /p/projects/edget/adjustmentDataFiles/IND_validation/Input/papers/:
  #   TERI (2020), Road Map for Electrification of Urban Freight in India (Part I); SourceID: S19;
  #     trucks/T19_TERI_2020_urbanFreightElectrification.pdf
  #   CEEW (2025), India's Road to Zero-emission Trucking; SourceID: S03; src08_CEEW_2025_zeroEmissionTrucking.pdf
  #   NITI Aayog and RMI (2022), Transforming Trucking in India; SourceID: S05;
  #     trucks/T05_NITI_RMI_2022_zeroEmissionTrucks.pdf
  #   ICCT (2015, 2019, 2021b), Market analysis of heavy-duty vehicles in India; SourceID: S18, S21, S17;
  #     trucks/T18_ICCT_2015_HDVmarket.pdf, T21_ICCT_2019_HDVmarket.pdf, T17_ICCT_2021_HDVmarket.pdf

  REGIONS_TO_FIX <- c("CHN", "HKG", "MAC", "JPN", "IND")
  TRUCK_SIZES    <- c("Truck (0-3_5t)", "Truck (7_5t)", "Truck (18t)", "Truck (26t)", "Truck (40t)")

  # Step 1: Define target vehicle count shares by size
  # These are shares of *vehicle count*, not ES. ES shares are derived below
  # by weighting by annual tkm per vehicle (mileage × load factor).
  targetVehicleShares <- data.table(
    univocalName = rep(TRUCK_SIZES, length(TRUCK_SIZES)),
    region       = rep(REGIONS_TO_FIX, each = length(TRUCK_SIZES)),
    value        = c(
      rep(c(76, 13, 5, 3, 3), 3),  # CHN, HKG, MAC
      c(88,  9, 1, 1, 1),       # JPN
      c(58.0, 12.7, 5.6, 16.9, 6.8)  # IND (2020 centrals used for 2010, see above)
    )
  )

  # Step 2: Extract current ES for trucks in affected regions
  currentES <- dt[region %in% REGIONS_TO_FIX & univocalName %like% "Truck",
                  .(region, univocalName, technology, period, value)]

  # Step 3: Initialize near-zero ES for size classes missing from input data
  # JPN: HD trucks entirely absent → seed with a tiny value so rescaling works.
  # CHN/HKG/MAC: 26t and 40t are zero → seed proportionally from 18t entries
  #   (preserving the technology split, since CNG share matters for CHN).
  # The seed values are negligible relative to total ES (factor 1e-4).
  minTotalES <- min(currentES[, sum(value), by = .(region, period)]$V1)
  seedValue  <- 1e-4 * minTotalES

  currentES[, allZero := all(value == 0), by = .(region, univocalName, period)]
  currentES[allZero == TRUE & technology == "Liquids", value := seedValue]
  currentES[, allZero := NULL]

  # For CHN/HKG/MAC, fill 26t and 40t ES based on 18t ES (preserving technology split)
  currentES[region %in% c("CHN", "HKG", "MAC"),
            value := ifelse(univocalName %in% c("Truck (26t)", "Truck (40t)"),
                            value[univocalName == "Truck (18t)"] * 1e-4,
                            value),
            by = .(period, region, technology)]

  # Step 4: Compute annual tkm per vehicle = mileage × load factor
  # Use 2010 Liquids values as the reference (dominant technology that year).

  annualTkmPerVehicle <- merge(
    histSourceData$annualMileage[
                                 region %in% REGIONS_TO_FIX & univocalName %like% "Truck" & period == 2010 &
                                   technology == "Liquids", .(region, univocalName, annualMileage = value)],
    histSourceData$loadFactor[
                              region %in% REGIONS_TO_FIX & univocalName %like% "Truck" & period == 2010 &
                                technology == "Liquids", .(region, univocalName, loadFactor)],
    by = c("region", "univocalName")
  )[, ESperVeh := annualMileage * loadFactor]

  # Step 5: Convert target vehicle shares → target ES shares
  # ES share ∝ vehicle share × ES per vehicle. Normalise within each region.

  #test whether row dimension matches: REGIONS_TO_FIX x TRUCK_SIZES = 5 x 5 = 25 rows
  if (nrow(targetVehicleShares) != nrow(annualTkmPerVehicle)) {
    stop("Mismatch in number of rows between target vehicle shares and annual tkm per vehicle data.")
  }

  targetESshares <- merge(targetVehicleShares, annualTkmPerVehicle,
                          by = c("region", "univocalName"))

  targetESshares[, ESshare := (value / 100) * ESperVeh]
  targetESshares[, ESshare := ESshare / sum(ESshare), by = region]

  targetESshares <- targetESshares[, .(region, univocalName, ESshare)]

  # Step 6: Derive target ES per size, preserving regional totals

  totalESbyRegion <- currentES[, .(totalES = sum(value)), by = .(region, period)]

  targetESbySize <- targetESshares[
    totalESbyRegion, on = "region", allow.cartesian = TRUE
  ][, targetES := ESshare * totalES]

  # Step 7: Compute per-size scaling factors and rescale

  currentESbySize <- currentES[, .(oldES = sum(value)), by = .(region, univocalName, period)]
  scalingFactors <- currentESbySize[targetESbySize,
    on = c("region", "univocalName", "period"),
    nomatch = NULL
  ][, scaling := targetES / oldES]

  updatedES <- merge(currentES, scalingFactors,
    by = c("region", "univocalName", "period"),
    all.x = TRUE
  )[, value := value * scaling
  ][, c("oldES", "ESshare", "totalES", "targetES", "scaling") := NULL
  ][, ":="(variable = "ES", unit = "billion tkm/yr")]


  # Step 8: Verify regional ES totals are unchanged
  stopifnot(all.equal(
    updatedES[, sum(value), by = .(period, region)],
    currentES[, sum(value), by = .(period, region)]
  ))

  # Step 9: Write updated values back to dt

  setnames(updatedES, "value", "valueNew")
  dt <- merge(dt, updatedES, by = intersect(names(dt), names(updatedES)), all.x = TRUE)
  dt[!is.na(valueNew), value := valueNew][, valueNew := NULL]

  # Also adjust car ES demands upwards to better reflect car stock numbers in 2010 and 2015
  # (~62 mio in 2010, 140 mio in 2015 , eg IEA GEVO and others - see file in the owncloud
  # "Data/RegionalData/compiling_CHA_data_LDV.xlsx", Increasing the ES values means that more
  # cars are on the road, but also that 2010 FE demand BEFORE the IEA calibration is higher
  # - thus preventing the strong upscaling of energy intensities during the IEA FE calibration that was
  # previously the case. The multipliers are staggered (reducing from 2.5 in 2010 to 1.5 in 2005) to represent
  # the fast growth of LDV numbers over these 5 years. The 2.5 multiplier in 2010 is a compromise betwen hitting
  # 2010 and 2015 numbers: 2010 85 mio instead of 62 mio, 2015 125 mio instead of 140 mio
  # plausibilityFix_CHN #plausibilityFix_LDV #plausibilityFix_Pass

  carTypes <- c("Compact Car", "Large Car", "Large Car and SUV", "Midsize Car", "Mini Car", "Subcompact Car", "Van")

  dt[univocalName %in% carTypes & region == "CHN" & period == 2010, value := 2.5 * value]
  dt[univocalName %in% carTypes & region == "CHN" & period == 2009, value := 2.3 * value]
  dt[univocalName %in% carTypes & region == "CHN" & period == 2008, value := 2.1 * value]
  dt[univocalName %in% carTypes & region == "CHN" & period == 2007, value := 1.9 * value]
  dt[univocalName %in% carTypes & region == "CHN" & period == 2006, value := 1.7 * value]
  dt[univocalName %in% carTypes & region == "CHN" & period <= 2005, value := 1.5   * value]

  ############ end of new CHA stuff from Robert (& Jarusch for IND)

  # plausibilityFix_USA #plausibilityFix_Truck #plausibilityFix_Freight
  dt[region %in% c("USA", "PRI", "UMI", "VIR"), value := ifelse(univocalName == "Truck (26t)",
                                                                value[univocalName == "Truck (18t)"] / 3,
                                                                value),
     by = c("period", "region", "technology")]
  dt[region %in% c("USA", "PRI", "UMI", "VIR"), value := ifelse(univocalName == "Truck (40t)",
                                                                value[univocalName == "Truck (18t)"] / 3,
                                                                value),
     by = c("period", "region", "technology")]
  dt[region %in% c("USA", "PRI", "UMI", "VIR") & univocalName == "Truck (18t)", value := value / 3,
     by = c("period", "region", "technology")]


  # plausibilityFix_CHN #plausibilityFix_Rail #plausibilityFix_Pass
  # from https://www.iea.org/reports/tracking-rail-2020-2
  dt[period <= 2010 & regionCode21 == "CHN" & univocalName == "HSR", value := 70000]

  # plausibilityFix_CHN #plausibilityFix_Truck #plausibilityFix_Freight
  # from https://theicct.org/sites/default/files/China_Freight_Assessment_English_20181022.pdf
  # total road freight demand seems to be around 5 billion tkm * 0.8, a factor 3 roughly
  dt[period <= 2010 & regionCode21 == "CHN" & univocalName %in% filter$trn_freight_road, value := value * 3]

  # 4: Demand level corrections, adjusting to ETP demands
  # #plausibilityFix_CHN #plausibilityFix_Bus #plausibilityFix_Pass
  # #plausibilityFix_OAS #plausibilityFix_NEU #plausibilityFix_MEA
  # IND: the former factor 1/2 is replaced by the bus share in step 7b.
  dt[regionCode21 == "CHA" & univocalName == "Bus", value := value / 2.5]
  dt[regionCode21 == "OAS" & univocalName == "Bus", value := value / 5]
  dt[regionCode12 == "NEU" & univocalName == "Bus", value := value / 2]
  dt[regionCode21 == "MEA" & univocalName == "Bus", value := value / 2]

  # plausibilityFix_DEU #plausibilityFix_Truck #plausibilityFix_Freight
  # 5: Adjust GER Truck size shares according to KBA data (calculated from stocks via AM and LF)
  dt[region == "DEU" & univocalName == "Truck (0-3_5t)", value := value * 2]
  dt[region == "DEU" & univocalName == "Truck (7_5t)", value := value * 0.25]
  dt[region == "DEU" & univocalName == "Truck (18t)", value := value * 0.65]
  dt[region == "DEU" & univocalName == "Truck (40t)", value := value * 1.4]

  # plausibilityFix_DEU #plausibilityFix_Truck #plausibilityFix_Freight
  # 6: Total 2010 Freight demands, from ViZ 2010
  # (the shares are roughly OK)
  dt[region == "DEU" & univocalName %in% filter$trn_freight, value := value * 620 / 587]
  dt[, c("countryName", "regionCode21", "regionCode12", "check") := NULL]

  # 7: Historical activity in India (IND): 2010 targets, 1990 road demand and the check of the aviation factor
  # #plausibilityFix_IND #plausibilityFix_Pass #plausibilityFix_Truck #plausibilityFix_Freight
  #
  # 7a: Remove factor 1.4 on all IND demand in 2005 and 2010, replaced by the 2010 targets of 7b.
  # Old code: dt[region == "IND" & period %in% c(2005, 2010) & univocalName %in% c(filter$trn_pass, filter$trn_freight),
  #    value := value * 1.4] 

  # 7b: Road, shipping and aviation demand in 2005-2010
  #
  # Passenger road (bus, cars, 2W, 3W): 3,852 billion pkm in 2010
  # Published: TERI (2024), Figure 2.2 (FY 2009-10)
  # Other published 2010 values: NITI Aayog (2026) 2,964; CEEW (2022) 1,782; Paladugula et al. (2018) 2,000-5,800
  #
  # Road freight (all truck classes): 1,082 billion tkm in 2010
  # Published: IEA, World Energy Outlook 2025, historical data (historical.mif);
  # Other published 2010 values: RITES (2009) 706; TERI (2024) about 615; CEEW (2022)
  # 810; Paladugula et al. (2018) 500-1,000; NITI Aayog (2026) 1,742
  # Truck stock in 2010 (4-5 million) points to a rather higher value and using a lower value
  # would lead to too high energy intensities.
  #
  # Domestic shipping: 105 billion tkm in 2010 (GCAM: 159)
  # Derived: 89.5 billion tkm in 2007-08 (published: RITES (2009), quoted in Raghuram (2021)) grown at 6.5 %/yr
  # (assumption: the rate of the 2020 value, note A2) -> lead's to good results: FE 0.036 EJ, close to IEA's 0.034 EJ
  #
  # Bus share: 54.5 % of passenger road pkm in 2010
  # Derived: bus pkm 2019-20 combined with bus fleet growth:
  # 2,903 / 1.033^10 / 3,852 = 54.5 % #2020-bus-ES / (bus-fleet growth)^10 / 2010-total-ES
  # 2,903 billion pkm: bus share 47.6 % (TERI (2024), Figure 5.8, chart) x 6,098 (TERI's 2019-20 road total)
  # 1.033: growth of the registered bus fleet 2009-2019 (TERI (2024), Table 1.2, from MoRTH)
  # Assumption: pkm per bus constant since 2010
  # Other published shares for more recent years: CEEW (2022), 2020: 25 %; IEA ETP 2017, 2014: 34 %; ITDP and UC Davis (2023),
  # urban, 2015: 37 %
  #
  # Domestic aviation: 18 billion pkm in 2005 and 44 in 2010 (GCAM: 21 and 35); 1990 keeps GCAM's value
  # Published: DGCA, Handbook on Civil Aviation Statistics, as charted in TERI (2024), Figure 2.4 (2010: chart
  # reading)
  #
  # Values correspond to: notes and source numbers in .../IND_validation/data/validationEstimates.xlsx
  # Sources, in /p/projects/edget/adjustmentDataFiles/IND_validation/Input/papers/:
  #   TERI (2024), Roadmap for India's Energy Transition in the Transport Sector; SourceID: S01;
  #     src07_TERI_2024_energyTransitionRoadmap.pdf
  #   NITI Aayog (2026), Scenarios Towards Viksit Bharat and Net Zero, Sectoral Insights: Transport (Vol. 3);
  #     SourceID: S22; src15_NITI_2026_viksitBharat_vol3Transport.pdf
  #   NITI Aayog and RMI (2021), Fast Tracking Freight in India; SourceID: S02;
  #     src02_NITI_RMI_2021_fastTrackingFreight.pdf
  #   Raghuram (2021), Challenges to Sustainable Road Freight Transport in India (slides quoting RITES (2009), Total
  #     Transport System Study); workbook source 60; Raghuram_challenges_sustainable_Road_Freight.pdf
  #   CEEW (2022), India Transport Energy Outlook; workbook source 10; src10_CEEW_2022_transportEnergyOutlook.pdf
  #   Paladugula et al. (2018), A multi-model assessment of energy and emissions for India's transportation sector
  #     through 2050; workbook source 3; src03_Paladugula_2018_multiModelAssessment.pdf
  #   ITDP and UC Davis (2023), Compact Cities Electrified: India; workbook source 6;
  #     src06_ITDP_UCD_2023_compactCitiesElectrified.pdf
  #   Datasets without a PDF: IEA (2025), World Energy Outlook 2025, and IEA (2017), Energy Technology Perspectives
  #     2017, in historical.mif of the edgeTransport input data; DGCA (read in TERI (2024), Figure 2.4)
  # Note: The IEA harmonization scales all IND liquids modes by one factor per year, so 2010 activity that is too
  # low or too high moves the intensity of every mode. A bus pkm needs a quarter to a third of the fuel of a car or
  # 2W pkm, so the bus share moves that factor too. The aviation intensity factor of toolAdjustEnergyIntensity()
  # (step 2c) is set for the activity here; the check in 7d warns when it no longer fits.
  passRoadIND2010 <- 3852  # billion pkm, TERI (2024)
  truckTkmIND2010 <- 1082  # billion tkm, IEA WEO 2025 historical road freight
  shipTkmIND2010  <- 105   # billion tkm, RITES (2009) 2007-08 value grown at 6.5 %/yr
  busShareIND2010 <- 0.545 # buses in passenger road pkm, derived from TERI (2024)
  airPkmIND       <- c("2005" = 18, "2010" = 44) # billion pkm, DGCA via TERI (2024), Figure 2.4
  busIND <- dt[region == "IND" & period == 2010 & univocalName == "Bus", sum(value)]
  ldvIND <- dt[region == "IND" & period == 2010 & univocalName %in% setdiff(filter$trn_pass_road, "Bus"), sum(value)]
  dt[region == "IND" & univocalName == "Bus",
     value := value * busShareIND2010 / (1 - busShareIND2010) * ldvIND / busIND]
  dt[region == "IND" & period >= 2005 & univocalName %in% filter$trn_pass_road,
     value := value * passRoadIND2010 / sum(value[period == 2010])]
  dt[region == "IND" & period >= 2005 & univocalName %in% filter$trn_freight_road,
     value := value * truckTkmIND2010 / sum(value[period == 2010])]
  dt[region == "IND" & period == 2010 & univocalName == "Domestic Ship",
     value := value * shipTkmIND2010 / sum(value)]
  dt[region == "IND" & period %in% c(2005, 2010) & univocalName == "Domestic Aviation",
     value := value * airPkmIND[as.character(period)] / sum(value), by = "period"]

  # 7c: Road demand in 1990 (passenger road and trucks; rail, aviation and shipping keep GCAM's values)
  # One factor, computed so that the 1990 liquids harmonization factor equals the 2005 one (1.28 on 2026-10-21)
  # With GCAM's 1990 road demand the 1990 factor was 1.41 against 1.13 in 2005, and EDGE-T's 2005 liquids FE was 9 %
  # above the IEA's: its 2005 fleet includes vehicles bought before 2005, with intensities interpolated from 1990.
  # Derived: factor = (IEA 1990 / harm2005 - non-road FE 1990) / road FE 1990, with harm2005 = IEA 2005 / FE 2005
  # FE: final energy of the raw liquids intensities (intensity / LF x demand), as in toolIEAharmonization()
  # IEA: the IEA's liquids without bunkers, as in toolIEAharmonization()
  # TERI (2024), Figures 2.2 and 2.7, prints lower 1990-91 road values (680 billion pkm, 127 billion tkm).
  roadIND <- c(filter$trn_pass_road, filter$trn_freight_road)
  ieaIND <- magpie2dt(calcOutput(type = "IEAOutputTransport", aggregate = FALSE)["IND", , ],
                      datacols = c("se", "fe", "te", "mod", "flow"), regioncol = "region", yearcol = "period")
  ieaIND <- ieaIND[te %in% c("tdfospet", "tdfosdie", "tdbiopet", "tdbiodie") & !grepl("BUNK", flow)]
  feIND <- merge(dt[region == "IND" & period %in% c(1990, 2005, 2010) & value > 0 &
                      !technology %in% c("BEV", "Electric", "Gases") &
                      !univocalName %in% c("Cycle", "Walk", "International Aviation", "International Ship"),
                    .(period, univocalName, technology, value)],
                 merge(histSourceData$enIntensity, histSourceData$loadFactor,
                       by = c("region", "period", "univocalName", "technology"))[region == "IND"],
                 by = c("period", "univocalName", "technology"), all.x = TRUE)
  if (anyNA(feIND)) stop("IND: energy intensity or load factor missing for a liquids mode with demand")
  feIND[, fe := value * enIntensity / loadFactor * 1e-3] # billion (p|t)km x MJ/vehkm / ((p|t)/veh) -> EJ
  harm2005IND <- ieaIND[period == 2005, sum(value)] / feIND[period == 2005, sum(fe)]
  road1990IND <- (ieaIND[period == 1990, sum(value)] / harm2005IND -
                    feIND[period == 1990 & !univocalName %in% roadIND, sum(fe)]) /
    feIND[period == 1990 & univocalName %in% roadIND, sum(fe)]
  dt[region == "IND" & period == 1990 & univocalName %in% roadIND, value := value * road1990IND]

  # 7d: Check of the domestic aviation intensity factor (toolAdjustEnergyIntensity(), step 2c): its harmonized fuel
  # use in 2010 should equal the IEA's domestic aviation (flow DOMESAIR). The factor depends on all other IND liquids
  # inputs through the common harmonization factor, so warn with the multiplier it needs when it is off by over 2 %.
  feAir <- feIND[period == 2010 & univocalName == "Domestic Aviation", sum(fe)]
  feOther <- feIND[period == 2010 & univocalName != "Domestic Aviation", sum(fe)]
  ieaAir <- ieaIND[period == 2010 & flow == "DOMESAIR", sum(value)]
  airMultiplier <- ieaAir * feOther / (feAir * (ieaIND[period == 2010, sum(value)] - ieaAir))
  if (abs(airMultiplier - 1) > 0.02) {
    warning(sprintf(paste("IND: harmonized domestic aviation fuel in 2010 does not match the IEA's; multiply the",
                          "factor in toolAdjustEnergyIntensity() (step 2c) by %.3f"), airMultiplier))
  }

  return(dt)
}
