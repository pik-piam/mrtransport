#' Perform parameter specific adjustments on the input data
#'
#' Fills gaps in the raw historical energy service demand (GCAM, TRACCS, Eurostat) and corrects implausible values
#' for single regions and modes.
#'
#' @details India (IND), values by assumption ID from the EDGETassumptions source ([toolGetAssumption()])
#' - Truck size split (step 3, A21): Vehicle shares derived from Indian stock and sales data replace previous
#'   assumptions
#' - Bus demand (step 7b, A22): 2010 bus pkm derived from India's pre-COVID bus use benchmarked against China at the
#'   same income; replaces a factor 1/2 in step 4
#' - Road demand (step 7b, A23, A24): passenger road = buses + 2-4-wheelers (as before); road freight from the IEA's
#'   published 2010 total; applied to 2005-2010; replace a factor 1.4 on all IND demand (step 7a)
#' - Domestic shipping and aviation (step 7b, A25, A26): published 2010 activity (RITES, DGCA)
#' - Bus technologies (step 7b, A27): share of the 2005-2010 bus pkm on gas (CNG share of the registered buses, TERI)
#' - Road demand 1990 (step 7c): computed so that the 1990 IEA harmonization factor equals the 2005 one; not an
#'   estimate of the 1990 activity
#' - Check (step 7d): warns when the aviation intensity factor of toolAdjustEnergyIntensity() no longer fits
#' - Status, derivation and sources of each value are in the table, references and PDF paths in README.txt of the
#'   source.
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
  # CEEW (2022), source 10 in .../IND_validation/27-10-2026/data/validationEstimates.xlsx)
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
  #   IND:         A21 (see below)
  #
  # CHN/HKG/MAC and JPN: these are compromise values: closer to real-world than GCAM defaults, but
  # still producing reasonable 2010/2015 truck numbers given other input data
  # constraints. Should be updated to real-world values once other inputs improve.
  # Source can be found in:
  #   CEIC: https://www.ceicdata.com/en/china/no-of-motor-vehicle/cn-no-of-motor-vehicle-truck-heavy
  #   Data/RegionalData/compiling_CHA_data_heavy_duty_vehicles.xlsx (cells V17:V21, CHA)
  #   Also for Data/RegionalData/compiling_JPN_data_heavy_duty_vehicles.xlsx
  #
  # A21 (IND): vehicle shares from Indian stock and sales data, 2020 values used for 2010

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
      unname(toolGetAssumption("A21")[TRUCK_SIZES])  # IND (A21)
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
  # A22-A26: 2010 targets for buses, 2-4-wheelers, trucks and domestic shipping, 2005 and 2010 for domestic aviation
  busPkmIND2010   <- toolGetAssumption("A22")[["Bus"]]                # billion pkm
  ldvPkmIND2010   <- toolGetAssumption("A23")[["trn_pass_road_LDV"]]  # billion pkm, 2-4-wheelers
  passRoadIND2010 <- busPkmIND2010 + ldvPkmIND2010
  busShareIND2010 <- busPkmIND2010 / passRoadIND2010
  truckTkmIND2010 <- toolGetAssumption("A24")[["trn_freight_road"]]   # billion tkm
  shipTkmIND2010  <- toolGetAssumption("A25")[["Domestic Ship"]]      # billion tkm
  airPkmIND       <- toolGetAssumption("A26", by = "period")          # billion pkm, 2005 and 2010
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

  # Bus technologies in 2005-2010
  # A27: share of bus pkm on gas (CNG share of the registered buses), the rest on liquids
  cngShareBusIND <- toolGetAssumption("A27")[["Bus"]]
  dt[region == "IND" & period >= 2005 & univocalName == "Bus",
     value := sum(value) * ifelse(technology == "Gases", cngShareBusIND,
                                  (1 - cngShareBusIND) * value / sum(value[technology != "Gases"])),
     by = "period"]

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
