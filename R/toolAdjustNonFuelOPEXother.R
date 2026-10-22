#' Perform parameter specific adjustments on the input data
#'
#' Fills gaps in the raw non-fuel OPEX of vehicle types without fleet tracking and sets values for single regions and
#' modes.
#'
#' @details India (IND)
#' - Passenger three-wheelers ("Rickshaw", step 4): Liquids from UCD's Indian three-wheeler (toolPrepareUCD()); BEV =
#'   Liquids x EV/ICE ratio of maintenance plus insurance from CEEW (2025c)
#' - Code comments give the source and status of each number, with local copies in
#'   /p/projects/edget/adjustmentDataFiles/IND_validation/.
#' - All India-specific pdfs are in .../Input/papers/
#'
#' @author Johanna Hoppe, Jarusch Muessel
#' @param dt calculated raw data without adjustments
#' @param ISOcountries list of iso countries
#' @param yrs temporal resolution of EDGE-T model
#' @param completeData All combinations of region, period, univocalName and technology in EDGE-T decision tree
#' @param filter list of filters for specific branches in the upper decision tree, containing all associated
#' univocalNames
#' @importFrom rmndt magpie2dt
#' @import data.table
#' @return a quitte object

toolAdjustNonFuelOPEXother <- function(dt, ISOcountries, yrs, completeData, filter) {
  value <- . <- region  <- univocalName <- check <- unit  <- variable <-
    period <- technology <- liquids <- NULL

  # 1: Aggregate different non fuel OPEX types
  dt <- dt[, .(value = sum(value)), by = c("region", "period", "univocalName", "technology", "unit")]
  dt[, variable := "Operating costs (total non-fuel)"]

  # 2: Non fuel OPEX are given combined with CAPEX for shipping and rail. Apply shares
  # Trains
  # https://www.unescap.org/sites/default/files/1.%20Part%20A.%20Point%20to%20point%20railway%20traffic%20costing%20model.pdf  # nolint: line_length_linter
  # O&M 80% for low traffic lines
  # 50% for high traffic lines
  # -> 60% O&M -> CAPEX share = 40%
  dt[univocalName %in% c("Freight Rail", "Passenger Rail", "HSR"), value := value * (1 - 0.4)]
  dt[univocalName %in% c("Freight Rail", "Passenger Rail", "HSR"), variable := "Operating costs (total non-fuel)"]
  # Ships
  # CCS ships doi:10.1016/j.egypro.2014.11.285
  # CAPEX ~ 30%
  dt[univocalName %in% c("Domestic Ship", "International Ship"), value := value * (1 - 0.3)]
  dt[univocalName %in% c("Domestic Ship", "International Ship"), variable := "Operating costs (total non-fuel)"]

  # 3: Add hydrogen airplanes
  h2Air <- dt[univocalName == "Domestic Aviation" & technology == "Liquids"][, technology := "Hydrogen"]
  # following https://www.fch.europa.eu/sites/default/files/FCH%20Docs/20200507_Hydrogen%20Powered%20Aviation%20report_FINAL%20web%20%28ID%208706035%29.pdf # nolint: line_length_linter
  # maintenance costs are 50% higher than than a liquids fuelled airplane
  h2Air[period <= 2020, value := 1.5 * value]
  # for hydrogen airplanes, in between 2020 and 2040 the cost follows a linear trend,
  # and reaches a value 30% higher than a liquids fuelled airplane
  h2Air[period >= 2020,
        value := ifelse(period <= 2040, value[period == 2020] - (value[period == 2020] - 1.3 * value[period == 2100])
                        * (period - 2020) / (2040 - 2020), value), by = c("region")]
  h2Air[, value := ifelse(period > 2040, value[period == 2040], value), by = c("region")]
  dt <- rbind(dt, h2Air)

  # 4: Some two wheeler classes are missing and are replaced by other vehicle classes
  # Find missing values
  completeData <- completeData[!(univocalName %in% c(filter$trn_freight_road, "Cycle", "Walk") |
                                   univocalName %in% filter$trn_pass_road_LDV_4W | univocalName == "Bus")]
  dt <- merge.data.table(dt, completeData, all.y = TRUE)

  missing50 <- dt[is.na(value) & univocalName == "Motorcycle (50-250cc)"]
  missing250 <- dt[is.na(value) & univocalName == "Motorcycle (>250cc)"]
  missingMoped <- dt[is.na(value) & univocalName == "Moped"]
  missing3W <- dt[is.na(value) & univocalName == "Rickshaw"]

  # Get values of other vehicle types
  twoW50 <- dt[!is.na(value) & univocalName == "Motorcycle (50-250cc)"]
  twoW50 <- twoW50[, c("region", "technology", "period", "value")]
  setnames(twoW50, "value", "twoW50")

  twoW250 <- dt[!is.na(value) & univocalName == "Motorcycle (>250cc)"]
  twoW250 <- twoW250[, c("region", "technology", "period", "value")]
  setnames(twoW250, "value", "twoW250")

  twoWmoped <- dt[!is.na(value) & univocalName == "Moped"]
  twoWmoped <- twoWmoped[, c("region", "technology", "period", "value")]
  setnames(twoWmoped, "value", "twoWmoped")

  # Assign values of other vehicle types (step by step)
  missing50 <- merge.data.table(missing50, twoW250, by = c("region", "technology", "period"), all.x = TRUE)
  missing50[, value := twoW250][, twoW250 := NULL]

  missing250 <- merge.data.table(missing250, twoW50, by = c("region", "technology", "period"), all.x = TRUE)
  missing250[, value := twoW50][, twoW50 := NULL]
  missing250 <- missing250[!region == "UMI"]
  # United States Minor Outlying Islands (UMI) is a special case -> UCD assigns it to south east asia,
  # but GCAM to the USA. USA only features motorcycles > 250cc.
  # As the map for country specific vehicle types is based on the energy service demand that comes from
  # GCAM, UMI only features motorcycles (> 250cc). However, the costs are supplied by UCD where south east asia
  # only features mopeds and motorcycles (50-250cc) + 3W ICEs that are mapped on motorcycles (> 250cc).
  # We assign UMI to OAS
  # 1st approach: take mean value of OAS for UMI
  # missingUMI <- dt[!is.na(value) & univocalName == "Motorcycle (>250cc)"] # nolint: commented_code_linter
  # missingUMI <- merge.data.table(missingUMI, ISOcountries[, c("region", "regionCode21")], by = "region") # nolint: commented_code_linter
  # missingUMI <- missingUMI[regionCode21 == "OAS"] # nolint: commented_code_linter
  # missingUMI <- missingUMI[, .(value = mean(value)), by = c("sector", "subsectorL1", "subsectorL2", "subsectorL3", # nolint: commented_code_linter
  # "vehicleType", "technology", "variable", "univocalName", "unit", "period")] # nolint: commented_code_linter
  # BEV > 250 is featured only by a small number of countries where prices are overall much higher -> this would lead  # nolint: commented_code_linter
  # to unrealistic prices for BEV compared to ICEs
  # 2nd approach: take prices of Tuvalu (TUV) for ICE + BEV from lower category (costs are somewhat similar)
  # (UMI is anyway really small and will add little to OAS overall)
  # filter out the 3W ICEs in dt to prevent duplicates
  dt <- dt[!(region == "UMI" & univocalName == "Motorcycle (>250cc)")]
  missingUMI <- dt[region == "TUV" & univocalName == "Motorcycle (50-250cc)"][, region := "UMI"]
  missingUMI[, univocalName := "Motorcycle (>250cc)"]
  missing250 <- rbind(missing250, missingUMI)

  missingMoped <- merge.data.table(missingMoped, twoW50, by = c("region", "technology", "period"), all.x = TRUE)
  missingMoped <- merge.data.table(missingMoped, twoW250, by = c("region", "technology", "period"), all.x = TRUE)
  missingMoped[, value := twoW50][, twoW50 := NULL]
  missingMoped[is.na(value), value := twoW250][, twoW250 := NULL]

  # Rickshaw (passenger three-wheelers, IND only) #plausibilityFix_IND #plausibilityFix_Pass
  # Liquids: UCD's Indian three-wheelers (see toolPrepareUCD()), 0.0062 US$2017/vehkm in all years
  # BEV: Liquids x EV/ICE ratio of maintenance plus insurance, as UCD has no electric three-wheeler.
  # Derived: ratio = (maintenance + insurance)_EV / (maintenance + insurance)_ICE in INR/km, for passenger
  # three-wheelers (3W-P); no currency conversion needed, as only the ratio is used
  # Maintenance EV 0.42, ICE petrol 0.61, diesel 0.61, CNG 0.64 INR/km (published: CEEW (2025c), Table 5)
  # Insurance 1 % of the purchase price per year (EV 2.38, ICE 1.77, 1.93, 1.97 INR lakh; published: CEEW (2025c),
  # Annexure 1) over CEEW's 33,000 km/yr (not the model's 26,200, to stay consistent within CEEW)
  # -> EV 0.49, ICE 0.68 INR/km (unweighted ICE means), ratio 0.73
  # Battery replacement left out, as CEEW gives no number for it (assumption)
  # Sources, in /p/projects/edget/adjustmentDataFiles/IND_validation/Input/papers/:
  #   CEEW (2025c), What Drives Vehicle Ownership Costs in India? A Segment-wise Analysis for India's Road
  #     Transport; SourceID: S26; trucks/T26_CEEW_2025_vehicleOwnershipCosts.pdf and
  #     trucks/T26_CEEW_2025_vehicleOwnershipCosts_annexure.pdf
  ceewMaintenance <- c(EV = 0.42, ICE = mean(c(0.61, 0.61, 0.64)))                       # INR/km
  ceewInsurance <- 0.01 * c(EV = 2.38, ICE = mean(c(1.77, 1.93, 1.97))) * 1e5 / 33000    # INR/km
  ceewOPEX <- ceewMaintenance + ceewInsurance
  ratio3W <- ceewOPEX[["EV"]] / ceewOPEX[["ICE"]]
  liquids3W <- dt[univocalName == "Rickshaw" & technology == "Liquids", .(region, period, technology = "BEV",
                                                                          liquids = value)]
  missing3W <- merge.data.table(missing3W, liquids3W, by = c("region", "technology", "period"), all.x = TRUE)
  missing3W[, value := liquids * ratio3W][, liquids := NULL]
  if (anyNA(missing3W$value)) stop("Rickshaw non-fuel OPEX: Liquids are missing")


  missing2_3W <- rbind(missing50, missing250, missingMoped, missing3W)
  missing2_3W[, unit := unique(dt[!(is.na(value))]$unit)][, variable := "Operating costs (total non-fuel)"]

  dt <- rbind(dt[!(is.na(value) &
                     univocalName %in% c(filter$trn_pass_road_LDV_3W, filter$trn_pass_road_LDV_2W))], missing2_3W)
  dt[, check := NULL]

  return(dt)
}
