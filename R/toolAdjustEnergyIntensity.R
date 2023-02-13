#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param magpieobj the input data read via readSource, a magpie object
#' @param sourcetype one of the different EDGE-T inputdata sources
#' @return a quitte object
#'
#' @importFrom rmndt magpie2dt
#' @importFrom data.table fread

toolAdjustEnergydtensity <- function(dt, regionEUR, TrendsEndtPSI, ariadneAdjustments = TRUE) {

#To complete dataset and ensure consistency and actuality a number of fixes are applied on the raw source data
browser()
#1: PSI trends in energydtensity are applied to TRACCS car and trucks data for EUR counrties [TRACCS data only available until 2010]
   #Calculate PSI factors for the energy dtensity improvement compared to 2010
   TrendsEndtPSI <- TrendsEndtPSI[region %in% regionEUR & technology %in% c("Liquids", "NG") &
     vehicleType %in% c("Large Car and SUV", "Compact Car", "Subcompact Car","Midsize Car") & period >= 2010]
   TrendsEndtPSI[, factor := value/value[period == 2010], by = c("region", "unit", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName")]
   TrendsEndtPSI[, value := NULL]
   dt <- merge(dt, TrendsEndtPSI, all = TRUE, by = c("region", "unit", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName", "period"))
   #Multiply constant extrapolated 2010 TRACCS values with calculated factor
   dt[, value := ifelse(!is.na(factor), value * factor, value)]

#2: Adjustments made by Alois in consequence of the ARIADNE model intercomparison in 2022
   if (ariadneAdjustments) {
     dt[region == "DEU" & subsectorL1 == "trn_pass_road_LDV_4W" & technology == "Liquids",
         value := value * 2.4/2.85]
     dt[region == "DEU" & subsectorL1 == "trn_pass_road_LDV_4W" & technology == "BEV",
         value := value * 0.77/0.94]
     dt[region == "DEU" & subsectorL1 == "trn_pass_road_LDV_4W" & technology == "FCEV",
         value := value * 1.41/1.75]
     dt[region == "DEU" & subsectorL1 == "trn_pass_road_LDV_4W" & technology == "Hybrid Electric",
         value := value * 1.86/1.3]

     ## we apply all constant from 2020 and adjust for DLR improvements (PSI improvements
     ## are calculated on sales only and are thus too ambitious)
     dt[region == "DEU" & subsectorL1 == "trn_pass_road_LDV_4W" & year >= 2020,
         value := .SD[year == 2020]$value, by = c("vehicle_type", "technology")]

     improvements <- fread("technology,year,factor
        Liquids,2030,0.94
        Liquids,2045,0.89
        BEV,2030,0.94
        BEV,2045,0.86
        FCEV,2030,0.83
        FCEV,2045,0.75")

     dt[region == "DEU" & subsectorL1 == "trn_pass_road_LDV_4W",
         factor := improvements[.SD, factor, on=c("technology", "year")]]
     dt[region == "DEU" & subsectorL1 == "trn_pass_road_LDV_4W" & year <= 2020, factor := 1]
     dt[region == "DEU" & subsectorL1 == "trn_pass_road_LDV_4W",
         factor := na.approx(factor, x=year, rule=2), by=c("region", "vehicle_type", "technology")]
     dt[region == "DEU" & subsectorL1 == "trn_pass_road_LDV_4W", value := value * factor]
     dt[, factor := NULL]

     ## the dtensity deviation is likely coming from a deviation in LF and size shares
     dt[region == "DEU" & subsectorL3 == "trn_freight_road" & technology == "Liquids",
         value := value * 1.4/1.6]
     dt[region == "DEU" & subsectorL3 == "trn_freight_road" & technology == "BEV",
         value := value * 2.94/2.3]
     dt[region == "DEU" & subsectorL3 == "trn_freight_road" & technology == "FCEV",
         value := value * 4.7/4.4]
    }

#3: Assume missing data
#a) Hydrogen airplanes are not covered by our datasources

   #a) based on "A review on potential use of hydrogen in aviation applications", Dincer, 2016:
   # the energy intensity of a hydrogen airplane is around 1MJ/pkm. The range of energy intensity of a fossil-based airplane
   # is here around 3-2 MJ/pkm->a factor of 0.5 is assumed
   dt <- rbind(dt, dt[subsectorL3 %in% c("Domestic Aviation")][, c("value", "technology") := list(0.5 * value, "Hydrogen")])





}
