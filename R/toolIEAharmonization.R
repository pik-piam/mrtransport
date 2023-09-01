#' Harmonize the energy intensities to match the IEA energy balances regarding final energy.
#'
#' We provide energy service trajectories. IEA energy balances have to be met and are not
#' consistent with GCAM intensities and energy service trajectories.
#' Therefore we have to adjust energy intensities.
#'
#' @param enIntensity energy intensity
#' @importFrom rmndt magpie2dt
#' @export

toolIEAharmonization <- function(enIntensity) {
 fe <- te <- period <- isBunk <- flow <- . <- feIEA <- region <- subsectorL1 <- univocalName <-
   vehicleType <- value <- enService <- harmFactor <- check <- technology <- sector <-
      subsectorL2 <- subsectorL3 <- variable <- unit <- NULL

 # Load IEA energy balances data for harmonization [unit: EJ]
 IEAbalMag <- calcOutput(type = "IO", subtype = "IEA_output", aggregate = FALSE)
 IEAbal <-  magpie2dt(IEAbalMag, datacols = c("se", "fe", "te", "mod", "flow"),
                      regioncol = "region", yearcol = "period")
 # Select only fuel types that are represented in EDGE-T
 IEAbal <- IEAbal[fe %in% c("fedie", "fepet", "fegat", "feelt")]
 IEAbal <- IEAbal[te != "dot"]  #delete fedie.dot #Q: what is fedie.dot?
 setnames(IEAbal, "value", "feIEA")

 # Year for hamonization is set to 2005 (important for functionality of REMIND)
 IEAbal <- IEAbal[period == 2005]
 # As freight and passenger are not seperated in IEA energy balances harmonize by "short-medium",
 # "MARBUNK" (eq. to shipping international) and "AVBUNK" (eq. to aviation international) and technology (te)
 IEAbal[, isBunk := ifelse(grepl("BUNK", flow), flow, "short-medium")]
 IEAbal[, c("se", "fe", "mod", "flow") := NULL]
 # sum fossil liquids and biofuel to tdlit, and biogas and natural gas to tdgat
 IEAbal[te %in% c("tdfospet", "tdfosdie", "tdbiopet", "tdbiodie"), te := "tdlit"]
 IEAbal[te %in% c("tdfosgat", "tdbiogat"), te := "tdgat"]
 IEAbal <- IEAbal[, .(feIEA = sum(feIEA)), by = .(region, period, te, isBunk)]

 # Energy intensity is given in [MJ/vehkm] and energy service demand in [bn (t|p)km]
 # Read load factor and energy service demand to calculate final energy
 enServiceDemMag <- calcOutput(type = "EdgeTransportSAinputs", aggregate = FALSE, warnNA = FALSE,
                               subtype = "histESdemand")
 enServiceDem <- magpie2dt(enServiceDemMag)
 enServiceDem[subsectorL1 == "trn_freight_road", univocalName := gsub("_", ".", univocalName)]
 enServiceDem[subsectorL1 == "trn_freight_road", vehicleType := gsub("_", ".", vehicleType)]
 enServiceDem <- enServiceDem[!univocalName %in% c("Cycle", "Walk")]
 setnames(enServiceDem, "value", "enService")
 enServiceDem <- enServiceDem[, c("region", "univocalName", "technology", "period", "enService")]
 loadFactorMag <- calcOutput(type = "EdgeTransportSAinputs", aggregate = FALSE, warnNA = FALSE,
                             subtype = "loadFactor")
 loadFactor <- magpie2dt(loadFactorMag)
 loadFactor[subsectorL1 == "trn_freight_road", univocalName := gsub("_", ".", univocalName)]
 loadFactor[subsectorL1 == "trn_freight_road", vehicleType := gsub("_", ".", vehicleType)]
 loadFactor <- loadFactor[!univocalName %in% c("Cycle", "Walk")]
 setnames(loadFactor, "value", "loadFactor")
 loadFactor <- loadFactor[, c("region", "univocalName", "technology", "period", "loadFactor")]
 enServiceDem <- merge.data.table(enServiceDem, loadFactor, by = c("region", "univocalName", "technology", "period"))

 # merge load factor and energy service demand with energy intensity
 enIntensity <- merge.data.table(enIntensity, enServiceDem,  by = c("region", "univocalName", "technology", "period"),
                                 all.x = TRUE)
 # Calculate final energy in EJ
 MJtoEJ <- 1e-12
 bn <- 1e9
 enIntensity[, fe := (value / loadFactor) * enService * bn * MJtoEJ]
 #Apply IEA categories
 enIntensity[technology %in% c("BEV", "Electric"), te := "tdelt"]
 enIntensity[technology == "NG", te := "tdgat"]
 #all others are handled as liquids (including hybrid electric)
 enIntensity[is.na(te), te := "tdlit"]
 enIntensity[, isBunk := ifelse(sector == "trn_aviation_intl", "AVBUNK", NA)]
 enIntensity[, isBunk := ifelse(sector == "trn_shipping_intl", "MARBUNK", isBunk)]
 enIntensity[, isBunk := ifelse(is.na(isBunk), "short-medium", isBunk)]
 enIntensity[, fe := sum(fe), by = c("region", "isBunk", "te", "period")]

 #Merge enery intensity and actual final energy with IEA data
 enIntensity <- merge.data.table(enIntensity, IEAbal, by = c("region", "isBunk", "te", "period"))
 #For some modes and technologies the IEA fe value is zero (and or our value is zero) -> omitted in the harmonization
 #Note that e.g. NG busses in AUT do have a demand regarding to the IEA data, but in our data there is no demand
 #This leads to a small deviation from our final energy data vs IEA fe data after the harmonization process that
 # is checked and in the end and accepted if not too large
 enIntensity[, harmFactor := ifelse(feIEA == 0 | fe == 0, 0, feIEA / fe)]
 # Harmonization factor of 2005 is taken for all years (To do: test if harmonization in 2005, 2010 and 2015 would be
 # better)
 enIntensity[, harmFactor := harmFactor[period == 2005]]
 enIntensity[, value := value * harmFactor]
 #Check wether hamonization worked
 enIntensity[, check := (value / loadFactor) * enService * bn * MJtoEJ]
 enIntensity[, check := sum(check), by = c("region", "isBunk", "te", "period")][, diff := check - feIEA]
 if (sum(enIntensity$diff > 1e-3) > 0) {
   stop("There is a problem regarding the Harmonization of the energy intensity data to match IEA energy balances
        final energy")
 }

 enIntensity <- enIntensity[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType",
                                "univocalName", "variable", "unit", "period", "value")]
 setkey(enIntensity,  region, sector, subsectorL1, subsectorL2, subsectorL3, vehicleType, univocalName,
        variable, unit, period)

 return(enIntensity)
}
