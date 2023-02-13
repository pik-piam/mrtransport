#' Perform source specific transformations to ensure a compatible structure.
#'
#' Map the source categories to the EDGE-T categories. Apply the full logit structure.
#'
#' @author Johanna Hoppe, Alois Dirnaichner
#' @param x the input data read via readSource, a magpie object
#' @param subtype one of the different EDGE-T inputdata sources
#' @return a quitte object
#'
#' @importFrom rmndt magpie2dt
#' @importFrom quitte as.quitte
#' @importFrom data.table setnames fread `:=`

toolPrepareGCAM <- function(x, subtype) {

  dt <- mapfile <- mappingGCAM <- weight <- Units <-
    esdem <- sector <- value <- . <- year <- convBTUtoMJ <-
       unit <- NULL

  dt <- magpie2dt(x)
  #mapfile <- system.file("extdata", "mappingGCAMtoEDGET.csv",
                        # package = "mredgeTransport", mustWork = TRUE)
  mapfile <- "C:/Users/johannah/Documents/Git_repos/mredgetransport/inst/extdata/mappingGCAMtoEDGET.csv"
  mappingGCAM <- fread(mapfile)
  switch(
    subtype,
    "energyIntensity" = {
      #weight <- readSource("GCAM", subtype = "histEsDemand")
      weight <- readGCAM(subtype = "histEsDemand")
      weight <- convertGCAM(weight, "histEsDemand")
      weight <- magpie2dt(weight)[, Units := NULL]
      setnames(weight, "value", "esdem")

      setnames(dt, c("tranSubsector", "stub_technology"), c("subsector", "technology"))
      dt <- weight[dt, on = c("region", "year", "subsector", "technology")]
      # some technologies have zero or no demand for certain countries
      #-> set to 1 so that they are equally considered
      dt[is.na(esdem) | esdem == 0, esdem := 1]

      dt <- mappingGCAM[dt, on = c(GCAMsector = "sector", GCAMsubsector = "subsector", GCAMtechnology = "technology")]
      dt <- dt[!is.na(sector)]

      dt <- dt[, .(value = sum(value * esdem) / sum(esdem)), by = c("region", "year", "sector", "subsectorL3",
                                                                    "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName")]

      #unit conversion from Mbtu/vehkm to MJ/vehkm
      convBTUtoMJ <- 1.055e-3
      dt[, value := value * convBTUtoMJ][, unit := "MJ/vehkm"]

      dt <- dt[, c("region", "year", "unit", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName", "value")]
      setkey(dt, region,  sector, subsectorL3, subsectorL2, subsectorL1, vehicleType, technology, year, unit, univocalName)

      },
    "loadFactor" = {
      #weight <- readSource("GCAM", subtype = "histEsDemand")
      weight <- readGCAM(subtype = "histEsDemand")
      weight <- convertGCAM(weight, "histEsDemand")
      weight <- magpie2dt(weight)[, Units := NULL]
      setnames(weight, "value", "esdem")

      setnames(dt, c("tranSubsector", "stub_technology"), c("subsector", "technology"))
      dt <- weight[dt, on = c("region", "year", "subsector", "technology")]
      # some technologies have zero or no demand for certain countries
      #-> set to 1 so that they are equally considered
      dt[is.na(esdem) | esdem == 0, esdem := 1]

      dt <- mappingGCAM[dt, on = c(GCAMsector = "sector", GCAMsubsector = "subsector", GCAMtechnology = "technology")]
      dt <- dt[!is.na(sector)]

      dt <- dt[, .(value = sum(value * esdem) / sum(esdem)), by = c("region", "year", "sector", "subsectorL3",
                                                                    "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName")]
      dt[sector %in% c("trn_pass", "trn_aviation_intl"), unit := "p/veh"]
      dt[sector %in% c("trn_freight", "trn_shipping_intl"), unit := "t/veh"]
      dt <- dt[, c("region", "year", "unit", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName", "value")]
      setkey(dt, region,  sector, subsectorL3, subsectorL2, subsectorL1, vehicleType, technology, year, unit, univocalName)
    },
    "histEsDemand" = {
      #use only historical demand
      dt <- dt[year %in% c(1990, 2005, 2010)]
      #map
      dt <- mappingGCAM[dt, on = c(GCAMsector = "sector", GCAMsubsector = "subsector", GCAMtechnology = "technology")]
      dt <- dt[!is.na(sector)]
      #aggregate
      dt <- dt[, .(value = sum(value)), by = c("sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType",
                                               "technology", "univocalName", "region", "year", "Units", "univocalName")]
      #convert to quitte
      setnames(dt, c("Units", "year"), c("unit", "period"))
      #rename units
      dt[, unit := gsub("million pass-km", "million pkm", unit)]
      dt[, unit := gsub("million ton-km", "million tkm", unit)]

      dt <- dt[, c("region", "year", "unit", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName", "value")]
      setkey(dt, region,  sector, subsectorL3, subsectorL2, subsectorL1, vehicleType, technology, year, unit, univocalName)
      },
    "speedMotorized" = {
      #weights are needed for GCAM vehicle types that are mapped on the same EDGE-T vehicle type
      #weight <- readSource("GCAM", subtype = "histEsDemand")
      weight <- readGCAM(subtype = "histEsDemand")
      weight <- convertGCAM(weight, "histEsDemand")
      weight <- magpie2dt(weight)[, Units := NULL]
      #Speed is not differentiated between different technologies -> aggregate weights (ES demand) to VehicleType level
      weight <- weight[, .(value = sum(value)),
               by = c("region", "year", "sector", "subsector")]
      setnames(weight, "value", "esdem")
      setnames(dt, c("supplysector", "tranSubsector"), c("sector", "subsector"))
      dt <- weight[dt, on = c("region", "year", "sector", "subsector")]
      # some technologies have zero or no demand for certain countries
      #-> set to 1 so that they are equally considered
      dt[is.na(esdem) | esdem == 0, esdem := 1]
      #Neglect technology level in GCAM/EDGET mapping
      #Prevent duplicates by filtering out the technologies that anyway do not exist in EDGET
      mappingGCAM <- mappingGCAM[!is.na(technology)]
      mappingGCAM[, c("GCAMtechnology", "technology") := NULL]
      mappingGCAM <- unique(mappingGCAM)
      dt <- mappingGCAM[dt, on = c(GCAMsector = "sector", GCAMsubsector = "subsector")]
      dt <- dt[!is.na(sector)]

      dt <- dt[, .(value = sum(value * esdem) / sum(esdem)),
               by = c("region", "year", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "univocalName")]
      dt[, unit := "km/h"]

      dt <- dt[, c("region", "year", "unit", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "univocalName", "value")]
      setkey(dt, region,  sector, subsectorL3, subsectorL2, subsectorL1, vehicleType, year, unit, univocalName)
      },
  "speedNonMotorized" = {
      setnames(dt, c("supplysector", "tranSubsector"), c("sector", "subsector"))
      dt <- mappingGCAM[dt, on = c(GCAMsector = "sector", GCAMsubsector = "subsector", GCAMtechnology = "technology")]
      dt <- dt[, c("region", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType",
                    "technology", "univocalName", "value")]
      dt[, unit := "km/h"]
      dt <- dt[, c("region", "year", "unit", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName", "value")]
      setkey(dt, region,  sector, subsectorL3, subsectorL2, subsectorL1, vehicleType, technology, year, unit, univocalName)
    },
  "valueOfTimeMultiplier" = {
    setnames(dt, c("supplysector", "tranSubsector"), c("sector", "subsector"))
    mappingGCAM <- mappingGCAM[!is.na(technology)]
    mappingGCAM[, c("GCAMtechnology", "technology") := NULL]
    mappingGCAM <- unique(mappingGCAM)
    dt <- mappingGCAM[dt, on = c(GCAMsector = "sector", GCAMsubsector = "subsector")]
    dt <- unique(dt)
    #convert to quitte
    dt <- dt[, unit := "-"]
    dt <- dt[, c("region", "unit", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType",
                 "univocalName", "value")]
    setkey(dt, region,  sector, subsectorL3, subsectorL2, subsectorL1, vehicleType, unit, univocalName)
    }
  )

  setnames(dt, "year", "period")
  return(dt)

}
