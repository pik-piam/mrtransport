#' Perform source specific transformations to ensure a compatible structure.
#'
#' Map the source categories to the EDGE-T categories. Apply the full structure of the decision tree.
#'
#' @author Johanna Hoppe
#' @param x the input data read via readSource, a magpie object
#' @param subtype one of the different EDGE-T inputdata sources
#' @return a quitte object
#'
#' @importFrom rmndt magpie2dt
#' @importFrom quitte as.quitte
#' @importFrom data.table setnames fread `:=`
#' @export

toolPrepareGCAM <- function(x, subtype) {
  dt <- mapfile <- mappingGCAM <- weight <-
    esdem <- sector <- value <- . <- period <- convBTUtoMJ <-
       unit <- NULL

  dt <- magpie2dt(x)
  mapfile <- system.file("extdata", "mappingGCAMtoEDGET.csv", package = "mredgetransport", mustWork = TRUE)
  mappingGCAM <- fread(mapfile)

  switch(
    subtype,
    "energyIntensity" = {
      weight <- readSource("GCAM", subtype = "histESdemand")
      weight <- magpie2dt(weight)[, c("variable", "unit") := NULL]
      setnames(weight, "value", "esdem")

      setnames(dt, c("tranSubsector", "stub_technology"), c("subsector", "technology"))
      dt <- weight[dt, on = c("region", "period", "subsector", "technology")]
      # some technologies have zero or no demand for certain countries
      #-> set to 1 so that they are equally considered
      dt[is.na(esdem) | esdem == 0, esdem := 1]

      dt <- mappingGCAM[dt, on = c(GCAMsector = "sector", GCAMsubsector = "subsector", GCAMtechnology = "technology")]
      #GCAM data partly contains aggregated values for different levels of the decision tree -> take only the lowest level
      dt <- dt[!is.na(sector)]

      dt <- dt[, .(value = sum(value * esdem) / sum(esdem)), by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology",
                                                                    "univocalName", "variable", "unit", "period")]

      #unit conversion from Mbtu/vehkm to MJ/vehkm
      convBTUtoMJ <- 1.055e-3
      dt[, value := value * convBTUtoMJ][, unit := "MJ/vehkm"]
      dt <- dt[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "univocalName", "variable", "unit", "period", "value")]
      setkey(dt, region, sector, subsectorL1, subsectorL2, subsectorL3, vehicleType, technology, univocalName, variable, unit, period)
      },
    "loadFactor" = {
      weight <- readSource("GCAM", subtype = "histESdemand")
      weight <- magpie2dt(weight)[, c("variable", "unit") := NULL]
      setnames(weight, "value", "esdem")

      setnames(dt, c("tranSubsector", "stub_technology"), c("subsector", "technology"))
      dt <- weight[dt, on = c("region", "period", "subsector", "technology")]
      # some technologies have zero or no demand for certain countries
      #-> set to 1 so that they are equally considered
      dt[is.na(esdem) | esdem == 0, esdem := 1]

      dt <- mappingGCAM[dt, on = c(GCAMsector = "sector", GCAMsubsector = "subsector", GCAMtechnology = "technology")]
      #GCAM data partly contains aggregated values for different levels of the decision tree -> take only the lowest level
      dt <- dt[!is.na(sector)]

      dt <- dt[, .(value = sum(value * esdem) / sum(esdem)), by = c("region", "sector", "subsectorL1",
                                                                    "subsectorL2", "subsectorL3", "vehicleType", "technology", "univocalName", "variable", "unit", "period")]
      dt[sector %in% c("trn_pass", "trn_aviation_intl"), unit := "p/veh"]
      dt[sector %in% c("trn_freight", "trn_shipping_intl"), unit := "t/veh"]
      dt <- dt[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "univocalName", "variable", "unit", "period", "value")]
      setkey(dt, region, sector, subsectorL1, subsectorL2, subsectorL3, vehicleType, technology, univocalName, variable, unit, period)
    },
    "histESdemand" = {
      #use only historical demand
      dt <- dt[period %in% c(1990, 2005, 2010)]
      #map
      dt <- mappingGCAM[dt, on = c(GCAMsector = "sector", GCAMsubsector = "subsector", GCAMtechnology = "technology")]
      #GCAM data partly contains aggregated values for different levels of the decision tree -> take only the lowest level
      dt <- dt[!is.na(sector)]
      #aggregate
      dt <- dt[, .(value = sum(value)), by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology",
                                               "univocalName", "variable", "unit", "period")]
      dt <- dt[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "univocalName", "variable", "unit", "period", "value")]
      setkey(dt,  region, sector, subsectorL1, subsectorL2, subsectorL3, vehicleType, technology, univocalName, variable, unit, period)
      },
    "speedMotorized" = {
      #weights are needed for GCAM vehicle types that are mapped on the same EDGE-T vehicle type
      weight <- readSource("GCAM", subtype = "histESdemand")
      weight <- magpie2dt(weight)
      #Speed is not differentiated between different technologies -> aggregate weights (ES demand) to VehicleType level
      weight <- weight[, .(value = sum(value)),
               by = c("region", "period", "sector", "subsector")]
      setnames(weight, "value", "esdem")
      setnames(dt, c("supplysector", "tranSubsector"), c("sector", "subsector"))
      dt <- weight[dt, on = c("region", "period", "sector", "subsector")]
      # some technologies have zero or no demand for certain countries
      #-> set to 1 so that they are equally considered
      dt[is.na(esdem) | esdem == 0, esdem := 1]
      #GCAM data for speed of modes is not technology specific
      mappingGCAM <- mappingGCAM[, c("GCAMtechnology", "technology") := NULL]
      mappingGCAM <- unique(mappingGCAM)
      dt <- mappingGCAM[dt, on = c(GCAMsector = "sector", GCAMsubsector = "subsector")]
      dt <- dt[sector == "trn_pass"]
      dt <- dt[, .(value = sum(value * esdem) / sum(esdem)),
               by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "univocalName", "variable", "unit", "period")]
      dt <- dt[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "univocalName", "variable", "unit", "period", "value")]
      setkey(dt,  region, sector, subsectorL1, subsectorL2, subsectorL3, vehicleType, univocalName, variable, unit, period)
      },
    "speedNonMotorized" = {
      setnames(dt, c("supplysector", "tranSubsector"), c("sector", "subsector"))
      #GCAM data for speed of modes is not technology specific
      mappingGCAM <- mappingGCAM[, c("GCAMtechnology", "technology") := NULL]
      mappingGCAM <- unique(mappingGCAM)
      dt <- mappingGCAM[dt, on = c(GCAMsector = "sector", GCAMsubsector = "subsector")]
      dt <- dt[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType",
                     "univocalName", "variable", "unit", "value")]
      dt <- dt[subsectorL1 %in% c("Cycle", "Walk")]
      setkey(dt,  region, sector, subsectorL1, subsectorL2, subsectorL3, vehicleType, univocalName, variable, unit)
    },
    "valueOfTimeMultiplier" = {
      #weights are needed for GCAM vehicle types that are mapped on the same EDGE-T vehicle type
      weight <- readSource("GCAM", subtype = "histESdemand")
      weight <- magpie2dt(weight)
      #Speed is not differentiated between different technologies -> aggregate weights (ES demand) to VehicleType level
      weight <- weight[, .(value = sum(value)),
                       by = c("region", "period", "sector", "subsector")]
      setnames(weight, "value", "esdem")
      setnames(dt, c("supplysector", "tranSubsector"), c("sector", "subsector"))
      #data has no temporal resolution
      dt[, year := NULL]
      #Choose weight using ES demand for 2010
      weight <- weight[period == "2010"][, period := NULL]
      dt <- weight[dt, on = c("region", "sector", "subsector")]
      # some technologies have zero or no demand for certain countries
      #-> set to 1 so that they are equally considered
      dt[is.na(esdem) | esdem == 0, esdem := 1]
      #GCAM data for speed of modes is not technology specific
      mappingGCAM <- mappingGCAM[, c("GCAMtechnology", "technology") := NULL]
      mappingGCAM <- unique(mappingGCAM)
      dt <- mappingGCAM[dt, on = c(GCAMsector = "sector", GCAMsubsector = "subsector")]
      dt <- dt[sector == "trn_pass"]
      dt <- dt[, .(value = sum(value * esdem) / sum(esdem)),
               by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "univocalName", "variable", "unit")]
      dt <- dt[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "univocalName", "variable", "unit", "value")]
      setkey(dt,  region, sector, subsectorL1, subsectorL2, subsectorL3, vehicleType, univocalName, variable, unit)
    }
  )
  if (anyNA(dt) == TRUE) {
    stop("GCAM data contains NAs")
  }
  return(dt)
}
