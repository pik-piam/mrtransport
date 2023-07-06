#' Read UCD road transportation data.
#'
#'
#' @param subtype One of the possible subtypes, see default argument.
#' @dturn magclass object
#'
#' @examples
#' \dontrun{
#' a <- readSource("UCD")
#' }
#' @author Johanna Hoppe, Alois Dirnaichner
#' @seealso \code{\link{readSource}}
#' @importFrom data.table fread
#' @importFrom magclass as.magpie
readUCD <- function(subtype = c(
                      "energyIntensity", "feDemand", "loadFactor", "annualMileage",
                      "nonMotorizedDemand", "speed", "costs")) {
  variable <- NULL
  UCD <- fread("UCD_transportation_database.csv", header = TRUE)
  switch(
    subtype,
    "energyIntensity" = {
      dt <- UCD[variable == "intensity"]
      dt[, unit := "MJ/vehkm"]
    },
    "feDemand" = {
      #fe data is given only for 2005
      dt <- UCD[variable == "energy"]
      dt[, unit := "PJ/yr"]
    },
    "loadFactor" = {
      dt <- UCD[variable == "load factor"]
      dt[UCD_sector == "Passenger", unit := "p/veh"]
      dt[UCD_sector == "Freight", unit := "t/veh"]
    },
    "annualMileage" = {
      dt <- UCD[variable == "annual travel per vehicle"]
      dt[, unit := "vehkm/veh/yr"]
    },
    "nonMotorizedDemand" = {
      dt <- UCD[variable == "service output"]
      dt[, unit := "bn pkm/yr"]
    },
    "speed" = {
      dt <- UCD[variable == "speed"]
      dt[unit, "km/h"]
    },
    "CAPEX" = {
      dt <- UCD[variable %in% c("CAPEX", "Capital costs (infrastructure)", "Capital costs (other)", "Capital costs (purchase)", "Capital costs (total)")]
    },
    "nonFuelOPEX" = {
      dt <- UCD[variable == "non-fuel OPEX"]
      dt[, unit := "US$2005/veh/yr"]
    },
    "CAPEXandNonFuelOPEX" = {
      dt <- UCD[variable == "CAPEX and non-fuel OPEX"]
      dt[, unit := "US$2005/vehkm"]
    },
    "OperatingSubsidies" = {
      dt <- UCD[variable == "Operating subsidy"]
      dt[, unit := "US$2005/vehkm"]
    }
  )

  dt <- data.table::melt(dt, id.vars = c("UCD_region", "UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel", "variable", "unit"), variable.name = "year")
  setnames(dt, "size.class", "size_class")
  return(as.magpie(dt, spatial = "UCD_region"))

}
