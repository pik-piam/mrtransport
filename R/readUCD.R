#' Read UCD road transportation data.
#'
#'
#' @param subtype One of the possible subtypes, see default argument.
#' @return magclass object
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
  db <- fread("UCD/UCD_transportation_database.csv", header = TRUE)
  switch(
    subtype,
    "energyIntensity" = {
      ret <- db[variable == "intensity"]
      ret[, unit := "MJ/vehkm"]
    },
    "feDemand" = {
      ret <- db[variable == "energy"]
    },
    "loadFactor" = {
      ret <- db[variable == "load factor"]
    },
    "annualMileage" = {
      ret <- db[variable == "annual travel per vehicle"]
    },
    "nonMotorizedDemand" = {
      ret <- db[variable == "service output"]
    },
    "speed" = {
      ret <- db[variable == "speed"]
    },
    "costsPerVeh" = {
      ret <- db[unit == "2005$/veh"]
    },
    "costsPerVehPerYear" = {
      ret <- db[unit == "2005$/veh/yr"]
    },
    "costsPerVehkm" = {
      ret <- db[unit == "2005$/vkt"]
    }
  )

  ret <- data.table::melt(ret, id.vars = c("UCD_region", "UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel", "variable", "unit"), variable.name = "year")
  setnames(ret, "size.class", "size_class")
  return(as.magpie(ret, spatial = "UCD_region"))

}
