#' Convert UCD road transportation data to iso country.
#'
#' @param subtype One of the possible subtypes, see default argument.
#' @return magclass object
#'
#' @examples
#' \dontrun{
#' a <- readSource("UCD")
#' }
#' @author Alois Dirnaichner
#' @seealso \code{\link{readSource}}
#' @importFrom data.table fread
#' @importFrom madrat toolAggregate
convertUCD <- function(x, subtype) {

  UCD2iso <- fread("~/Git_repos/mredgeTransport/inst/extdata/isoUCD.csv")
  #gdp <- calcOutput("GDP", aggregate = FALSE)
  gdp <- readRDS("C:/Users/johannah/Documents/EDGE-Transport/EDGE-T_standalone_InputData/gdp.RDS")
  gdp <- gdp[, getYears(x),  "gdp_SSP2"]
  #UCD2isoMapFile <- system.file("extdata", "isoUCD.csv",
   #package = "mredgeTransport", mustWork = TRUE)
  #UCD2iso = fread(UCD2isoMapFile, skip = 0)
  if (subtype %in% c("feDemand", "nonMotorizedDemand")) {
    x <- toolAggregate(x, rel = UCD2iso, weight = gdp)
  } else {
    x <- toolAggregate(x, rel = UCD2iso)
  }
  getSets(x)["d1.1"] <- "region"
  return(x)
  }
