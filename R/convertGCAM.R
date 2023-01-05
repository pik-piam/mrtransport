#' Convert GCAM road transportation data to iso country.
#'
#' @param magpieobj a magpie data object
#' @param subtype One of the possible subtypes, see default argument.
#' @return magpie object
#'
#' @examples
#' \dontrun{
#' a <- readSource("GCAM", subtype = "esDemand")
#' }
#' @author Johanna Hoppe, Alois Dirnaichner
#' @seealso \code{\link{readSource}}
#' @importFrom madrat toolAggregate getISOlist calcOutput readSource
#' @importFrom magclass getYears getItems getSets getItems<- getSets<-
#' @importFrom rmndt magpie2dt
#' @importFrom data.table ':=' fread
convertGCAM <- function(magpieobj, subtype) {

  magpieobj <- subtype <- GCAM2iso <- gdp <- IsoCountries <- country <- NULL

  GCAM2iso <- fread(system.file("extdata", "isoGCAM.csv", package = "mredgeTransport"))
  gdp <- calcOutput("GDP", aggregate = FALSE)[, getYears(magpieobj),  "gdp_SSP2"]
  getItems(magpieobj, dim = 1) <- gsub("_", " ", getItems(magpieobj, dim = 1), fixed = TRUE)
  if (subtype == "histEsDemand") {
    #extensive variables need a weight for disaggregation
    magpieobj <- toolAggregate(magpieobj, rel = GCAM2iso, weight = gdp)
    getSets(magpieobj)["d1.1"] <- "iso"
  } else if (subtype %in% c("feVkmIntensity", "loadFactor", "speedMotorized")) {
    #intensive variables do not need a weight for disaggregation
    magpieobj <- toolAggregate(magpieobj, rel = GCAM2iso)
    getSets(magpieobj)["d1.1"] <- "iso"
  } else if (subtype == "speedNonMotorized") {
    # data is not region specific and is applied here to all iso countries similarly
    IsoCountries <- as.data.table(getISOlist(type = "all"))
    IsoCountries[, country := "iso"]
    magpieobj <- magpie2dt(magpieobj)
    magpieobj[, country := "iso"]
    magpieobj <- merge(magpieobj, IsoCountries, allow.cartesian = TRUE)
    magpieobj[, country := NULL]
    setnames(magpieobj, "IsoCountries", "iso")
    magpieobj <- as.magpie(magpieobj)
  }
  return(magpieobj)
}
