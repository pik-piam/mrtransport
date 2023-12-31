#' Convert GCAM transportation data to iso country level.
#'
#' @param x a magpie data object
#' @param subtype One of the possible subtypes, see default argument.
#' @return magpie object
#'
#' @examples
#' \dontrun{
#' a <- readSource("GCAM", subtype = "esDemand")
#' }
#' @author Johanna Hoppe, Alois Dirnaichner
#' @seealso \code{\link{readSource}}
#' @import data.table
#' @importFrom madrat toolAggregate getISOlist calcOutput readSource
#' @importFrom magclass getYears getItems getSets getItems<- getSets<-
#' @importFrom rmndt magpie2dt
#' @export

convertGCAM <- function(x, subtype) {
  region <- NULL

  GCAM2iso <- fread(system.file("extdata", "isoGCAM.csv", package = "mrtransport"))
  gdp <- calcOutput("GDP", aggregate = FALSE)
  gdp <- gdp[, getYears(x),  "gdp_SSP2"]
  getItems(x, dim = 1) <- gsub("_", " ", getItems(x, dim = 1), fixed = TRUE) # nolint: object_usage_linter

  if (subtype == "histESdemand") {
    #extensive variables need a weight for disaggregation
    x <- toolAggregate(x, rel = GCAM2iso, weight = gdp)
    #convert unit from million (t|p)km to billion (t|p)km
    millionToBillion <- 0.001
    x <- x * millionToBillion
    getItems(x, dim = "unit") <- c("billion pkm/yr", "billion tkm/yr")       # nolint: object_usage_linter
  } else if (subtype %in% c("energyIntensity", "loadFactor", "speedMotorized", "PPPtoMERfactor")) {
    #intensive variables do not need a weight for disaggregation
    x <- toolAggregate(x, rel = GCAM2iso)
  } else if (subtype == "speedNonMotorized") {
    # data is not region specific and is applied here to all iso countries similarly
    IsoCountries <- as.data.table(getISOlist(type = "all"))
    IsoCountries[, region := "iso"]
    dt <- magpie2dt(x)
    dt[, region := "iso"]
    dt <- merge.data.table(dt, IsoCountries, allow.cartesian = TRUE)
    dt[, region := NULL]
    setnames(dt, c("V1"), c("region"))
    dt <- dt[, c("region", "tranSubsector", "supplysector", "variable", "unit", "value")]
    x <- as.magpie(as.data.frame(dt), spatial = "region", temporal = 0)
  } else if (subtype == "valueOfTimeMultiplier") {
    # data is not region specific and is applied here to all iso countries similarly
    IsoCountries <- as.data.table(getISOlist(type = "all"))
    IsoCountries[, region := "iso"]
    dt <- magpie2dt(x)
    dt[, region := "iso"]
    dt <- merge.data.table(dt, IsoCountries, allow.cartesian = TRUE)
    dt[, region := NULL]
    setnames(dt, "V1", "region")
    dt <- dt[, c("region", "tranSubsector", "supplysector", "variable", "unit", "value")]
    x <- as.magpie(as.data.frame(dt), spatial = "region", temporal = 0)
  }
  return(x)
}
