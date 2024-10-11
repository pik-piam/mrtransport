#' Convert UCD road transportation data to iso country.
#'
#' @param x a magpie data object
#' @param subtype One of the possible subtypes, see default argument.
#' @return magpie object
#'
#' @examples
#' \dontrun{
#' a <- readSource("UCD")
#' }
#' @author Johanna Hoppe, Alois Dirnaichner
#' @seealso \code{\link{readSource}}
#' @importFrom data.table fread
#' @importFrom madrat toolAggregate
#' @importFrom magclass getItems getSets getItems<- getSets<-
#' @export

convertUCD <- function(x, subtype) {

  gdp <- calcOutput("GDP", aggregate = FALSE)
  gdp <- gdp[, getYears(x),  "gdp_SSP2"]

  UCD2isoMapFile <- system.file("extdata", "isoUCD.csv",
                                package = "mrtransport", mustWork = TRUE)
  UCD2iso <- fread(UCD2isoMapFile, skip = 0)

  if (subtype == "feDemand") {
    x <- toolAggregate(x, rel = UCD2iso, weight = gdp)
    #Convert PJ to MJ
    PJToMJ <- 1e9
    x <- x * PJToMJ
    getItems(x, dim = "unit") <- "MJ"               # nolint: object_usage_linter
  } else if (subtype == "nonMotorizedDemand") {
    x <- toolAggregate(x, rel = UCD2iso, weight = gdp)
  } else if (subtype %in% c("CAPEX", "nonFuelOPEX",
                            "CAPEXandNonFuelOPEX", "OperatingSubsidies")) {

    x <- toolAggregate(x, rel = UCD2iso)

    # convert US$2005 to US$20217
    y <- GDPuc::toolConvertGDP(
      gdp = x,
      unit_in = "constant 2005 US$MER",
      unit_out = mrdrivers::toolGetUnitDollar(),
      replace_NAs = "with_USA"
    )

    magclass::getNames(x) <- gsub("US\\$2005", "US$2017", magclass::getNames(x))

  } else {
    x <- toolAggregate(x, rel = UCD2iso)
  }

  getSets(x)["d1.1"] <- "region"                  # nolint: object_usage_linter
  return(x)
}
