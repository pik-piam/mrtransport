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
#' @importFrom magclass getItems getSets
#' @export

convertUCD <- function(x, subtype) {
  gdp <- UCD2isoMapFile <- UCD2iso <- PJToMJ <- NULL

  gdp <- calcOutput("GDP", aggregate = FALSE)
  gdp <- gdp[, getYears(x),  "gdp_SSP2"]

  UCD2isoMapFile <- system.file("extdata", "isoUCD.csv",
   package = "mredgetransport", mustWork = TRUE)
  UCD2iso <- fread(UCD2isoMapFile, skip = 0)

  if (subtype == "feDemand"){
    x <- toolAggregate(x, rel = UCD2iso, weight = gdp)
    #Convert PJ to MJ
    PJToMJ <- 1e9
    x <- x * PJToMJ
    getItems(x, dim = "unit") <- "MJ"
  } else if (subtype == "nonMotorizedDemand"){
    x <- toolAggregate(x, rel = UCD2iso, weight = gdp)
  } else {
    x <- toolAggregate(x, rel = UCD2iso)
  }

  getSets(x)["d1.1"] <- "region"
  return(x)
  }
