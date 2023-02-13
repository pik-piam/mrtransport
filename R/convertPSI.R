#' Convert PSI data to iso country.
#'
#' @param x a magpie data object
#' @param subtype One of the possible subtypes, see default argument.
#' @return magpie object
#'
#' @examples
#' \dontrun{
#' a <- readSource("PSI", subtype = "costs")
#' }
#' @author Johanna Hoppe
#' @seealso \code{\link{readSource}}
#' @importFrom madrat toolAggregate getISOlist calcOutput readSource
#' @importFrom magclass getYears getItems getSets getItems<- getSets<-
#' @importFrom rmndt magpie2dt
#' @importFrom data.table `:=` fread
#' @export
convertPSI <- function(x, subtype) {
  PSI2iso <- fread("~/Git_repos/mredgeTransport/inst/extdata/regionmapping21EU11.csv")
  #PSI data is mapped on EUR iso countries - Note that we do not have regionally differentiated data
  #PSI2isoMapFile <- system.file("extdata", "regionmapping21EU11.csv",
  #package = "mredgeTransport", mustWork = TRUE)
  #PSI2iso = fread(PSI2isoMapFile, skip = 0)
  setnames(PSI2iso, c("CountryCode", "missingH12"), c("iso", "region"))
  PSI2iso <- PSI2iso[, c("region", "iso")]
  PSI2iso[, region := "GLO"]
  x <- toolAggregate(x, rel = PSI2iso)
  return(x)
}
