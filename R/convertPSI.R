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

  #PSI data is mapped on iso countries - Note that we do not have regionally differentiated data
  PSI2isoMapFile <- system.file("extdata", "regionmapping21.csv",
    package = "edgeTransport", mustWork = TRUE)
  PSI2iso = fread(PSI2isoMapFile, skip = 0)
  setnames(PSI2iso, c("CountryCode"), c("iso"))
  PSI2iso <- PSI2iso[, c("iso")][, region := "GLO"]
  #Here data is mapped on all iso countries -> same vaue according to vehicle class/technology/year
  x <- toolAggregate(x, rel = PSI2iso)

  switch(
    subtype,
    "CAPEX" = {
      #PSI CAPEX need to be transformed to USD2005
      EUR2017toUSD2017 <- 1.14
      USD2017toUSD2005 <- 0.78
      x <- x * EUR2017toUSD2017 * USD2017toUSD2005
      getItems(x, dim = 3.2) <- "US$2005/veh"
    },
    "energyIntensity" = {
      #PSI energy intensity needs to be transformed to MJ/vehkm
      kJPerVehkmtoMJperVehkm <- 1e-3
      x <- x * kJPerVehkmtoMJperVehkm
      getItems(x, dim = 3.2) <- "MJ/vehkm"
    }
  )

  return(x)
}
