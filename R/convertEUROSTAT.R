#' Convert Eurostat road transportation data to iso country.
#'
#' @param subtype One of the possible subtypes, see default argument.
#' @return magclass object
#'
#' @examples
#' \dontrun{
#' a <- readSource("EUROSTAT")
#' }
#' @author Johanna Hoppe
#' @seealso \code{\link{readSource}}
#' @importFrom madrat toolCountry2isocode
#' @importFrom magclass as.magpie getItems getSets mselect

convertEUROSTAT <- function(x, subtype) {
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1), mapping=c("EL"="GRC"))
  getSets(x)["d1.1"] <- "region"
  #Convert Mtoe to MJ
  MtoeToMJ <- 41868000000
  x <- x * MtoeToMJ
  getItems(x, dim = 3.2) <- "MJ"
  x <- suppressMessages(toolCountryFill(x, fill = NA))
  return(x)
}

