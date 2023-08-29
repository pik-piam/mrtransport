#' Convert Eurostat road transportation data to iso country.
#'
#' @param x a magpie data object
#' @param subtype One of the possible subtypes, see default argument.
#' @return magpie object
#'
#' @examples
#' \dontrun{
#' a <- readSource("EUROSTAT")
#' }
#' @author Johanna Hoppe
#' @seealso \code{\link{readSource}}
#' @importFrom madrat toolCountry2isocode toolCountryFill
#' @importFrom magclass as.magpie getItems getSets mselect getItems<- getSets<-

convertEUROSTAT <- function(x, subtype) {
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1), mapping = c("EL" = "GRC")) # nolint: object_usage_linter
  getSets(x)["d1.1"] <- "region"                                                               # nolint: object_usage_linter
  #Convert Mtoe to MJ
  MtoeToMJ <- 41868000000
  x <- x * MtoeToMJ
  getItems(x, dim = "unit") <- "MJ"                                                            # nolint: object_usage_linter
  x <- suppressMessages(toolCountryFill(x, fill = NA))                                         # nolint: object_usage_linter
  return(x)
}
