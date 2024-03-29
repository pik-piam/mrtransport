#' Converts transport subsidies data
#'
#' @param x MAgPIE object to be converted
#' @return A MAgPIE object containing transport subsidies per technology
#' @author Caroline Cronjaeger
#' @examples
#'
#' \dontrun{ a <- convertTransportSubsidies(x)
#' }
#'

convertTransportPurchasePriceSubsidies <- function(x) {
  x  <- toolCountryFill(x, fill = NA, verbosity = 2)
  return(x)
}
