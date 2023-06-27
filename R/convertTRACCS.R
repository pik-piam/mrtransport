#' Convert TRACCS road transportation data to iso country.
#'
#' @param subtype One of the possible subtypes, see default argument.
#' @return magclass object
#'
#' @examples
#' \dontrun{
#' a <- readSource("TRACCS")
#' }
#' @author Alois Dirnaichner
#' @seealso \code{\link{readSource}}
#' @importFrom madrat toolCountry2isocode

convertTRACCS <- function(x, subtype) {

  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))
  getSets(x)["d1.1"] <- "region"

  if (subtype == "histEsDemand"){
   #convert unit from million (t|p)km to billion (t|p)km
   millionToBillion <- 0.001
   x <- x * millionToBillion
   getItems(x, dim = 3.4) <- c("billion tkm/yr", "billion pkm/yr")
   }
  #TRACCS data is provided only on EUR -> toolCountryFill notes that important countries are filled with NA.
  #This is considered, data fo these important countries is taken from other sources. Therefore, the note is supressed
  x <- suppressMessages(toolCountryFill(x, fill = NA))
  return(x)
}

