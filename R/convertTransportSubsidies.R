#' Converts transport subsidies data
#' 
#' @param x MAgPIE object to be converted
#' @return A MAgPIE object containing transport subsidies per technology
#' @author Renato Rodrigues
#' @examples
#' 
#' \dontrun{ a <- convertTransportSubsidies(x)
#' }
#'  

convertTransportSubsidies <- function(x) {
  
  x  <- toolCountryFill(x,fill=NA,verbosity=2) # fill countries with no data 
  #x[is.na(x)] <- 0
  
  return(x)
}
