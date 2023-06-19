#' Read UCD road transportation data.
#'
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
#' @importFrom data.table fread
#' @importFrom magclass as.magpie
readEUROSTAT <- function(subtype = c(
                      "feDemand")) {

  countries <- c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE")
  switch(
    subtype,
    "feDemand" = {
      dt <- do.call("rbind",lapply(countries,
                                      function(x) {
                                        output = suppressMessages(data.table(read_excel(path = file.path("Energy statistical country datasheets", "Energy statistical country datasheets 2023-04.xlsx"),
                                                                                        sheet = x,"C8:AI249")))
                                        names(output)[1] <- "variable"
                                        output = output[variable %in% c("International maritime bunkers", "International aviation", "Domestic aviation", "Domestic navigation")]
                                        output = melt(output, id.vars = c("variable"), variable.name = "period")
                                        output$region <- x
                                        output$unit <- "Mtoe"
                                        return(output)
                                      }))
    }
  )

  #Bring to quitte column order
  dt <- dt[, c("region", "variable", "unit", "period",  "value")]
  x <- as.magpie(as.data.frame(dt), spatial = 1)
  return(x)

}
