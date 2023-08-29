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
#' @import data.table
#' @importFrom magclass as.magpie
#' @importFrom readxl read_excel
readEUROSTAT <- function(subtype = c(
                      "feDemand")) {
  EUROSTATsector <- variable <- NULL

  countries <- c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", "ES", "FR", "HR", "IT", "CY",
                 "LV", "LT", "LU", "HU", "MT", "NL", "AT", "PL", "PT", "RO", "SI", "SK", "FI", "SE")
  switch(
    subtype,
    "feDemand" = {
      dt <- do.call("rbind", lapply(countries,
                                      function(x) {
                                        output <- suppressMessages(data.table(read_excel(path =                                 # nolint: object_usage_linter
                                                                             file.path("Energy statistical country datasheets", # nolint: line_length_linter
                                                                             "Energy statistical country datasheets 2023-04.xlsx"), # nolint: line_length_linter
                                                                             sheet = x, "C8:AI249")))
                                        names(output)[1] <- "EUROSTATsector"
                                        output <- output[EUROSTATsector %in% c("International maritime bunkers",
                                                         "International aviation", "Domestic aviation", "Domestic navigation")] # nolint: line_length_linter
                                        output <- melt(output, id.vars = c("EUROSTATsector"), variable.name = "period")
                                        output$region <- x
                                        output$unit <- "Mtoe"
                                        return(output)
                                      }))
    }
  )

  #Bring to quitte column order
  dt[, variable := "Final energy"]
  dt <- dt[, c("region", "EUROSTATsector", "variable", "unit", "period",  "value")]
  x <- as.magpie(as.data.frame(dt), spatial = "region", temporal = "period")
  return(x)

}
