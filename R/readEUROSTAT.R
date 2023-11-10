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
                      "feDemand", "LDVfleet")) { # added "fleet"
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
                                        #browser()
                                        return(output)
                                      }))
      #Bring to quitte column order
      dt[, variable := "Final energy"]
      dt <- dt[, c("region", "EUROSTATsector", "variable", "unit", "period",  "value")]
      x <- as.magpie(as.data.frame(dt), spatial = "region", temporal = "period")
      browser()
      return(x)
    },
    "LDVfleet" ={ # added this subtype
       dt <- do.call("rbind", lapply( 2:18,
                                        function(x) {
                                          
                                          # load the sheet
                                          output <- suppressMessages(data.table(read_excel(path = 
                                                                              file.path("LDV fleet",
                                                                              "road_eqs_carpda_spreadsheet.xlsx"),
                                                                              sheet = paste("Sheet", toString(x), sep=" "),
                                                                              #rowIndex = 9:52, colIndex = 1:19 )))
                                                                              "A9:R52" )))
                                          # load technology name per sheet
                                          tech <- (suppressMessages(read_excel(path = file.path("LDV fleet",
                                                                                                     "road_eqs_carpda_spreadsheet.xlsx"),
                                                                                         sheet = paste("Sheet", toString(x), sep=" "),
                                                                                         #rowIndex = 9:52, colIndex = 1:19 )))
                                                                                         "C7:C7", col_names = FALSE )))[[1]]
                                          names(output)[1] <- "region"
                                          output <- output[-1,]
                                          output <- output[-1,]
                                          cols = c("region", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")
                                          output <- output[, ..cols]
                                          
                                          # We drop cosovo
                                          output <- output[region != "Kosovo (under United Nations Security Council Resolution 1244/99)"]
                                          
                                          # fill in NAs
                                          output[output == ":"] <- NA
                                          
                                          output <- melt(output, id.vars = c("region"), variable.name = "period")
                                          output$EUROSTATtechnology <- tech
                                          output$EUROSTATvehicleType <- "LDV-4Wheeler"
                                          output$unit <- "veh"
                                          return(output)
                                        } ))
       dt[, variable := "LDV Fleet"]
       dt <- dt[, c("region", "EUROSTATvehicleType", "EUROSTATtechnology", "variable", "unit", "period",  "value")]
       x <- as.magpie(as.data.frame(dt), spatial = "region", temporal = "period")
       return(x)
    },
  )

}
