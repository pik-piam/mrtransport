#' Perform source specific transformations to ensure a compatible structure.
#'
#' Map the source categories to the EDGE-T categories. Apply the full logit structure.
#'
#' @author Johanna Hoppe
#' @param x the input data read via readSource, a magpie object
#' @return a quitte object
#'
#' @import data.table
#' @importFrom rmndt magpie2dt
#' @export

toolPreparePSI <- function(x) {
  variable <- .   <- technology <- period <- value <-
    univocalName  <- unit <- region  <- vehicleTypePSI <-
    technologyPSI <- PSI <- NULL

  mapfile <- system.file("extdata", "mappingPSItoEDGET.csv",
   package = "mrtransport", mustWork = TRUE)
  mappingPSI <- fread(mapfile, skip = 0)
  setkey(mappingPSI, technologyPSI, vehicleTypePSI)
  dt <- magpie2dt(x)
  dt <- merge.data.table(dt, mappingPSI, all.x = TRUE, by = c("technologyPSI", "vehicleTypePSI"), allow.cartesian = TRUE)
  dt <- dt[!univocalName == ""]
  #Average the energy intensity for petrol and diesel ICEs and PHEVs
  dt <- dt[, .(value = mean(value)),
           by = c("region", "period", "univocalName", "technology", "variable", "unit")]
  setkey(dt, region, univocalName, technology, variable, unit, period)

  if (anyNA(dt) == TRUE) {
    stop("PSI data contains NAs")
  }
  return(dt)
}
