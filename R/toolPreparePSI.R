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
  subsectorL3 <- variable <- . <- subsectorL1 <- subsectorL2 <- technology <- period <- value <-
    univocalName <- vehicleType <- unit <- region <- sector <- technology <- vehicleType <-
    technologyPSI <- vehicleTypePSI <- NULL

  mapfile <- system.file("extdata", "mappingPSItoEDGET.csv",
   package = "mredgetransport", mustWork = TRUE)
  mappingPSI <- fread(mapfile, skip = 0)
  setkey(mappingPSI, technologyPSI, vehicleTypePSI)
  dt <- magpie2dt(x)
  dt[, vehicleTypePSI := gsub("_", ".", vehicleTypePSI)]
  dt <- merge.data.table(dt, mappingPSI, all.x = TRUE, by = c("technologyPSI", "vehicleTypePSI"))
  dt <- dt[!sector == ""]
  #Average the energy intensity for petrol and diesel ICEs and PHEVs
  dt <- dt[, .(value = mean(value)),
           by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType",
                  "technology", "univocalName", "variable", "unit", "period")]
  setkey(dt, region,  sector, subsectorL1, subsectorL2, subsectorL3, vehicleType, technology, univocalName,
         variable, unit, period)

  if (anyNA(dt) == TRUE) {
    stop("PSI data contains NAs")
  }
  return(dt)
}
