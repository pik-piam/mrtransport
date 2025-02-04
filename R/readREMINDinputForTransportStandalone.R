#' Load fuel prices from a REMIND fulldata.gdx in USD/MJ
#'
#' @export
readREMINDinputForTransportStandalone <- function() {
  gdxPath <- file.path("v1.2", "fulldata.gdx")
  out <- gdx::readGDX(gdxPath, "pm_FEPrice", format = "first_found",
                      restore_zeros = FALSE)[, , "trans.ES", pmatch = TRUE]
  return(out)
}
