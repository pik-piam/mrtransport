#' Read GCAM road transportation data.
#'
#'
#' @param subtype One of the possible subtypes, see default argument.
#' @return magpie object
#'
#' @exaxles
#' \dontrun{
#' a <- readSource("GCAM", subtype="esDemand")
#' }
#' @author Johnna Hoppe, Alois Dirnaichner
#' @seealso \code{\link{readSource}}
#' @importFrom data.table melt fread setnames `:=`
#' @importFrom magclass as.magpie
#' @importFrom magrittr `%>%`
#' @export
readGCAM <- function(subtype = c(
  "feVkmIntensity",  "loadFactor", "histEsDemand", "speedMotorized",
  "speedNonMotorized", "valueOfTimeMultiplier")) {
  market.name <- dt <- x <- value <- speed_source <- . <- supplysector <- share_weight <-
    scenario <- NULL

  switch(
    subtype,
    "energyIntensity" = {
      dt <- fread("GCAM/L254.StubTranTechCoef.csv", skip = 4)[, market.name := NULL]
      setnames(dt, gsub(".", "_", colnames(dt), fixed = TRUE))
      x <- as.magpie(as.data.frame(dt), temporal = 5, spatial = 1)
    },
    "loadFactor" = {
      dt <- fread("GCAM/L254.StubTranTechLoadFactor.csv", skip = 4)
      setnames(dt, gsub(".", "_", colnames(dt), fixed = TRUE))
      x <- as.magpie(as.data.frame(dt), temporal = 5, spatial = 1)
    },
    "histEsDemand" = {
      dt <- fread("GCAM/tech_output.csv", skip = 1, sep = ";", header = TRUE) %>%
      melt(measure.vars = 6:26, variable.name = "year")
      dt[, scenario := NULL]
      x <- as.magpie(as.data.frame(dt), temporal = 6, spatial = 1)
    },
    "speedMotorized" = {
      dt <- fread("GCAM/L254.tranSubsectorSpeed.csv", skip = 4)
      #data includes a lot of duplicates
      dt <- dt[!duplicated(dt, by = c("region", "supplysector", "tranSubsector", "year"))]
      x <- as.magpie(as.data.frame(dt), temporal = 4, spatial = 1, datacol = 5)
    },
    "speedNonMotorized" = {
      dt <- fread("GCAM/A54.globaltech_nonmotor.csv", skip = 1, sep = ",", header = TRUE)
      dt[, c("renewable.input", "share.weight") := NULL]
      x <- as.magpie(dt, temporal = 0, datacol = 3)
    },
    "valueOfTimeMultiplier" = {
      dt <- fread("GCAM/A54.tranSubsector_VOTT.csv", skip = 1)[!grepl("#", supplysector)] %>%
        unique() %>%
        setnames(gsub(".", "_", colnames(.), fixed = TRUE))
      dt[, c("speed_source", "fuelprefElasticity", "addTimeValue", "wait_walk_vott", "wait_walk_share", "in_vehicle_VOTT") := NULL]
      dt <- melt(dt, id.vars = c("supplysector", "tranSubsector"), na.rm = TRUE)
      dt[, value := as.numeric(value)]
      x <- as.magpie(dt)
    }
  )

  return(x)
}
