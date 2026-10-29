#' Read EDGE-T assumptions
#'
#' Read-in the table of IND-specific assumptions used in the toolAdjust* functions as data.frame
#'
#' @param subtype version folder of the table, e.g. "v1"
#' @return data.frame with one row per value and the columns id, region, variable, univocalName,
#'   technology, period, value, unit, status, derivation, sources
#' @author Jarusch Muessel
#' @seealso [madrat::readSource()]
#' @examples
#' \dontrun{ a <- readSource(type = "EDGETassumptions", subtype = "v1", convert = FALSE)
#' }
readEDGETassumptions <- function(subtype = "v1") {
  data <- data.table::fread(file.path(subtype, "assumptionsIND.csv"), colClasses = list(character = "period"))

  cols <- c("id", "region", "variable", "univocalName", "technology", "period", "value", "unit",
            "status", "derivation", "sources")
  if (!identical(names(data), cols)) stop("readEDGETassumptions: unexpected columns: ",
                                          paste(names(data), collapse = ", "))
  if (!is.numeric(data$value) || anyNA(data$value)) stop("readEDGETassumptions: column value must be numeric")
  if (!all(data$status %in% c("published", "derived", "assumption", "calibrated"))) {
    stop("readEDGETassumptions: unknown status")
  }
  key <- c("id", "region", "variable", "univocalName", "technology", "period")
  if (anyDuplicated(data, by = key)) stop("readEDGETassumptions: duplicated keys")

  list(x = as.data.frame(data), class = "data.frame")
}
