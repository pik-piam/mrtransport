toolAggregateVehicleTypes <- function(...) {
  browser()
  args <- list(...)

  weightMask <- !is.na(args[["x"]])
  getItems(weightMask, dim = "variable") <- NULL
  getItems(weightMask, dim = "unit") <- NULL

  if (is.null(args[["weight"]])) {
    args[["weight"]] <- 1 *weightMask
  } else {
    args[["weight"]]  <- args[["weight"]] * weightMask
    getItems(args[["weight"]], dim = "variable") <- NULL
  }

  args[["x"]][is.na(args[["x"]])] <- 0

  out <- do.call(toolAggregate, args)
  args[["weight"]] <- NULL
  args[["x"]] <- weightMask
  naItems <- do.call(toolAggregate, args)

  out[naItems == 0] <- NA

  return(out)
}
