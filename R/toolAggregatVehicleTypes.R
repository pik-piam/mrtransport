toolAggregateVehicleTypes <- function(...) {

  args <- list(...)

  # save NAs (= no values) in weightMask
  weightMask <- !is.na(args[["x"]])
  getItems(weightMask, dim = "variable") <- NULL
  getItems(weightMask, dim = "unit") <- NULL

  if (!is.null(args[["weight"]])) {
    # set weight to zero where no values are
    args[["weight"]]  <- args[["weight"]] * weightMask
    getItems(args[["weight"]], dim = "variable") <- NULL
  }

  # set NAs to zeros, so that they are considered for aggregation
  # e.g. some vehicle types are present in only certain countries of a region.
  # they should be considered nevertheless, the zeros in other countries are weighted with a zero
  # and do not influence the resulting aggregated value
  args[["x"]][is.na(args[["x"]])] <- 0

  out <- do.call(toolAggregate, args)
  args[["weight"]] <- NULL
  args[["x"]] <- weightMask

  # aggregate weightMask
  naItems <- do.call(toolAggregate, args)

  # set NAs where zeros actually represent no values
  out[naItems == 0] <- NA

  return(out)
}
