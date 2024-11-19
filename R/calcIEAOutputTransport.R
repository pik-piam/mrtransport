#' Calc Input Output
#'
#' Computes IEA-based model data by use of raw IEA "Energy Balances" data
#' and a mapping that corresponds to the structure of "products" and "flows" of IEA.
#'
#' Mapping structure example: IEA product ANTCOAL used for IEA flow TPATFUEL, contributes via REMIND technology
#' coaltr for generating sesofos from pecoal (REMIND names)
#'
#' @return IEA data as MAgPIE object aggregated to country level
#' @author Falk Benke
#'
#' @importFrom dplyr filter mutate
#' @importFrom tidyr unite
#' @importFrom tidyselect all_of
#'
calcIEAOutputTransport <- function() {

  # read in data and convert from ktoe to EJ
  data <- readSource("IEA", subtype = "EnergyBalances") * 4.1868e-5

  ieamatch <- toolGetMapping(type = "sectoral", name = "structuremappingIO_outputs.csv",
                             where = "mrcommons")

  # add total buildings electricity demand (feelb = feelcb + feelhpb + feelrhb)
  ieamatch <- rbind(
    ieamatch,
    ieamatch %>%
      filter(.data$REMINDitems_out %in% c("feelcb", "feelhpb", "feelrhb")) %>%
      mutate(REMINDitems_out = "feelb")
  )

  # delete NAs rows
  target <- c("REMINDitems_in", "REMINDitems_out", "REMINDitems_tech", "iea_product", "iea_flows")

  ieamatch <- ieamatch %>%
    as_tibble() %>%
    select(all_of(c("iea_product", "iea_flows", "Weight", target))) %>%
    na.omit() %>%
    unite("target", all_of(target), sep = ".", remove = FALSE) %>%
    unite("product.flow", c("iea_product", "iea_flows"), sep = ".") %>%
    filter(.data$product.flow %in% getNames(data))


  reminditems <- do.call(
    mbind,
    lapply(
      unique(ieamatch$target),
      function(item) {

        productFlow <- ieamatch %>%
          filter(.data$target == item) %>%
          pull("product.flow")

        weights <- ieamatch %>%
          filter(.data$target == item) %>%
          pull("Weight") %>%
          as.numeric()

        tmp <- dimSums(
          data[, , productFlow] * setNames(as.magpie(weights), productFlow),
          dim = 3, na.rm = TRUE
        )

        getNames(tmp) <- item

        return(tmp)
      }
    )
  )

  return(list(
    x = reminditems, weight = NULL, unit = "EJ",
    description = "IEA SE Output Data based on 2022 edition of IEA World Energy Balances"
  ))
}
