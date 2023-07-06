#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @return a quitte object

toolAdjustCAPEX <- function(dt) {
    ## define markups on alternative techs based on the percentage difference we find in EU countries
    PSI_c = rbind(PSI_c, PSI_c[year == 2040][, year := 2100])
    ## in 2100, purchase price for BEVs is 0.8*purchase price, for Hybrid Electric is 0.7, for FCEVs is 0.9
    decr = data.table(technology = c("BEV", "Hybrid Electric", "FCEV", "Liquids", "NG"), val = c(0.8, 0.7, 0.9, 1, 1))
    PSI_c = merge(PSI_c, decr, by = "technology")
    PSI_c[year == 2100, tot_purchasecost := tot_purchasecost[technology== "Liquids"]*val, by = "vehicle_type"]


    ## add "Large Car"and "Light Truck and SUV" taking the same values as for "Large Car and SUV"
    PSI_c = rbind(PSI_c,
                  PSI_c[vehicle_type == "Large Car and SUV",][, vehicle_type := "Light Truck and SUV"],
                  PSI_c[vehicle_type == "Large Car and SUV",][, vehicle_type := "Large Car"])

    PSI_c = approx_dt(PSI_c,
                      xdata = years,
                      xcol = "year",
                      ycol = "tot_purchasecost",
                      idxcols = c("technology", "vehicle_type"),
                      extrapolate = TRUE)

    PSI_c[, val := NULL]
    PSI_c[, markup := ifelse(technology %in% c("BEV", "Hybrid Electric", "FCEV"),
                             tot_purchasecost[technology %in% c("FCEV", "BEV", "Hybrid Electric")]/
                               tot_purchasecost[technology == "Liquids"],
                             0), by = c("year")]



    electric = costs[vehicle_type %in% c(trucks, buses) & UCD_technology == "Liquids" & variable == "CAPEX and non-fuel OPEX"][, "UCD_technology" := "Electric"]
    hydrogen = costs[vehicle_type %in% c(trucks, buses) & UCD_technology == "Liquids" & variable == "CAPEX and non-fuel OPEX"][, "UCD_technology" := "FCEV"]
    altcost = rbind(electric, hydrogen)

    yeartarget_early = 2035  ## target year for electric trucks and electric and hydrogen buses
    yeartarget_late = 2150  ## target year for FCEV trucks

    ## cost of electric truck is 60% more than a as conventional truck today
    altcost[vehicle_type %in% trucks & year <= 2020 & UCD_technology == "Electric", value := 1.6*value, by = c("iso", "year", "vehicle_type")]
    ## cost of a FCEV truck is 80% more than a as conventional truck today
    altcost[vehicle_type %in% trucks & year <= 2020 & UCD_technology == "FCEV", value := 1.8*value, by = c("iso", "year", "vehicle_type")]

    ## for electric and FCEV trucks, in between 2020 and the target year, the cost follows a linear trend
    altcost[vehicle_type %in% trucks & year <= yeartarget_early & year >= 2020 & UCD_technology %in% c("Electric"),
            value := (value[year == 2020] - value[year == yeartarget_early])*(1 - (year - 2020)/(yeartarget_early - 2020)) + value[year == yeartarget_early],
            by = c("iso", "UCD_technology", "vehicle_type")]

    altcost[vehicle_type %in% trucks & year <= yeartarget_late & year >= 2020 & UCD_technology %in% c("FCEV"),
            value := (value[year == 2020] - value[year == yeartarget_late])*(1 - (year - 2020)/(yeartarget_late - 2020)) + value[year == yeartarget_late],
            by = c("iso", "UCD_technology", "vehicle_type")]

    ## cost of electric and H2 buses is 40% more of a conventional bus today
    altcost[vehicle_type %in% buses & year <= 2020, value := 1.4*value, by = c("iso", "year", "vehicle_type")]

    ## for electric and FCEV buses, in between 2020 and target year the cost follows a linear trend
    altcost[vehicle_type %in% buses & UCD_technology == "Electric" & year <= yeartarget_early & year >= 2020,
            value := (value[year == 2020]-value[year == yeartarget_early])*(1 - (year - 2020)/(yeartarget_early - 2020)) + value[year == yeartarget_early],
            by = c("iso", "UCD_technology", "vehicle_type")]

    ## for electric and FCEV buses, in between 2020 and target year the cost follows a linear trend
    altcost[vehicle_type %in% buses & UCD_technology == "FCEV" & year <= yeartarget_late & year >= 2020,
            value := (value[year == 2020]-value[year == yeartarget_late])*(1 - (year - 2020)/(yeartarget_late - 2020)) + value[year == yeartarget_late],
            by = c("iso", "UCD_technology", "vehicle_type")]

    ## assumed same other costs components to be equal to ICE trucks/buses
    tmp = rbind(costs[vehicle_type %in% c(trucks, buses) & variable != "CAPEX and non-fuel OPEX" & UCD_technology == "Liquids"][, "UCD_technology" := "Electric"],
                costs[vehicle_type %in% c(trucks, buses) & variable != "CAPEX and non-fuel OPEX" & UCD_technology == "Liquids"][, "UCD_technology" := "FCEV"])

    ## add hydrogen airplanes
    h2air = costs[vehicle_type == "Domestic Aviation_tmp_vehicletype" & UCD_technology == "Liquids"][, UCD_technology := "Hydrogen"]
    ## CAPEX of hydrogen airplanes is assumed today 5 times more expensive than a conventional airplane (i.e. not present in the market)
    h2air[vehicle_type %in% "Domestic Aviation_tmp_vehicletype" & year <= 2020,
          value := 5*value]
    ## following https://www.fch.europa.eu/sites/default/files/FCH%20Docs/20200507_Hydrogen%20Powered%20Aviation%20report_FINAL%20web%20%28ID%208706035%29.pdf
    ## maintenance costs are 50% higher than than a liquids fuelled airplane
    h2air[vehicle_type =="Domestic Aviation_tmp_vehicletype" & year >= 2020 & variable == "non-fuel OPEX",
          value := 1.5*value]
    ## for hydrogen airplanes, in between 2020 and 2040 the cost follows a linear trend, and reaches a value 30% higher than a liquids fuelled airplane
    h2air[vehicle_type =="Domestic Aviation_tmp_vehicletype" & year >= 2020 & variable == "CAPEX",
          value := ifelse(year <= 2040, value[year==2020] + (1.3*value[year == 2100]-value[year==2020]) * (year-2020) / (2100-2020), 1.3*value[year == 2100]),
          by = c("iso", "UCD_technology", "vehicle_type")]
}
