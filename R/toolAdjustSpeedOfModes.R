#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @return a quitte object

toolAdjustSpeedOfModes <- function(dt, mapIso2region) {

  ## conversion GDP -> PPP-MER coefficient
  PPP_MER = fread(file.path(GCAM_folder, "GCAM_PPP_MER.csv"), header = T)
  PPP_MER = PPP_MER[,.(region,PPP_MER)] ## select only the coefficient to move from PPP to MER GDP
  regions = unique(PPP_MER$region)

  speed_not_mot = speed_not_mot[,.(tmp=paste0(supplysector,"#",tranSubsector,"#",speed))]
  speed_not_mot = CJ(tmp = speed_not_mot$tmp, region = regions, unique = T)
  speed_not_mot = speed_not_mot[,.(tmp = paste0(tmp,"#",region))]
  speed_not_mot = CJ(tmp = speed_not_mot$tmp, year = speed_mot$year, unique = TRUE)
  speed_not_mot[, `:=`(c("supplysector","tranSubsector","speed","region"), tstrsplit(tmp, "#",fixed = TRUE))]
  speed_not_mot[, c("tmp", "speed") := list(NULL, as.numeric(speed))]
  speed_not_mot = rename_region(speed_not_mot)
  speed = merge(speed_not_mot, speed_mot, all=TRUE, by=c("region", "year", "tranSubsector", "supplysector", "speed"))
  speed = addvehicletypes(speed,
                          vehfrom = "Large Car and SUV",
                          vehto = "Midsize Car",
                          reg = c("EU-15","European Free Trade Association","Europe Non EU"),
                          col2use = "tranSubsector")
  ## Apply convergence in time to the fastest vehicle across regions
  speed[, maxspeed := max(speed[year == 2100]), by = .(tranSubsector)]
  speed[year >= 2020, speed := speed[year == 2020]*(2100-year)/(2100-2020) + maxspeed*(year-2020)/(2100-2020), by =c("tranSubsector", "region")]
  speed[, maxspeed := NULL]
  ## Speed correction to enhance influence of VOT for 2W (Robert's idea)
  speed[supplysector == "trn_pass_road_LDV_2W" & speed != 1, speed := speed * 0.75]

  ## rename category following EDGE-T structure
  speed[supplysector == "trn_pass_road_bus", supplysector := "trn_pass_road_bus_tmp_subsector_L1"]

}
