VF <- calibration_output$list_SW$FV_final_SW
setnames(VF, c("year", "vehicle_type", "sw"), c("period", "vehicleType", "sw"))
VF <- VF[, c("region", "period", "vehicleType", "technology", "sw")]
VF<- merge(VF, tree, by = c("region", "vehicleType", "technology"), all.x = TRUE)
VF <- VF[!vehicleType == "Light Truck and SUV"]
VF <- VF[, c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "univocalName", "technology", "sw")]

S3V <- calibration_output$list_SW$VS1_final_SW
setnames(S3V, c("year", "vehicle_type", "subsector_L1"), c("period", "vehicleType", "subsectorL3"))
S3V <- S3V[, c("region", "period", "vehicleType", "subsectorL3", "sw")]
S3V <- merge(S3V, unique(VF[, c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "univocalName")]), by = c("region", "period", "subsectorL3", "vehicleType"))
S3V <- S3V[, c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "univocalName", "sw")]

S2S3 <- calibration_output$list_SW$S1S2_final_SW
setnames(S2S3, c("year", "subsector_L2", "subsector_L1"), c("period", "subsectorL2", "subsectorL3"))
S2S3 <- S2S3[, c("region", "period", "subsectorL3", "subsectorL2", "sw")]
S2S3 <- merge(S2S3, unique(VF[, c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3")]), by = c("region", "period", "subsectorL2", "subsectorL3"))
S2S3 <- S2S3[, c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "sw")]

S1S2 <- calibration_output$list_SW$S2S3_final_SW
setnames(S1S2, c("year", "subsector_L2", "subsector_L3"), c("period", "subsectorL2", "subsectorL1"))
S1S2 <- S1S2[, c("region", "period", "subsectorL1", "subsectorL2", "sw")]
S1S2 <- merge(S1S2, unique(VF[, c("region", "period", "sector", "subsectorL1", "subsectorL2")]), by = c("region", "period", "subsectorL2", "subsectorL1"))
S1S2 <- S1S2[, c("region", "period", "sector", "subsectorL1", "subsectorL2", "sw")]

SS1 <- calibration_output$list_SW$S3S_final_SW
setnames(SS1, c("year", "subsector_L3"), c("period", "subsectorL1"))
SS1 <- SS1[, c("region", "period", "subsectorL1", "sector", "sw")]
SS1 <- merge(SS1, unique(VF[, c("region", "period", "sector", "subsectorL1")]), by = c("region", "period", "sector", "subsectorL1"))
SS1 <- SS1[, c("region", "period", "sector", "subsectorL1", "sw")]

prefTrends <- list(VF = VF, S3V = S3V,  S1S2 = S1S2, SS1 = SS1)
