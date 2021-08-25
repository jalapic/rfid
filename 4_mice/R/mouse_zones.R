## creating data subset

zone_list <- list(mouse1_zone, mouse2_zone, mouse3_zone, mouse4_zone)

saveRDS(zone_list, file="processed_data/mouse_zones.Rdata")
