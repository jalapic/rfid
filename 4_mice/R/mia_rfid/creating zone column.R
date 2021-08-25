## creating zone column

zone <- function(df){
  df$row <- 1:nrow(df)
  df$deviceid <- as.numeric(df$deviceid)
  df$antennaID <- as.numeric(df$antennaID)
  
  z1 <- filter(df, deviceid==17 & antennaID==2 | deviceid==3 & antennaID==1)
  z2 <-filter(df, deviceid==3 & antennaID==2 | deviceid==9 & antennaID==1)
  z3 <-filter(df, deviceid==9 & antennaID==2 | deviceid==19 & antennaID==1)
  z4 <- filter(df, deviceid==19 & antennaID==2 | deviceid==17 & antennaID==1)
  
  z1$zone=1
  z2$zone=2
  z3$zone=3
  z4$zone=4
  
  zone_df <- rbind(z1,z2,z3,z4)
  zone_df <- arrange(zone_df, row)
  
  x <- zone_df$zone
  x1 <- sort(unique(c(length(x) - cumsum(rle(rev(x))$lengths) + 1, cumsum(rle(x)$lengths) )))
  x1 <- unlist(x1)
  
  zone_df <- zone_df[x1,]
}

mouse1_zone <- zone(mouse1)
mouse2_zone <- zone(mouse2)
mouse3_zone <- zone(mouse3)
mouse4_zone <- zone(mouse4)