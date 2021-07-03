### More on NAs.

# just look up deviceid and atennaID...

head(df)
df[1:15,]

qq <- subset(df, deviceid==17 & antennaID==2)
qq$ms <- as.numeric(qq$ms)
qq$dif_ms <- lead(qq$ms) - qq$ms
qq
qq[!is.na(qq$data),]


subset(df, deviceid==9 & antennaID==1)
