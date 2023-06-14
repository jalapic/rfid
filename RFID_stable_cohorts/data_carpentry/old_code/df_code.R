library(tidyverse)

## data 
temp <- list.files(path="RFID_stable_cohorts/data_raw/tracking_behavior/",pattern="*.csv")
xfiles <- lapply(temp, function(x) read_csv2(paste0("RFID_stable_cohorts/data_raw/tracking_behavior/",x)) )                 
lapply(xfiles, head)
lapply(xfiles, colnames)
df <- do.call(rbind, Map(data.frame, xfiles))

## mouse ids
ids <- read_csv("RFID_stable_cohorts/data_raw/id_data.csv")
head(ids)
id_data <- ids[,1:4]

head(df)
