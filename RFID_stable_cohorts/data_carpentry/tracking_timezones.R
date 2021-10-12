## start of tracking analysis 


#libraries 
library(tidyverse)

#sources 
temp <- list.files(path="RFID_stable_cohorts/data_raw/tracking_behavior/",pattern="*.csv")
xfiles <- lapply(temp, function(x) read_csv2(paste0("RFID_stable_cohorts/data_raw/tracking_behavior/",x)) )                 
lapply(xfiles, head)
lapply(xfiles, colnames)


# how to make it a big data frame
myfiles1 <- Map(cbind, xfiles)



# id data read in 
ids <- read_csv("RFID_stable_cohorts/raw_data/id_data.csv")
head(ids)

#just the data you need 
id_data <- ids[,1:4]


## separate cohorts
## find out what mice are moving the mouse
## find out what cages or zones are the most occupied 


# columns: number of tag_reading, zones, time in zones, day, ms_moving

