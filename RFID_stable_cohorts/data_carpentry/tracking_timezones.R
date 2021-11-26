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

c12 <- myfiles1[c(1:11)] 
c12_df <-  do.call('rbind',c12) 

d <- c(1,2,3,4,8,9)

c12_df$cohort <- ifelse(c12_df$deviceid %in% c(1,2,3,4,8,9), 1,2 )

c34 <- myfiles1[c(12:22)] 
c34_df <-  do.call('rbind',c34)

c34_df$cohort <- ifelse(c34_df$deviceid %in% c(1,2,3,4,8,9), 3,4 )


l <- list('c12' = c12_df , 'c34' = c34_df )
lapply(l, head)

# id data read in 
ids <- read_csv("RFID_stable_cohorts/data_raw/id_data.csv")
head(ids)

#just the data you need 
id_data <- ids[,1:5]
id_data$back_tag <- as.character(id_data$back_tag) 

## separate cohorts
## find out what mice are moving the most
## find out what cages or zones are the most occupied 


# columns: number of tag_reading, zones, time in zones, day, ms_moving

