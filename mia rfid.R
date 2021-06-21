library(tidyverse)

#load in data
df <- read_csv2("raw_data/Tracking/rawdata20210610.csv")
df$data <- as.character(df$data)

#looking for any impossible routes 3&19 or 17&9
mouse2 <- df %>% 
  filter(data==900133000459722 | data==900133000459721) %>%
  as.data.frame()
z <- mouse1$deviceid
rle(z)$values
# rows 116-131, 137-151, 167-172, 180-189
# at row 116, 18 min delay from previous entry

#converting 'datetimestamp' into a milliseconds column
range(df$cantimestamp)
df$time <- df$cantimestamp - min(df$cantimestamp)
df[1:10,]

plot(df$cantimestamp, type="l")

#subtraction method won't work if time keeps resetting

#figuring out the amount of time between each entry
library(lubridate)
time_interval <- x[1] %--% x[2]
time_elapsed <- as.duration(time.interval)
time_elapsed

#questions
# (1) why are there so many NA values in 'data'
# possible that NA accounts for impossible routes?
df$data

# (2) why does this plot keep resetting to 0? 
plot(df$cantimestamp, type="l")

# look at row 976
df





