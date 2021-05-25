
library(tidyverse)

## One mouse pilot

df <- read_csv2("raw_data/rawdata20210519.csv")
df$data <- as.character(df$data)

df[1:10,]
table(df$data)


## Two mice pilot

# cantimestamp = time since run has started
# datetimestamp = time and data of activity
# deviceid = the number of the antenna channel
# antennaID= what number in that channel
# data = microchip tag

dfx <- read_csv2("raw_data/rawdata2mice20210519.csv")
dfx$data <- as.character(dfx$data)

dfx[1:10,]
table(dfx$data)

x <- strptime(as.character(dfx$datetimestamp),'%d.%m.%Y %H:%M:%S:%OS')
x[[44]]

table(dfx$deviceid, dfx$antennaID)


# just look at one antenna/device....
dfx %>% filter(deviceid==17, antennaID==1) %>% as.data.frame()


# filter this mouse....
dfx %>% filter(data==900133000459724 | data==900133000459725) %>% as.data.frame()


