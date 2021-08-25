
library(tidyverse)



## Four mouse pilot

df <- read_csv2("raw_data/Tracking/rawdata20210604.csv")
df$data <- as.character(df$data)

df[1:10,]
table(df$data)


# cantimestamp = time since run has started
# datetimestamp = time and data of activity
# deviceid = the number of the antenna channel
# antennaID= what number in that channel
# data = microchip tag

x <- strptime(as.character(df$datetimestamp),'%d.%m.%Y %H:%M:%S:%OS')
x[[44]]

table(df$deviceid, df$antennaID)


# just look at one antenna/device....
df %>% filter(deviceid==17, antennaID==1) %>% as.data.frame()


# filter this mouse....
df %>% filter(data==900133000459723 | data==900133000459727) %>% as.data.frame()


# create a column with milliseconds
range(df$cantimestamp)
df$time <- df$cantimestamp - min(df$cantimestamp)
df[1:10,]

plot(df$cantimestamp, type="l")

# date objects are in x
x[[44]]

lubridate::milliseconds(x[[44]])


plot(df$datetimestamp) #won't work - 

#15:21 --> 15:27
df[525:545,]

df %>% 
  #filter(deviceid==19) %>%
  filter(data==900133000459722  | data== 900133000459721) %>%
  as.data.frame()


# assume these belong to same animal....
mouse1 <- df %>% 
  #filter(deviceid==19) %>%
  filter(data==900133000459722  | data== 900133000459721) %>%
  as.data.frame()

z <- mouse1$deviceid
z
rle(z)$values

check_seq <- function(n1,n2,df,id1,id2){
  code that checks the sequence here
}

