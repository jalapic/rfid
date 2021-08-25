
## From line 174 onwards....

df$row <- 1:nrow(df)
df$ms <- as.integer(df$ms)

head(df)  #tube errors need to be looked at later.

table(df$data)
table(df$deviceid,df$antennaID)


## 1. we should check if this code completely always has ID,NA,ID,NA,ID,NA sequence in 'data'...
subset(df, deviceid==3 & antennaID==1)

## 2. One option is to add the 'ID/tag' for the previous row to each NA, and add a column 'entry'/'leave'

j1 <- subset(df, deviceid==3 & antennaID==1)

j1$el <- ifelse(is.na(j1$data), "leave", "entry")
j1$data1 <-zoo::na.locf(j1$data)

head(j1)

## 3. Just filter out the NA rows i.e the 'leaves'.

q3_1 <- subset(df, deviceid==3 & antennaID==1)
q3_1 <- q3_1[!is.na(q3_1$data),]

q3_1

## 4. Could just remove the NAs from the original data....

df1 <- df[!is.na(df$data),]

# dfx<-data.frame(data=c("900133000459724", "900133000459725",
#                   "900133000459723","900133000459727",
#                   "900133000459722", "900133000459721",
#                   "900133000459719", "900133000459693"),
#            mouse = c(4,4,3,3,2,2,1,1)
#            )
# getwd()
# write.csv(dfx,"raw_data/mouseids.csv", row.names = F)

dfx <- read_csv("raw_data/mouseids.csv")
dfx

## 5. Extract individual dataframes for each animal.

head(df1)

mouse1 <- subset(df1, data %in% dfx[dfx$mouse==1,]$data) %>% mutate(mouse = 1)
mouse2 <- subset(df1, data %in% dfx[dfx$mouse==2,]$data) %>% mutate(mouse = 2)
mouse3 <- subset(df1, data %in% dfx[dfx$mouse==3,]$data) %>% mutate(mouse = 3)
mouse4 <- subset(df1, data %in% dfx[dfx$mouse==4,]$data) %>% mutate(mouse = 4)

mouse3
