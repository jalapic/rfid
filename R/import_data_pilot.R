
library(tidyverse)

## One mouse pilot

df <- read_csv2("raw_data/rawdata20210519.csv")
df$data <- as.character(df$data)

df[1:10,]
table(df$data)


## Two mice pilot

dfx <- read_csv2("raw_data/rawdata2mice20210519.csv")
dfx$data <- as.character(dfx$data)

dfx[1:10,]
table(dfx$data)
plot(dfx$cantimestamp)
