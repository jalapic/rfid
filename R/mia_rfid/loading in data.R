library(tidyverse)

## loading in data

df1 <- read_csv2("raw_data/Tracking/rawdata20210604.csv")
df2 <- read_csv2("raw_data/Tracking/rawdata20210605.csv")
df3 <- read_csv2("raw_data/Tracking/rawdata20210606.csv")
df4 <- read_csv2("raw_data/Tracking/rawdata20210607.csv")
df5 <- read_csv2("raw_data/Tracking/rawdata20210608.csv")
df6 <- read_csv2("raw_data/Tracking/rawdata20210609.csv")
df7 <- read_csv2("raw_data/Tracking/rawdata20210610.csv")

## stacking the data together

df <- rbind(df1, df2, df3, df4, df5, df6, df7)

df$data <- as.character(df$data)

df