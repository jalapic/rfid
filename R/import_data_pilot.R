
library(tidyverse)
df <- read_csv2("raw_data/rawdata20210519.csv")
df$data <- as.character(df$data)

df[1:10,]
table(df$data)
