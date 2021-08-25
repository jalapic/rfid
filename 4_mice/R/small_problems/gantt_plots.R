#####
df<-data.frame(
  ms = c(1,5,10,16,26,40,100,111,116,130),
  zone = c(1,1,2,2,3,4,3,2,1,1)
)
df$duration <- df$ms - Hmisc::Lag(df$ms)

df$start <- Hmisc::Lag(df$ms)

df$start[1]<-0

df

library(tidyverse)

ggplot(df) + 
  geom_segment(aes(x=start, xend=ms, y=zone, yend=zone), size=15) +
  theme_classic()
