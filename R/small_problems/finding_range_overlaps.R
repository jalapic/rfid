# overlapping Gantts

df<-data.frame(start = c(0,5,6,8,10,13,15,20,22,26,29,37,40,42,
                         0,3,6,9,15,20,25,33,35,40),
           mouse = c(rep(1,14),
                     rep(2,10)
                     ),
           zone = c("A","B","A","D","C","B","C","B","A","B","A","D","C","D",
                    "A","B","C","D","A","B","C","B","A","D")
)           

df

# adding end time (this is done manually - don't need to do it like this in our data)
df$end <- lead(df$start)
df$end[nrow(df)]<-45
df$end[14]<-45

df

# non overlapping
library(tidyverse)

ggplot(df) + 
  geom_segment(aes(x=start, xend=end, y=zone, yend=zone), size=15) +
  theme_classic() +
  facet_wrap(~mouse)



## look at overlaps

# Need to have a 3 column df, with zone, start, end 
# must have "start/end" as names of last 2 columns
rangesA <- df[df$mouse==1,c(3,1,4)]
rangesB <- df[df$mouse==2,c(3,1,4)]

rangesA
rangesB

library(data.table)
setDT(rangesA)
setDT(rangesB)

setkey(rangesB)
foverlaps(rangesA, rangesB, type="within", nomatch=0L)

