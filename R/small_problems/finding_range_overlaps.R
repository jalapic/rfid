# overlapping Gantts

df<-data.frame(start = c(0,5,6,8,10,13,15,20,22,26,29,37,40,42,
                         0,3,6,9,15,20,25,33,35,40),
           id = c(rep(1,14),
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
  facet_wrap(~id)


df





## look at overlaps #### THIS DOESNT WORK

# Need to have a 3 column df, with zone, start, end 
# must have "start/end" as names of last 2 columns
rangesA <- df[df$id==1,c(3,1,4)]
rangesB <- df[df$id==2,c(3,1,4)]

rangesA
rangesB

library(data.table)
setDT(rangesA)
setDT(rangesB)

setkey(rangesB)
foverlaps(rangesA, rangesB, type="within", nomatch=0L)


##

### Smaller Version

df<-data.frame(start = c(0,6,7,8,10,
                         0,3,5,6,7),
               id = c(rep(c(1,2),each=5)),
               zone = c("A","B","A","C","B",
                        "A","B","A","B","C")
               )           

df
df$end<-lead(df$start)
df$end[5]<-11
df$end[10]<-11
df <- df[c(2,3,1,4)]
df

split(df,df$zone)

## desired output - together:

data.frame(zone = c("A","A","B","C"),
           start = c(0,5,6,8),
           end = c(3,6,7,10),
           id = "1-2"
)


# id=1 unique:

data.frame(zone = c("A","A","B"),
           start = c(3,7,10), 
          end = c(5,8,11))

# id=2 unique:

data.frame(zone = c("B","C","C"),
           start = c(3,7,10), 
           end = c(5,8,11))
