### Example Dataset
library(tidyverse)

df2<-data.frame(start = c(0,5,6,8,10,13,15,20,22,26,29,37,40,42,
                         0,3,6,9,15,20,25,33,35,40),
               id = c(rep(1,14),
                      rep(2,10)
               ),
               zone = c("A","B","A","D","C","B","C","B","A","B","A","D","C","D",
                        "A","B","C","D","A","B","C","B","A","D")
)           

df2
df2$end <- lead(df2$start)
df2$end[nrow(df2)]<-45
df2$end[14]<-45

# look at example dataset
df2


# Split by zone
df2z <- split(df2, df2$zone)
df2z


# Each zone
df2z[[1]] #overlaps
df2z[[2]] #overlaps
df2z[[3]] # no overlaps
df2z[[4]] # overlaps including edge case



### Write function to extract overlaps
f1 <- function(x) {

x <- setDT(x)
Asp <- split(x, by = "id")

u <- na.omit(foverlaps(Asp[[1]], setkey(Asp[[2]], start, end)))

if(nrow(u)==0){
  outdf<-data.frame(zone=NA,t1=NA,t2=NA)
} else {
  
r <- c()

for (k in 1:nrow(u)) {
  
  p <- cbind(
    t1=pmax(u[k,start],u[k,i.start]), 
    t2=pmin(u[k,end],u[k,i.end])
  )
        
        r[[k]] <- p
        
}

        
outdf<-data.frame(
  cbind(
  zone = u[, zone],
  do.call('rbind', r)
)
)


}
return(outdf)
}

f1(df2z[[1]]) # correct
f1(df2z[[2]]) # correct
f1(df2z[[3]]) # returns NA as no overlaps
f1(df2z[[4]]) # correct


### To do in one step:
na.omit(rbindlist(Map(f1, split(setDT(df2), by = "zone"))))

f2 <- function(DF){
  na.omit(rbindlist(Map(f1, split(setDT(DF), by = "zone"))))
}

# this should work:
f2(df2)



#### Harder, Non-realistic example
set.seed(1)
N<-20
s1 <- sample(1:100,N,F)
s2 <- sample(1:100,N,F)
s1[order(s1)]
s2[order(s2)]

df <- data.frame(
  start = c(s1[order(s1)], s2[order(s2)]),
  id = rep(c(1,2),each=N),
  zone = sample(LETTERS[1:4],N*2,T)
)

df

# add end times
df$end <- lead(df$start)
df$end[nrow(df)]<-100
df$end[20]<-100

df

f2(df)

split(df,df$zone)




#### BETTER solution:

# this one is cleaner, but doesn't include times of length 1 to be shared
# but this isn't a big deal.

# this also produces an output of times when not together.


df

DT = data.table(df)
setkey(DT, start, end)
oDT0 = foverlaps(DT[id==1], DT[id==2])
oDT0[, `:=`(
  ostart = pmax(start, i.start),
  oend = pmin(end, i.end)
)]
oDT = oDT0[ostart < oend]

# together
oDT[zone == i.zone, .(ids = '1-2', zone, ostart, oend)]

# apart
oDT[zone != i.zone, .(id, zone, i.id, i.zone, ostart, oend)]



### Functions.

# get overlaps
get_overlaps <- function(df){
  DT = data.table(df)
setkey(DT, start, end)
oDT0 = foverlaps(DT[id==1], DT[id==2])
oDT0[, `:=`(
  ostart = pmax(start, i.start),
  oend = pmin(end, i.end)
)]
oDT = oDT0[ostart < oend]
return(oDT)
}


# together
together <- function(df){
oDT <- get_overlaps(df)
oDT[zone == i.zone, .(ids = '1-2', zone, ostart, oend)]
}

# apart
apart <- function(df){
oDT <- get_overlaps(df)
oDTapt <- oDT[zone != i.zone, .(id, zone, i.id, i.zone, ostart, oend)]
return(rbindlist(list(oDTapt[,c(1,2,5,6)],oDTapt[,3:6])))
}


### Example:
df2
apart(df2)
together(df2)

df
apart(df)
together(df)



# Plot

df.ap <- apart(df)
df.to <- together(df)


ggplot() + 
  geom_segment(data=df.ap, aes(x=ostart, xend=oend, y=zone, yend=zone, color=factor(id)), size=15) +
  geom_segment(data=df.to, aes(x=ostart, xend=oend, y=zone, yend=zone), color = "#571e16", size=15) +
  theme_classic() +
  scale_color_manual(values=c("#f2b03d", "#4287f5"))
