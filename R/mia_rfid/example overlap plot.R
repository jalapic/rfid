library(tidyverse)
library(data.table)

m1_zone_day1 <- mouse1_zone %>% filter(date == "06.04.2021" & ms > 15000000 & ms < 18600000)

m1_plot <- data.frame(
  start = m1_zone_day1$ms,
  id = m1_zone_day1$mouse,
  zone = m1_zone_day1$zone
)

m1_plot$end <- lead(m1_plot$start)
m1_plot$end[210]<-18504385

m2_zone_day1 <- mouse2_zone %>% filter(date == "06.04.2021" & ms > 15000000 & ms < 18600000)

m2_plot <- data.frame(
  start = m2_zone_day1$ms,
  id = m2_zone_day1$mouse,
  zone = m2_zone_day1$zone
)

m2_plot$end <- lead(m2_plot$start)
m2_plot$end[198]<-18588009

m3_zone_day1 <- mouse3_zone %>% filter(date == "06.04.2021" & ms > 15000000 & ms < 18600000)

m3_plot <- data.frame(
  start = m3_zone_day1$ms,
  id = m3_zone_day1$mouse,
  zone = m3_zone_day1$zone
)

m3_plot$end <- lead(m3_plot$start)                                     
m3_plot$end[184]<-18565024

m4_zone_day1 <- mouse4_zone %>% filter(date == "06.04.2021" & ms > 15000000 & ms < 18600000)

m4_plot <- data.frame(
  start = m4_zone_day1$ms,
  id = m4_zone_day1$mouse,
  zone = m4_zone_day1$zone
)

m4_plot$end <- lead(m4_plot$start)                                     
m4_plot$end[191]<-18590769



m2_plot$end <- lead(m2_plot$start)
m2_plot$end[198]<-18588009

m1_m2_plot <- rbind(m1_plot, m2_plot)

m1_m2_plot$start <- as.numeric(m1_m2_plot$start)
m1_m2_plot$end <- as.numeric(m1_m2_plot$end)

# Split by zone
m1_m2_plotz <- split(m1_m2_plot, m1_m2_plot$zone)
m1_m2_plotz

# Each zone
m1_m2_plotz[[1]] #overlaps
m1_m2_plotz[[2]] #overlaps
m1_m2_plotz[[3]] # no overlaps
m1_m2_plotz[[4]] # overlaps including edge case


### Write function to extract overlaps
f1 <- function(x) {
  
  x <- setDT(x)
  Asp <- split(x, by = "mouse")
  
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


f1(m1_m2_plotz[[1]]) # correct
f1(m1_m2_plotz[[2]]) # returns NA as no overlaps
f1(m1_m2_plotz[[3]]) # correct
f1(m1_m2_plotz[[4]]) # correct


### To do in one step:

f2 <- function(df){
  na.omit(rbindlist(Map(f1, split(setDT(df), by = "zone"))))
  }

# good up until here:
f2(m1_m2_plotz)


## other (better) method

### Functions.

# get overlaps
get_overlaps <- function(df, id1=1, id2=2){
  DT = data.table(df)
  setkey(DT, start, end)
  oDT0 = foverlaps(DT[id==id1], DT[id==id2])
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
apart <- function(df, ida=1, idb=2){
  oDT <- get_overlaps(df, ida, idb)
  oDTapt <- oDT[zone != i.zone, .(id, zone, i.id, i.zone, ostart, oend)]
  return(rbindlist(list(oDTapt[,c(1,2,5,6)],oDTapt[,3:6])))
}


### Example:
apart(m1_m2_plot)
together(m1_m2_plot)


# Plot

df.ap <- apart(m1_m2_plot)
df.to <- together(m1_m2_plot)


ggplot() + 
  geom_segment(data=df.ap, aes(x=ostart, xend=oend, y=zone, yend=zone, color=factor(id)), size=15) +
  geom_segment(data=df.to, aes(x=ostart, xend=oend, y=zone, yend=zone), color = "#571e16", size=15) +
  theme_classic() +
  scale_color_manual(values=c("#f2b03d", "#4287f5")) +
  labs(color= "mouse") +
  xlab("time since start (ms)") +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
  

###########
# function for any pair

zone_overlap <- function(df1, df2){
  
m1_m2_plot <- rbind(df1, df2)

m1_m2_plot$start <- as.numeric(m1_m2_plot$start)
m1_m2_plot$end <- as.numeric(m1_m2_plot$end)

m1_m2_plotz <- split(m1_m2_plot, m1_m2_plot$zone)

df.ap <- apart(m1_m2_plot)
df.to <- together(m1_m2_plot)

p <- ggplot() + 
  geom_segment(data=df.ap, aes(x=ostart, xend=oend, y=zone, yend=zone, color=factor(id)), size=15) +
  geom_segment(data=df.to, aes(x=ostart, xend=oend, y=zone, yend=zone), color = "#571e16", size=15) +
  theme_classic() +
  scale_color_manual(values=c("#f2b03d", "#4287f5")) +
  labs(color= "mouse") +
  xlab("time since start (ms)") +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE))

return(list(p,df.ap,df.to,m1_m2_plotz))
}


zone_overlap(m1_plot, m2_plot)
zone_overlap(m1_plot, m4_plot)




