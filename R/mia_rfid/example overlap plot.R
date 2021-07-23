library(tidyverse)
library(data.table)


#### Turn each mouse_zone plot into smaller df

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


head(m1_plot)
head(m2_plot)
head(m3_plot)
head(m4_plot)


####  Making a plot for Mouse1 and Mouse2

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
apart <- function(df){
  oDT <- get_overlaps(df)
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
  


####  Make these functions more 'generic' so don't have to manually input ids.

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
togetherX <- function(df, ida=1, idb=2){
  oDT <- get_overlaps(df, id1=ida, id2=idb)
  oDT[zone == i.zone, .(ids = paste0(ida,"-",idb), zone, ostart, oend)]
}


# apart
apartX <- function(df, ida=1, idb=2){
  oDT <- get_overlaps(df, id1=ida, id2=idb)
  oDTapt <- oDT[zone != i.zone, .(id, zone, i.id, i.zone, ostart, oend)]
  return(rbindlist(list(oDTapt[,c(1,2,5,6)],oDTapt[,3:6])))
}


# check it's working for mouse ids
get_overlaps(m1_m2_plot) #works for mouse1 and mouse2 as default is 1/2
together(m1_m2_plot)
togetherX(m1_m2_plot)
apartX(m1_m2_plot)

# for mouse1 and mouse3
m1_m3_plot <- rbind(m1_plot, m3_plot)
m1_m3_plot$start <- as.numeric(m1_m3_plot$start)
m1_m3_plot$end <- as.numeric(m1_m3_plot$end)

get_overlaps(m1_m3_plot, id1=1, id2=3)
togetherX(m1_m3_plot, ida=1,idb=3)
apartX(m1_m3_plot, ida=1,idb=3)


###

# Function for any pair

zone_overlap <- function(df1, df2, ida=1, idb=2){
  
m_plot <- rbind(df1, df2)
m_plot$start <- as.numeric(m_plot$start)
m_plot$end <- as.numeric(m_plot$end)

df.ap <- apartX(m_plot, ida, idb)
df.to <- togetherX(m_plot, ida, idb)

p <- ggplot() + 
  geom_segment(data=df.ap, aes(x=ostart, xend=oend, y=zone, yend=zone, color=factor(id)), size=15) +
  geom_segment(data=df.to, aes(x=ostart, xend=oend, y=zone, yend=zone), color = "#571e16", size=15) +
  theme_classic() +
  scale_color_manual(values=c("#f2b03d", "#4287f5")) +
  labs(color= "mouse") +
  xlab("time since start (ms)") +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE))

return(p)
}


zone_overlap(m1_plot, m2_plot)
zone_overlap(m1_plot, m2_plot, ida=1, idb=2)

zone_overlap(m1_plot, m3_plot, ida=1, idb=3)


### Should probably work an automated way of doing this:

range(m1_plot$start)
range(m1_plot$end)

p1 <- zone_overlap(m1_plot, m2_plot, ida=1, idb=2) + xlim(15000000,19000000)+
  scale_color_manual(values=c("#f2b03d", "#4287f5"))
p2 <- zone_overlap(m1_plot, m3_plot, ida=1, idb=3) + xlim(15000000,19000000)+
  scale_color_manual(values=c("#f2b03d", "#FE0004"))
p3 <- zone_overlap(m1_plot, m4_plot, ida=1, idb=4) + xlim(15000000,19000000)+
  scale_color_manual(values=c("#f2b03d", "#1AF5FC"))
p4 <- zone_overlap(m2_plot, m3_plot, ida=2, idb=3) + xlim(15000000,19000000)+
  scale_color_manual(values=c("#4287f5", "#FE0004"))
p5 <- zone_overlap(m2_plot, m4_plot, ida=2, idb=4) + xlim(15000000,19000000)+
  scale_color_manual(values=c("#4287f5", "#1AF5FC"))
p6 <- zone_overlap(m3_plot, m4_plot, ida=3, idb=4) + xlim(15000000,19000000)+
  scale_color_manual(values=c("#FE0004", "#1AF5FC"))


library(gridExtra)
grid.arrange(p1,p2,p3,p4,p5,p6,nrow=3)
