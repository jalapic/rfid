library(tidyverse)

mouse1_zone

m1_zone_plot <- data.frame(
  start = mouse1_zone$ms,
  id = mouse1_zone$mouse,
  zone = mouse1_zone$zone
  )

m1_zone_plot$end <- lead(m1_zone_plot$start)
#m1_zone_plot$end[13500]<-525625653

head(m1_zone_plot)

m1_zone_ggplot <- ggplot() + 
  geom_segment(data=m1_zone_plot, aes(x=start, xend=end, y=zone, yend=zone), color="dodgerblue4", size=15) +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  ggtitle("mouse 1") +
  xlab("time since start (ms)") +
  theme(legend.position = "none") 
  
mouse2_zone

m2_zone_plot <- data.frame(
  start = mouse2_zone$ms,
  id = mouse2_zone$mouse,
  zone = mouse2_zone$zone
)

m2_zone_plot$end <- lead(m2_zone_plot$start)
#m2_zone_plot$end[4832]<-525621281

head(m2_zone_plot)

m2_zone_ggplot <- ggplot() + 
  geom_segment(data=m2_zone_plot, aes(x=start, xend=end, y=zone, yend=zone), color="firebrick3", size=15) +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  xlab("time since start (ms)") +
  ggtitle("mouse 2") +
  theme(legend.position = "none") 
  
mouse3_zone

m3_zone_plot <- data.frame(
  start = mouse3_zone$ms,
  id = mouse3_zone$mouse,
  zone = mouse3_zone$zone
)

m3_zone_plot$end <- lead(m3_zone_plot$start)
#m3_zone_plot$end[9561]<-525446628

head(m3_zone_plot)

m3_zone_ggplot <- ggplot() + 
  geom_segment(data=m3_zone_plot, aes(x=start, xend=end, y=zone, yend=zone), color="darkorange2", size=15) +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  xlab("time since start (ms)") +
  ggtitle("mouse 3") +
  theme(legend.position = "none") 


mouse4_zone

m4_zone_plot <- data.frame(
  start = mouse4_zone$ms,
  id = mouse4_zone$mouse,
  zone = mouse4_zone$zone
)

m4_zone_plot$end <- lead(m4_zone_plot$start)
#m4_zone_plot$end[5888]<-525625597

head(m4_zone_plot)

m4_zone_ggplot <- ggplot() + 
  geom_segment(data=m4_zone_plot, aes(x=start, xend=end, y=zone, yend=zone), color="darkgreen", size=15) +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  xlab("time since start (ms)") +
  ggtitle("mouse 4") +
  theme(legend.position = "none") 

library(gridExtra)

grid.arrange(m1_zone_ggplot,
             m2_zone_ggplot,
             m3_zone_ggplot,
             m4_zone_ggplot)


