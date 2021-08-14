library(tidyverse)

m3_zone_ggplot2 <- ggplot(m3_zone_plot, aes(x=start)) +
  geom_histogram(fill="dodgerblue4", color="white") +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  ggtitle("mouse 1")

m2_zone_ggplot2 <- ggplot(m2_zone_plot, aes(x=start)) +
  geom_histogram(fill="firebrick3", color="white") +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  ggtitle("mouse 2")


m3_zone_ggplot2 <- ggplot(m3_zone_plot, aes(x=start)) +
  geom_histogram(fill="darkorange2", color="white") +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  ggtitle("mouse 3")


m4_zone_ggplot2 <- ggplot(m4_zone_plot, aes(x=start)) +
  geom_histogram(fill="darkgreen", color="white") +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  ggtitle("mouse 4")

library(gridExtra)

grid.arrange(m3_zone_ggplot2,
             m2_zone_ggplot2,
             m3_zone_ggplot2,
             m4_zone_ggplot2)


head(m3_zone_plot)

## figuring out a way to remove rows with duplicated zones
table(rle(m3_zone_plot$zone)$lengths)

m1zp1 <- rbind(m1_zone_plot[1,],
  m1_zone_plot[m1_zone_plot$zone!=lag(m1_zone_plot$zone),])[-2,]

m1zp1$end <- lead(m1zp1$start)

head(m3zp1)

m2zp1 <- rbind(m2_zone_plot[1,],
               m2_zone_plot[m2_zone_plot$zone!=lag(m2_zone_plot$zone),])[-2,]

m2zp1$end <- lead(m2zp1$start)

head(m2zp1)

m3zp1 <- rbind(m3_zone_plot[1,],
               m3_zone_plot[m3_zone_plot$zone!=lag(m3_zone_plot$zone),])[-2,]

m3zp1$end <- lead(m3zp1$start)

head(m3zp1)

m4zp1 <- rbind(m4_zone_plot[1,],
               m4_zone_plot[m4_zone_plot$zone!=lag(m4_zone_plot$zone),])[-2,]

m4zp1$end <- lead(m4zp1$start)

head(m4zp1)


mzp1 <- rbind(m1zp1,
      m2zp1,
      m3zp1,
      m4zp1)

head(mzp1)

mzp1 %>%
  group_by(id) %>%
  mutate(trans=row_number()) %>%
  ggplot(., aes(x=start,y=trans, color=factor(id))) +
  geom_line()

mzp1 %>%
  group_by(id) %>%
  mutate(trans=row_number()) %>%
  ggplot(., aes(x=start,y=trans, color=factor(id))) +
  geom_step()

## overlay intercepts of night and day times








