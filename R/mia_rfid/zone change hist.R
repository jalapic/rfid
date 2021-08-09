library(tidyverse)

m1_zone_ggplot2 <- ggplot(m1_zone_plot, aes(x=start)) +
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

grid.arrange(m1_zone_ggplot2,
             m2_zone_ggplot2,
             m3_zone_ggplot2,
             m4_zone_ggplot2)



