#AT weight data
library(tidyverse)


id <- read_csv("RFID_stable_cohorts/data_raw/id_data.csv")
head(id)

rank <- read_csv("RFID_stable_cohorts/data_clean/socialbehavior/rank_data.csv")
head(rank)


rank$dom <- ifelse(rank$glicko_rank == 1, "Dominant", rank$glicko_rank)
rank$dom <- ifelse(rank$glicko_rank == 2, "Subdominant", rank$dom)
rank$dom <- ifelse(rank$glicko_rank == 6, "Subordinate", rank$dom)

colnames(rank)[3]<- "mouse"

id$mouse <- as.character(id$mouse)


all <- id %>% full_join(rank)
head(all)
colnames(all)

all <- all[,c(1,3,6,8,9,11,16,17)]
all

dstat <- all %>% filter(dom != "3") %>% filter(dom != "4") %>% filter(dom !="5")
head(dstat)

dstat$AT_corr <- ((dstat$weight_AT/dstat$weight_GD1)*100)
dstat$growth <- (((dstat$weight_GD1 - dstat$weight_PD1)/dstat$weight_PD1)*100)

library(viridis)

options(ggplot2.continuous.colour="viridis")
ggplot(dstat, aes(dom, weight_GH10, color = dom))+
  geom_boxjitter(aes(fill = dom), outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  theme(legend.position = "none", text = element_text(size=20))+
  ylab("Final Body Weight (g)")+
  xlab("")+  newggtheme + ylim(30, 45)

ggplot(dstat, aes(dom,growth))+
  geom_boxplot(aes(color = dom, fill =dom),outlier.shape = NA, alpha = 0.2)+
  geom_jitter(width = .001) +
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  theme_classic()+
  theme(legend.position = "none", text = element_text(size=20))+
  ylab("% Growth Rate")+
  xlab("")

ggplot(dstat, aes(dom,weight_GD1))+
  geom_boxplot(aes(color = dom, fill =dom),outlier.shape = NA, alpha = 0.2)+
  geom_jitter(width = .001) +
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  theme_classic()+
  theme(legend.position = "none", text = element_text(size=20))+
  ylab("Final Body Weight (g)")+
  xlab("") +
  ylim(30,45)







 
