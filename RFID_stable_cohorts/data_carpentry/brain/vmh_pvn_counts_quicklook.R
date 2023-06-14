library(tidyverse)

rank <- read_csv("RFID_stable_cohorts/data_clean/socialbehavior/rank_data.csv")
head(rank)


vmh <- read_csv("RFID_stable_cohorts/data_raw/RNAscope/_PVN_summary.csv", locale=locale(encoding="latin1"))
head(vmh)
colnames(vmh)


vmhx <- vmh %>% pivot_longer(., cols = 24:35, names_to = 'target') %>% 
  mutate(prop_touching = value/Accepted) 
colnames(vmhxx)

vmhxx <- vmhx %>% full_join(rank)

vxx <- vmhxx %>% mutate(dom_group = as.factor(ifelse(.$ds_rank == 1, "DOM", "SUB"))) %>% 
  select(1:6, 50:56) %>% na.omit(.) %>% unique(.)
head(vxx)

vxx$target <- gsub(" Count Touching", "", vxx$target)

ggplot(vxx, aes(dom_group, prop_touching, color = dom_group))+
  geom_boxjitter(aes(fill = dom_group), outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                  jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  facet_wrap(~target, scales = "free")+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  ylab("positive/total cells")+
  theme_classic()+
  theme(text = element_text(size=20)) 

