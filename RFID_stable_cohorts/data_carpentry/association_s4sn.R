#sources
act <- readRDS("RFID_stable_cohorts/data_clean/activity.RDS")
rank <- read_csv("RFID_stable_cohorts/data_clean/socialbehavior/rank_data.csv")
rank$dom <- ifelse(rank$glicko_rank == 1, "Dominant", rank$glicko_rank)
rank$dom <- ifelse(rank$glicko_rank == 2, "Subdominant", rank$dom)
rank$dom <- ifelse(rank$glicko_rank == 6, "Subordinate", rank$dom)

rank$id <- as.character(rank$id)
colnames(rank)[3]<- "mouse"


help <- act %>% select(cohort, mouse, zone, ms)

l <- help %>% split(.$cohort)

al <- l %>% map(~group_by(., mouse)) %>% 
  map(~mutate(.,start = ms)) %>% 
  map(~mutate(.,end1 = lead(start))) %>% 
  map(~mutate(.,end = coalesce(end1, start))) %>% 
  map(~select(.,cohort, mouse, zone, ms, start, end))

lapply(al, head)
lapply(al, tail)


a_df <- do.call(rbind, al)

rank$mouse <- as.numeric(rank$mouse)

asx <- a_df%>% full_join(rank)
head(asx)
asx <- asx %>% select(cohort, mouse, zone, ms, start,end, glicko_rank,dom)

#saveRDS(asx, "RFID_stable_cohorts/data_clean/axs_corrected.rds")

c4p <- asx %>% filter(cohort == 8 & ms < 6000000)
c4p

p <- ggplot() + 
  geom_segment(data=c4p, aes(x=start, xend=end, y=zone, yend=zone), size=15, color="navy") +
  theme_classic() +
  xlab("time since start (ms)") +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_y_continuous(breaks=seq(1,10,by=1))+ 
  facet_wrap(~mouse)
p


a_df %>% 
  filter(cohort == 4 & ms < 200000) %>%
ggplot() + 
  geom_segment(aes(x=start, xend=end, y=zone, yend=zone,
                   color = zone), 
               size=15) +
  theme_classic() +
  xlab("time since start (ms)") +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_y_continuous(breaks=seq(1,10,by=1))+ 
  facet_wrap(~mouse)





#######################



