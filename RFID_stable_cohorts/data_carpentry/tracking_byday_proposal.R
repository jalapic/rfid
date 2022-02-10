library(tidyverse)


ts <- readRDS("RFID_stable_cohorts/data_clean/tracking_byday.RDS")


rank <- read_csv("RFID_stable_cohorts/data_clean/socialbehavior/rank_data.csv")
head(rank)
rank$dom <- ifelse(rank$glicko_rank == 1, "Dominant", rank$glicko_rank)
rank$dom <- ifelse(rank$glicko_rank == 2, "Subdominant", rank$dom)
rank$dom <- ifelse(rank$glicko_rank == 6, "Subordinate", rank$dom)

rank <- rank %>% select(cohort, mouse, glicko_rank)

head(ts)
head(rank)

dms <- rank %>% full_join(ts)
head(dms)

dms$glicko_rank <- factor(dms$glicko_rank, levels = c (1,2,3,4,5,6))

day1 <- dms %>% filter(day == 1) %>% 
  group_by(cohort, mouse, zone) %>%
  mutate(tot_ms = sum(duration))
  
head(day1)

ggplot(day1, aes(glicko_rank, tot_ms, color = glicko_rank))+
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(~zone)




d1 <- ggplot(day1, aes(glicko_rank, tot_ms, color = glicko_rank))+
  geom_boxjitter(aes(fill = glicko_rank), outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  facet_wrap(~zone)+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  theme_classic()+
  theme(legend.position = "none", text = element_text(size=20))+
  ylab("Total time (ms)") +
  xlab("Mouse Rank") +
  ggtitle("Day 1")


print(d1)
dev.off()





day10 <- dms %>% filter(day == 10) %>% 
  group_by(cohort, mouse, zone) %>%
  mutate(tot_ms = sum(duration))

head(day10)


d10 <- ggplot(day10, aes(glicko_rank, tot_ms, color = glicko_rank))+
  geom_boxjitter(aes(fill = glicko_rank), outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  facet_wrap(~zone)+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  theme_classic()+
  theme(legend.position = "none", text = element_text(size=20))+
  ylab("Total time (ms)") +
  xlab("Mouse Rank")+
  ggtitle("Day 10")

print(d10)

st <- gridExtra::grid.arrange(d1, d10)

print(st)


 ggsave("total_ms_day1_10.png", st, width = 12, height = 12)



dd <- day1 %>% rbind(day10)

dd$day <- ifelse(dd$day == 1, "Day 1", "Day 10")
dd$zone <- as.factor(dd$zone)


d110 <- ggplot(dd, aes(zone, tot_ms, color = glicko_rank))+
  geom_boxjitter(aes(fill = glicko_rank), outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  facet_grid(glicko_rank~day)+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  theme_classic()+
  theme(legend.position = "none", text = element_text(size=20))+
  ylab("Total time (ms)") +
  xlab("Cage")


print(d110)

ggsave("total_ms_day1_10X2.png", d110,width = 12, height = 12)


ad <- dms %>%
  group_by(cohort, mouse, zone) %>%
  mutate(tot_ms = sum(duration))
head(ad)
table(ad$day)

ad <- ad %>% filter(day !="11")
ad$day <- paste("Day",ad$day)
ad$day <- factor(ad$day, level = c("Day 1","Day 2","Day 3","Day 4","Day 5","Day 6","Day 7","Day 8","Day 9","Day 10"))
ad$glicko_rank <- as.factor(ad$glicko_rank)
ad$zone <- as.factor(ad$zone)

ad$glicko_rank1 <- ifelse(ad$glicko_rank == 1, "Rank 1", ad$glicko_rank)
ad$glicko_rank1 <- ifelse(ad$glicko_rank == 2, "Rank 2", ad$glicko_rank1)
ad$glicko_rank1 <- ifelse(ad$glicko_rank == 3, "Rank 3", ad$glicko_rank1)
ad$glicko_rank1 <- ifelse(ad$glicko_rank == 4, "Rank 4", ad$glicko_rank1)
ad$glicko_rank1 <- ifelse(ad$glicko_rank == 5, "Rank 5", ad$glicko_rank1)
ad$glicko_rank1 <- ifelse(ad$glicko_rank == 6, "Rank 6", ad$glicko_rank1)


all <- ggplot(ad, aes(zone, tot_ms, color = glicko_rank1))+
  geom_boxjitter(aes(fill = glicko_rank1), outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  facet_grid(day~glicko_rank1)+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  theme_classic()+
  theme(legend.position = "none", text = element_text(size=20))+
  ylab("Total time (ms)") +
  xlab("Cage")


ggsave("total_ms_allrank.png", all,width = 15, height = 25)


all <- ggplot(ad, aes(zone, tot_ms))+
  geom_boxjitter( outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  facet_grid(glicko_rank1~day)+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  theme_classic()+
  theme(legend.position = "none", text = element_text(size=20))+
  ylab("Total time (ms)") +
  xlab("Cage")


ggsave("total_ms_allrankX2.png", all,width = 25, height = 15)


dp1 <- ad %>% filter(day == "Day 1")

dp5 <- ad %>% filter(day == "Day 5")

dp10 <- ad %>% filter(day == "Day 10")


dp <- dp1 %>% rbind(dp5,dp10)
table(dp$day)

pp <- ggplot(dp, aes(zone, tot_ms, color = glicko_rank1))+
  geom_boxjitter(aes(fill = glicko_rank1),outlier.color = NA, jitter.shape = 21,
                  alpha = 0.5,
                  jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                  position = position_dodge(0.85)) +
  facet_grid(day ~ glicko_rank1)+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  theme_classic()+
  theme(legend.position = "none", text = element_text(size=20))+
  ylab("Total time (ms)") +
  xlab("Cage")
pp


ggsave("tracking_proposal.png", pp, width = 12, height = 12)



pp2 <- ggplot(dp, aes(zone, tot_ms, color = zone))+
  geom_boxjitter(aes(fill = zone),outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  facet_grid(day ~ glicko_rank1)+
  scale_color_manual(values = viridis::viridis(8)) +
  scale_fill_manual(values = viridis::viridis(8)) +
  theme_classic()+
  theme(legend.position = "none", text = element_text(size=20))+
  ylab("Total time (ms)") +
  xlab("Cage")
pp2


ggsave("tracking_proposal2.png", pp2, width = 12, height = 12)

pp2 <- ggplot(dp, aes(zone, tot_ms))+
  geom_boxjitter(outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  facet_grid(day ~ glicko_rank1)+
  scale_color_manual(values = viridis::viridis(12)) +
  scale_fill_manual(values = viridis::viridis(12)) +
  theme_classic()+
  theme(legend.position = "none", text = element_text(size=20))+
  ylab("Total time (ms)") +
  xlab("Cage")
pp2


ggsave("tracking_proposal3.png", pp2, width = 12, height = 12)



pp4 <- ggplot(dp, aes(zone, tot_ms, color = zone))+
  geom_boxjitter(aes(fill = zone),outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  facet_grid(glicko_rank1~day)+
  scale_color_manual(values = viridis::viridis(8)) +
  scale_fill_manual(values = viridis::viridis(8)) +
  theme_classic()+
  theme(legend.position = "none", text = element_text(size=20))+
  ylab("Total time (ms)") +
  xlab("Cage")
pp4


ggsave("tracking_proposal4.png", pp4, width = 12, height = 12)
