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
dms$zone <- as.factor(dms$zone)


table(dms$zone)
#there is some NAS in the zone - need to fix later. 
total <- na.omit(total)

total <- dms %>% group_by(cohort,glicko_rank,day,zone) %>%
  summarize(tot_ms = sum(duration, na.rm = T)) %>% unique(.)

total <- total %>% group_by(cohort,glicko_rank, zone) %>% 
  mutate(mt = mean(tot_ms))


head(total)
total <- total %>% group_by(glicko_rank) %>% 
  summarise(mt = mean(tot_ms), med = median(tot_ms), quantile(tot_ms,))




head(total)
table(total$zone)


ggplot(total, aes(glicko_rank, tot_ms, color = glicko_rank))+
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(~zone)


total$zone1 <- ifelse(total$zone == 1, "Cage 1", total$zone)
total$zone1 <- ifelse(total$zone == 2, "Cage 2", total$zone1)
total$zone1 <- ifelse(total$zone == 3, "Cage 3", total$zone1)
total$zone1 <- ifelse(total$zone == 4, "Cage 4", total$zone1)
total$zone1 <- ifelse(total$zone == 5, "Cage 5", total$zone1)
total$zone1 <- ifelse(total$zone == 6, "Cage 6", total$zone1)



total$glicko_rank1 <- ifelse(total$glicko_rank == 1, "Rank 1", total$glicko_rank)
total$glicko_rank1 <- ifelse(total$glicko_rank == 2, "Rank 2", total$glicko_rank1)
total$glicko_rank1 <- ifelse(total$glicko_rank == 3, "Rank 3", total$glicko_rank1)
total$glicko_rank1 <- ifelse(total$glicko_rank == 4, "Rank 4", total$glicko_rank1)
total$glicko_rank1 <- ifelse(total$glicko_rank == 5, "Rank 5", total$glicko_rank1)
total$glicko_rank1 <- ifelse(total$glicko_rank == 6, "Rank 6", total$glicko_rank1)


t2 <- total %>% filter(tot_ms < 30000)

ggplot(total, aes(zone, mt, color = zone))+
  geom_boxjitter(aes(fill = zone), outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  facet_wrap(~glicko_rank1)+
  scale_color_manual(values = viridis::viridis(8)) +
  scale_fill_manual(values = viridis::viridis(8)) +
  theme_classic()+
  theme(legend.position = "none", 
        axis.text.x = element_text(size= 15) ,
        axis.text.y = element_text(size= 15),
        strip.text = element_text(size = 15),
        text = element_text(size= 20))+
  ylab("Average Time (ms)") +
  xlab("Cage")


library(lme4)
library(MASS)
library(car)

hist(total$tot_ms) #not normal - glmer


total <- dms %>% group_by(cohort,glicko_rank,zone) %>%
  summarize(tot_ms = sum(duration, na.rm = T))
total$zone <- as.numeric(total$zone)

tx <- total %>% group_by(cohort,glicko_rank,zone)  %>%
  summarize(mean = mean(tot_ms, na.rm = T))
  tx$zone <- as.numeric(tx$zone)    

t2 <- total %>% filter(glicko_rank != 3)  %>% filter(glicko_rank != 4) %>% filter(glicko_rank != 5)

total$glicko_rank <- factor(total$glicko_rank, level = c(6,4,5,3,1,2))

tm <-glmer(tot_ms~ glicko_rank + zone+ glicko_rank:zone +(1|cohort), data = total ,family = Gamma(link = "log"))
summary(tm)

hist(t2$tot_ms)



AIC(tm)
total






heat <- as.matrix(table(dms$cohort, dms$zone, dms$duration))
heatmap(heat, 
        Colv = NA, Rowv = NA, 
        xlab = "zone", ylab = "cohort", main = "zone usage")




 ggplot(data = dms, mapping = aes(x =cohort, y = zone, fill = duration)) +
  geom_tile() 
 
 
 total <- dms %>% group_by(cohort,zone,day) %>%
   summarize(tot_ms = sum(duration, na.rm = F)) %>% unique(.)
 
 
 total <- total %>% filter(day != 11)
 
 total$day1 <- ifelse(total$day == 1, "Day 1", total$day)
 total$day1 <- ifelse(total$day == 2, "Day 2", total$day1)
 total$day1 <- ifelse(total$day == 3, "Day 3", total$day1)
 total$day1 <- ifelse(total$day == 4, "Day 4", total$day1)
 total$day1 <- ifelse(total$day == 5, "Day 5", total$day1)
 total$day1 <- ifelse(total$day == 6, "Day 6", total$day1)
 total$day1 <- ifelse(total$day == 7, "Day 7", total$day1)
 total$day1 <- ifelse(total$day == 8, "Day 8", total$day1)
 total$day1 <- ifelse(total$day == 9, "Day 9", total$day1)
 total$day1 <- ifelse(total$day == 10, "Day 10", total$day1)
 total$day1 <- factor(total$day1, levels = c("Day 1", "Day 2", "Day 3", "Day 4","Day 5", "Day 6", "Day 7","Day 8","Day 9","Day 10"))
 
 total$cohort <- as.factor(total$cohort)
 # total <- na.omit(total)

 ggplot(data = total, mapping = aes(x =zone, y = cohort, fill =tot_ms)) +
   geom_tile() +
   scale_fill_gradient(name = "Total Time (ms)",
                       low = "#FFFFFF",
                       high = "#012345")+
   theme_classic()+
   theme(axis.text.x = element_text(size= 20) ,
         axis.text.y = element_text(size= 20),
         strip.text = element_text(size = 20),
         text = element_text(size= 25))+
   ylab("Cohort") +
   xlab("Cage")
 


 breaks=c(0,0.5,1),labels=c("Minimum",0.5,"Maximum"),
 limits=c(0,1)




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

ad$zone1 <- ifelse(ad$glicko_rank == 1, "Rank 1", ad$glicko_rank)
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
  theme(legend.position = "none", 
        axis.text.x = element_text(size= 20) ,
        axis.text.y = element_text(size= 20),
        strip.text = element_text(size = 20),
        text = element_text(size= 25))+
  ylab("Total time (ms)") +
  xlab("Cage")
pp4


ggsave("tracking_proposal4.png", pp4, width = 15, height = 25)


head(dp)
 


library(lme4)
library(MASS)
library(car)

hist(dp$tot_ms) #not normal - glmer

tm <-glmer(tot_ms~glicko_rank+ zone+(1|mouse)+(1|cohort), data =dp,family = Gamma(link = "log"))
summary(tm)
AIC(pre.glm)




head(dms)



la <- dms %>% split(.$cohort)
lapply(la, head)


la2 <- la %>% map(~group_by(.,day,glicko_rank)) %>% 
  map(~mutate(.,zd = start-lag(start))) %>% map(~group_by(.,day,glicko_rank,zone)) %>% 
  map(~mutate(., total = sum(!is.na(zd))))
dfnames <- c(1:10)


head(ztime)

zt <- ztime %>% group_by(cohort,glicko_rank) %>% 
  mutate(avg = mean(total))
zt$glicko_rank <- as.factor(zt$glicko_rank)



ggplot(zt, aes(glicko_rank, avg))+
  geom_boxplot()+
  geom_jitter()



zx <- dms %>% group_by(cohort,glicko_rank,day,zone) %>% 
  mutate(.,zd = start-lag(start)) %>% 
  mutate(., total = sum(!is.na(zd)))
head(zx)

  zx<- zx%>% group_by(cohort,mouse,glicko_rank,day) %>% 
    summarize(avg = mean(total))

tt <- zx %>% dplyr::select(1:5,10) %>% unique()

tt$day <- as.factor(tt$day)

tt <- tt %>% filter(day != 11) %>% filter(cohort != 6)
tt <- na.omit(tt)

head(tt)

ttx <- tt %>% group_by(glicko_rank,day,zone)%>% 
   summarize(avg = mean(total))

head(ttx)



ttx$zone <- ifelse(ttx$zone == 1, "Cage 1", ttx$zone)
ttx$zone <- ifelse(ttx$zone == 2, "Cage 2", ttx$zone)
ttx$zone <- ifelse(ttx$zone == 3, "Cage 3", ttx$zone)
ttx$zone <- ifelse(ttx$zone == 4, "Cage 4", ttx$zone)
ttx$zone <- ifelse(ttx$zone == 5, "Cage 5", ttx$zone)
ttx$zone <- ifelse(ttx$zone == 6, "Cage 6", ttx$zone)



ggplot(ttx, aes(day,avg, color = glicko_rank))+
  geom_line(aes(group = glicko_rank), size = .4, alpha=.8)+
  geom_point(aes(group =avg), alpha = .5, size = 1.2)+
  facet_wrap(~zone)+
  scale_color_manual(values = viridis::viridis(6)) +
  scale_fill_manual(values = viridis::viridis(6)) +
  theme_minimal()+
  scale_y_continuous(limits =c(0, 4000),
                     labels = c(0, seq(500, 4000, by = 1000)))+
  labs(color = "Rank")+
  theme( strip.background  = element_rect(fill = NA, color = "black"),
         axis.text.x = element_text(size= 10) ,
        axis.text.y = element_text(size= 10),
        strip.text = element_text(size = 11),
        text = element_text(size= 15))+
  ylab("Average Cage Entries ") +
  xlab("Day")


head(tt)
ttx1 <- tt %>% group_by(glicko_rank,day)


jp <- dms %>% filter(cohort != 6)%>% group_by(cohort,glicko_rank,day) %>% 
  mutate(.,zd = start-lag(start)) %>% 
  mutate(., total = sum(!is.na(zd))) %>% ungroup(.)
head(jp)



jp1 <-jp %>% dplyr::select(4,10) %>% unique() %>% filter(day != 11) 
head(jp1)


jp2 <- jp1 %>% group_by(day,glicko_rank) %>% mutate(avg = sum(total)/9) 

jp3 <- jp2 %>% filter(glicko_rank != 3)%>% filter(glicko_rank != 4) %>% filter(glicko_rank != 5)

jp3$glicko_rank <- ifelse(jp3$glicko_rank == 1, "Dominant", jp3$glicko_rank)
jp3$glicko_rank <- ifelse(jp3$glicko_rank == 2, "Subdominant", jp3$glicko_rank)
jp3$glicko_rank <- ifelse(jp3$glicko_rank == 6, "Subordinate", jp3$glicko_rank)

ggplot(jp2, aes(glicko_rank, avg, color = glicko_rank))+
  geom_boxjitter(aes(fill = glicko_rank),outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  scale_color_manual(values = viridis::viridis(3)) +
  scale_fill_manual(values = viridis::viridis(3)) +
  ylim(2000,10000)+
  theme_classic()+
  theme(legend.position = "none", 
        axis.text.x = element_text(size= 15) ,
        axis.text.y = element_text(size= 15),
        text = element_text(size= 20))+
  ylab("Average Food Cage Entries") +
  xlab("Mouse Rank")


zx <- dms %>%filter(cohort != 6) %>% filter(cohort != 5) %>%  group_by(cohort,glicko_rank,day,zone) %>% 
  mutate(.,zd = start-lag(start)) %>% 
  mutate(., total = sum(!is.na(zd))) %>% filter(zone == 1)
head(zx)
zx%>%filter(zone ==1) %>%  group_by(glicko_rank) %>% 
  summarize(med = median(total), q25 = quantile(total,0.25),  q75 = quantile(total,0.75))


zx<- zx  %>%  group_by(glicko_rank) %>% 
  mutate(avg = mean(total)) %>% unique(.)

tm <-glmer(avg~glicko_rank+ day+(1|mouse)+(1|cohort), data =zx,family = Gamma(link = "log"))
summary(tm)
AIC(pre.glm)





zx$day <- as.factor(zx$day)
zx <- zx %>% filter(day != 11) %>% na.omit(.) 

ggplot(zx, aes(day,avg, color = glicko_rank))+
  geom_line(aes(group = glicko_rank), size = 1, alpha=.8)+
  geom_point(aes(group =avg), alpha = .5, size = 3)+
  scale_color_manual(values = viridis::viridis(6)) +
  scale_fill_manual(values = viridis::viridis(6)) +
  theme_minimal()+
  labs(color = "Rank")+
  scale_y_continuous(limits =c(0, 1250),
                     labels = c(0, seq(250, 1250, by = 500)))+
  theme( strip.background  = element_rect(fill = NA, color = "black"),
         axis.text.x = element_text(size= 10) ,
         axis.text.y = element_text(size= 10),
         strip.text = element_text(size = 11),
         text = element_text(size= 15))+
  ylab("Average Food Cage Entries ") +
  xlab("Day")



##################


jp <- dms %>% filter(cohort != 6)%>% group_by(glicko_rank,day) %>% 
  mutate(.,zd = start-lag(start)) %>% 
 mutate(., total = sum(!is.na(zd))) %>% ungroup(.)
head(jp)

colnames(jp1)

jp1 <- jp %>% dplyr::select(2,3,5,7,13) %>% unique(.) 
head(jp1)


jp2 <- jp1 %>% group_by(mouse,cohort) %>% mutate(acum = cumsum(total))
jp2$day <- as.factor(jp2$day)
jp2 <- na.omit(jp2)
jp2 <- jp2 %>% filter(day !=11)
head(jp2)


 p2 <- ggplot(jp2, aes(day,acum, color = glicko_rank))+
  geom_line(aes(group = glicko_rank), size = 1, alpha=.8)+
  geom_point(aes(group =acum), alpha = .6, size = 3)+
  scale_color_manual(values = viridis::viridis(6)) +
  scale_fill_manual(values = viridis::viridis(6)) +
  theme_minimal()+
  labs(color = "Rank")+
  theme( strip.background  = element_rect(fill = NA, color = "black"),
         axis.text.x = element_text(size= 15) ,
         axis.text.y = element_text(size= 15),
         strip.text = element_text(size =20),
         text = element_text(size= 20))+
  ylab("Total Cage Transitions") +
  xlab("Day")

ggsave("RFID_stable_cohorts/imgs/total_cage_changes.png", p2,height = 5, width = 5, dpi = 300)


##for SBN 

cpep <- cpep %>% mutate(analyte = "C-Peptide2")
sbn <- ins %>% rbind(ghr,cpep) %>% filter(time == "GH")
sbn$glicko_rank <- as.numeric(sbn$glicko_rank)
tot <- jp2 %>% filter(glicko_rank != 2)%>% filter(glicko_rank != 3)%>% filter(glicko_rank != 4)%>% filter(glicko_rank != 5)
tot$glicko_rank <- as.numeric(tot$glicko_rank)

sbn2 <- sbn %>% full_join(tot) %>% filter(day == 10) 

sbn2 <- na.omit(sbn2)

sbn.list <- sbn2 %>%  split(.$analyte)

sbn.list %>% map(~cor.test(., value, acum))


x <- sbn2 %>%  filter(analyte == "") %>% filter(glicko_rank == 1)

cor.test(x$value, x$acum)
sbn2$analyte <- factor(sbn2$analyte, levels = c("Insulin", "C-Peptide2", "Ghrelin"))
 p2 <- ggplot(sbn2, aes(acum, value, color = as.factor(glicko_rank), fill = as.factor(glicko_rank)))+
  geom_point(size = 3, shape = 21, alpha = 0.6)+
  geom_smooth(method = "lm", alpha = 0.2, size = 1.2,se =F)+
  scale_color_manual(name="GH Rank",values = viridis::viridis(2)) +
  scale_fill_manual(name="GH Rank",values = viridis::viridis(2))+
  labs(y = "GH Concentration (pg/ml)",
       x = "Total Cage Transitions") +
  facet_wrap(~analyte, scales = "free_y")+ 
  theme(axis.text.x = element_text(vjust = 1, size = 15),
        axis.text.y = element_text(hjust = 0.5, size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.title = element_text(size = 20),
        axis.text= element_text(color="#3C3C3C", size=20),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 20)
  )

 
 p2
ggsave("RFID_stable_cohorts/imgs/met_tot.png", p2, width = 14, height = 5, dpi = 300)
 



cpep <- cpep %>% mutate(analyte = "C-Peptide2")
sbn <- ghr %>% rbind(ins,cpep) %>% filter (time == "PH")
sbn$glicko_rank <- as.numeric(sbn$glicko_rank)
tot <- jp2 %>% filter(glicko_rank != 2)%>% filter(glicko_rank != 3)%>% filter(glicko_rank != 4)%>% filter(glicko_rank != 5)
tot$glicko_rank <- as.numeric(tot$glicko_rank)

sbn2 <- sbn %>% full_join(tot) %>% filter(day == 10) %>% unique(.)

sbn2 <- na.omit(sbn2)
# sbn2$time <- factor(sbn2$time, levels =c("Pre", "Post"))
# sbn2$time <- ifelse(sbn2$time == "Post",'Post Concentration (pg/ml)', "Pre Concentration (pg/ml)")

 sbn2$analyte <- factor(sbn2$analyte, levels = c("Insulin", "C-Peptide2", "Ghrelin"))

 df <- read_csv("RFID_stable_cohorts/data_raw/id_data.csv")
 head(df) 
 
 df1 <- df %>% dplyr::select(1,3,11, 13)
 sbn2 <- sbn2 %>% full_join(df1)
 
 head(sbn2)
 
 sbn2 <- na.omit(sbn2)
 
 
 
 sbn2$pre_rank <- ifelse(sbn2$pre_rank == "D", 1, 6)
 p3 <- ggplot(sbn2, aes(acum, value, color = as.factor(glicko_rank), fill = as.factor(glicko_rank)))+
  geom_point(size = 3, shape = 21, alpha = 0.6)+
  geom_smooth(method = "lm", alpha = 0.2, size = 1.2,se =F)+
  scale_color_manual(name="GH Rank",values = viridis::viridis(2)) +
  scale_fill_manual(name="GH Rank",values = viridis::viridis(2))+
  labs(y = "PH Concentration (pg/ml)",
       x = "Total Cage Transitions") +
  facet_wrap(~analyte, scales = "free_y")+ 
  theme(axis.text.x = element_text(vjust = 1, size = 15),
        axis.text.y = element_text(hjust = 0.5, size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.title = element_text(size = 20),
        axis.text= element_text(color="#3C3C3C", size=20),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 20))


p3
p2

mt <- gridExtra::grid.arrange(p3,p2)

ggsave("RFID_stable_cohorts/imgs/met_tot_pp_GH.png", mt, width = 14, height = 10, dpi = 300)





 hist(jp2$acum)
jp2$day <- as.integer(jp2$day)
hist(jp2$total)

jp2$glicko_rank <- factor(jp2$glicko_rank, levels= c(2,3,4,5,6,1)) 

tm <-glmer(total~glicko_rank+day +(1|cohort), data =jp2,family = Gamma(link = "log"))
summary(tm)
AIC(pre.glm)

zx%>% group_by(glicko_rank) %>% 
  summarize(med = median(total), q25 = quantile(total,0.25),  q75 = quantile(total,0.75))

zx$glicko_rank <- as.factor(zx$glicko_rank)
ggplot(zx, aes(glicko_rank, avg))+
  geom_boxplot()+
  geom_jitter()


zx$glicko_rank <- factor(zx$glicko_rank, level = c(2,3,4,5,6,1))

tm <-lmer(total~glicko_rank+ (1|mouse)+(1|cohort), data =zx)
summary(tm)
AIC(pre.glm)


zx1 <- zx %>% filter(cohort !=6)

zx1 %>% arrange(-avg)

 ggplot(zx1, aes(glicko_rank, avg, color = glicko_rank))+
  geom_boxjitter(aes(fill = glicko_rank),outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  scale_color_manual(values = viridis::viridis(6)) +
  scale_fill_manual(values = viridis::viridis(6)) +
  theme_classic()+
  theme(legend.position = "none", 
        axis.text.x = element_text(size= 20) ,
        axis.text.y = element_text(size= 20),
        strip.text = element_text(size = 20),
        text = element_text(size= 25))+
  ylab("Average Cage Changes per Day") +
  xlab("Mouse Rank ")



         