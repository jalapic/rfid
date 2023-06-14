library(tidyverse)


ts <- readRDS("RFID_stable_cohorts/data_clean/tracking_byday.RDS")


rank <- read_csv("RFID_stable_cohorts/data_clean/socialbehavior/rank_data.csv")
head(rank)
rank$dom <- ifelse(rank$glicko_rank == 1, "Dominant", rank$glicko_rank)
rank$dom <- ifelse(rank$glicko_rank == 2, "Subdominant", rank$dom)
rank$dom <- ifelse(rank$glicko_rank == 6, "Subordinate", rank$dom)

rank <- rank %>% dplyr::select(cohort, mouse, glicko_rank)

head(ts)
head(rank)

dms <- rank %>% full_join(ts)
head(dms)

dms$glicko_rank <- factor(dms$glicko_rank, levels = c (1,2,3,4,5,6))
dms$zone <- as.factor(dms$zone)


table(dms$zone)

#there is some NAS in the zone - need to fix later. 
total <- na.omit(total)

total <- dms %>% group_by(cohort,mouse,glicko_rank,day,zone) %>%
  summarize(tot_ms = sum(duration, na.rm = T)) %>% unique(.)

total

total <- total %>% filter(day != 11) # need to fix days later. 


la <- dms %>% split(.$cohort)
lapply(la, head)


la2 <- la %>% map(~group_by(.,day,glicko_rank)) %>% 
  map(~mutate(.,zd = start-lag(start))) %>% map(~group_by(.,day,glicko_rank,zone)) %>% 
  map(~mutate(., total = sum(!is.na(zd))))
dfnames <- c(1:10)

zx <- dms %>% group_by(cohort,glicko_rank,day,zone) %>% 
  mutate(.,zd = start-lag(start)) %>% 
  mutate(., total = sum(!is.na(zd)))
head(zx)

move <- zx %>% dplyr::select(cohort,mouse,day,glicko_rank,zone,total) %>% unique(.)
move$glicko_rank <- as.numeric(move$glicko_rank)
saveRDS(move, "RFID_stable_cohorts/data_clean/cagechangebyzone_day.RDS")

df <- read_csv("RFID_stable_cohorts/data_clean/all_blood.csv")
head(df)


rfid_all <- df %>% full_join(move)
saveRDS(rfid_all, "RFID_stable_cohorts/data_clean/RFID_blood_fordiscriminateanalysis.RDS")
