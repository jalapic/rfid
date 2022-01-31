library(tidyverse)



id <- read.csv("RFID_stable_cohorts/data_raw/id_data.csv")
head(id)

df <- read.csv("RFID_stable_cohorts/data_clean/socialbehavior/rank_data.csv")
head(df)

idx <- id %>% select(cohort,pre_id, pre_rank, mouse) 
dfx <- df %>% select(cohort, mouse, glicko_rank)

x <- idx %>% full_join(dfx) %>%  filter(glicko_rank != 3)%>%  filter(glicko_rank != 4) %>%  filter(glicko_rank != 5)

write.csv(x,"RFID_stable_cohorts/data_carpentry/physiology_data/plasma.csv",row.names = F )
