library(tidyverse)

#source
#rank data
rank <- read_csv("RFID_stable_cohorts/data_clean/socialbehavior/rank_data.csv")
head(rank)

rank$dom <- ifelse(rank$glicko_rank == 1, "Dominant", rank$glicko_rank)
rank$dom <- ifelse(rank$glicko_rank == 2, "Subdominant", rank$dom)
rank$dom <- ifelse(rank$glicko_rank == 6, "Subordinate", rank$dom)

#metabolic hormone data 
m <- read_csv("RFID_stable_cohorts/data_raw/physiological/endocrine_copy.csv")
head(m)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
# get cohort and mouse ids
m$time <- ifelse(grepl("pre",m$ID), "Pre", "Post")
m$cohort  <-  as.numeric(gsub(".*?([0-9]+).*", "\\1", m$ID))
m$mouse <- as.numeric(substrRight(m$ID,1))

m1 <- m %>% select(-ID)
head(m1)

m1$BDNF <- as.numeric(gsub("\\D", "", m1$BDNF))
m1$GH <- as.numeric(gsub("\\D", "", m1$GH))
m1$Prolactin <- as.numeric(gsub("\\D", "", m1$Prolactin))
m1$TSH <- as.numeric(gsub("\\D", "", m1$TSH))
m1$LH <- as.numeric(gsub("\\D", "", m1$LH))


df <- m1 %>% full_join(rank)
head(df)



df1 <- df %>% filter(time == "Post")

df.long <- df %>% pivot_longer(cols = 1:7, names_to="analyte") 

df.t <- df.long %>% filter(analyte == "GH") %>% filter(value <75000)

ggplot(df.t, aes(dom, value))+
  geom_boxplot() +
  geom_jitter()+
  facet_wrap(~analyte,  scales="free_y")+
  theme_minimal()

