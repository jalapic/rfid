library(tidyverse)


#source
#rank data
rank <- read_csv("RFID_stable_cohorts/data_clean/socialbehavior/rank_data.csv")
head(rank)

rank$dom <- ifelse(rank$glicko_rank == 1, "Dominant", rank$glicko_rank)
rank$dom <- ifelse(rank$glicko_rank == 2, "Subdominant", rank$dom)
rank$dom <- ifelse(rank$glicko_rank == 6, "Subordinate", rank$dom)

#metabolic hormone data 
m <- read_csv("RFID_stable_cohorts/data_raw/physiological/metabolic_copy.csv")
head(m)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
# get cohort and mouse ids
m$time <- ifelse(grepl("pre",m$ID), "Pre", "Post")
m$cohort  <-  as.numeric(gsub(".*?([0-9]+).*", "\\1", m$ID))
m$mouse <- as.numeric(substrRight(m$ID,1))

m$Secretin <- as.numeric(regmatches(m$Secretin,regexpr("[[:digit:]]+\\.[[:digit:]]+",m$Secretin)))

#join rank data
#we didn't have plasma for C10M5 - one last dominant mouse
mdf <- m %>% full_join(rank) %>% filter(ID != "C10M5")
head(mdf)


library(tidyverse)
library(MASS)
library(car)

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

#filtering out C10M2, C4_M2, C10_M5, more than 2 out of range. 
#GH: sample C7_M6 and BDNF sample C7_M2  10X+ higher than all other samples 
# C10M2 - SUB
# C4_M2 - SUB
# C10_M5 - DOM
# C7_M6 - SUB
# C7_M2 - DOM

m1 <- m  %>% filter(ID != "post_C10M2")%>% 
  filter(ID != "post_C10M5") %>% 
  filter(ID != "post_C4M2")


head(m1)

m$BDNF <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m$BDNF))
m1$GH <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m1$GH))
m1$Prolactin <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m1$Prolactin))
m1$TSH <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m1$TSH))
m1$LH <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m1$LH))


m1 <- m1 %>% filter(BDNF <20) %>% filter(GH <12000)

df <- m1 %>% left_join(rank)
head(df)


df$dom2 <- ifelse(df$dom == "Dominant", "Dominant", "Subordinate")
df1 <- df %>% filter(dom != "Subdominant")
colnames(df1)
df1x <- df1 %>% dplyr::select(1:13)

all <- mdf %>% full_join(df1x)


write.csv(all, "RFID_stable_cohorts/data_clean/all_blood.csv", row.names =F)
