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

m1$BDNF <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m1$BDNF))
m1$GH <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m1$GH))
m1$Prolactin <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m1$Prolactin))
m1$TSH <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m1$TSH))
m1$LH <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m1$LH))


m1 <- m1 %>% filter(BDNF <20) %>% filter(GH <20000)%>% 
  select(-ID)

df <- m1 %>% left_join(rank)
head(df)



df1 <- df %>% filter(time == "Post")


df.long <- df %>% pivot_longer(cols = 1:7, names_to="analyte") 


ggplot(df.long, aes(dom, value))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter()+
  facet_wrap(~analyte,  scales="free_y")+
  theme_minimal()



endo <- ggplot(df.long, aes(dom, value, color = dom))+
  geom_boxjitter(aes(fill = dom), outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  facet_wrap(~analyte, scales = 'free_y')+
  scale_color_manual(values = viridis::viridis(3)) +
  scale_fill_manual(values = viridis::viridis(3)) +
  theme_classic()+
  theme(legend.position = "none", 
        axis.text.x = element_text(size= 15) ,
        axis.text.y = element_text(size= 15),
        strip.text = element_text(size = 15),
        text = element_text(size= 20))+
  ylab("Concentration pg/ml") +
  xlab("")

# ggsave("img/endocrine.png", endo,width = 14, height = 12)


library(lmerTest)
acth <- df1%>% select(ACTH, cohort, mouse, dom) 
bdnf <- df1 %>% select(BDNF,  cohort, mouse, dom)
fsh <-  df1 %>% select(FSH, cohort, mouse, dom)
gh <- df1 %>% select(GH,  cohort, mouse, dom)
lh <- df1 %>% select(LH,  cohort, mouse, dom)
pro <- df1 %>% select(Prolactin, cohort, mouse, dom)
tsh <- df1 %>% select(TSH,  cohort, mouse, dom)

##statistics


hist(acth$ACTH) #pretty normal 
acthx<-lmer(ACTH~dom+(1|mouse)+(1|cohort), data =acth)
summary(acthx) #subdominant - 0.077 barely lower

hist(bdnf$BDNF) #normal-ish?
bdnfx<-lmer(BDNF~dom+(1|mouse)+(1|cohort), data =bdnf)
summary(bdnfx)# nothing is significant 

hist(fsh$FSH)
fx<-lmer(FSH~dom+(1|mouse)+(1|cohort), data =fsh)
summary(fx)# nothing is significant 

hist(gh$GH) #not normal 
gx<-glmer(GH~dom+(1|mouse)+(1|cohort), data =gh, family = Gamma(link = "log"))
summary(gx)# #subordinate barely lower 0.0574

hist(lh$LH) #not normal 
lx<-glmer(LH~dom+(1|mouse)+(1|cohort), data =lh, family = Gamma(link = "log"))
summary(lx)# # nothing is significant 

hist(pro$Prolactin) #not normal 
px<-glmer(Prolactin~dom+(1|mouse)+(1|cohort), data =pro, family = Gamma(link = "log"))
summary(px)# #subordinate barely lower 0.0574


hist(tsh$TSH) #not normal 
tx<-glmer(TSH~dom+(1|mouse)+(1|cohort), data =tsh, family = Gamma(link = "log"))
summary(tx)# # nothing is significant 





