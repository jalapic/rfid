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

m1$BDNF <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m1$BDNF))
m1$GH <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m1$GH))
m1$Prolactin <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m1$Prolactin))
m1$TSH <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m1$TSH))
m1$LH <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m1$LH))


m1 <- m1 %>% filter(BDNF <20) %>% filter(GH <12000)

df <- m1 %>% left_join(rank)
head(df)


df$dom2 <- ifelse(df$dom == "Dominant", "Dominant", "Subordinate")


df1 <- df %>% filter(time == "Post")
df1$ID <- substr(df1$ID, 5,9)
df1$ID <- gsub("_", "", df1$ID)
df1$ID 

df.long <- df1 %>% pivot_longer(cols = 2:7, names_to="analyte") 


ggplot(df.long, aes(dom, value, color = dom, fill = dom))+
  geom_boxjitter(outlier.color = NA, jitter.shape = 21,
                 alpha = 0.4,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  facet_wrap(~analyte, scales = "free_y")+
  labs(title = "",
       x = "",
       y = "Concentration (pg/ml)") +
  scale_color_manual(values = viridis::viridis(3)) +
  scale_fill_manual(values = viridis::viridis(3))+theme_classic() +
  theme(axis.text.x = element_text(vjust = 1, size = 15),
        legend.position = 'none',
        axis.text.y = element_text(hjust = 0.5, size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.title = element_text(size = 20),
        axis.text= element_text(color="#3C3C3C", size=20),
        strip.background = element_blank() 
  )



endo <- ggplot(df.long, aes(dom2, value, color = dom2))+
  geom_boxjitter(aes(fill = dom2), outlier.color = NA, jitter.shape = 21,
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

 # ggsave("img/endocrineDS.png", endo,width = 14, height = 12)


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


#Comparing sub and dominants
acth <- df1%>% select(ACTH, cohort, mouse, glicko_rank, dom2) 
bdnf <- df1 %>% select(BDNF,  cohort, mouse,glicko_rank, dom2)
fsh <-  df1 %>% select(FSH, cohort, mouse,glicko_rank, dom2)
gh <- df1 %>% select(GH,  cohort, mouse,glicko_rank, dom2)
lh <- df1 %>% select(LH,  cohort, mouse,glicko_rank, dom2)
pro <- df1 %>% select(Prolactin, cohort,glicko_rank, mouse, dom2)
tsh <- df1 %>% select(TSH,  cohort, mouse,glicko_rank, dom2)


hist(acth$ACTH) #non-normalish?
acthx<-glmer(ACTH~dom2+(1|mouse)+(1|cohort), data =acth, family = Gamma(link = "log"))
summary(acthx) #non significant 

hist(bdnf$BDNF) #normal-ish?
bdnfx<-lmer(BDNF~dom2+(1|mouse)+(1|cohort), data =bdnf)
summary(bdnfx)# nothing is significant 

hist(fsh$FSH)
fx<-glmer(FSH~dom2+(1|mouse)+(1|cohort), data =fsh,family = Gamma(link = "log") )
summary(fx)# nothing is significant 

hist(gh$GH) #not normal 
gx<-glmer(GH~dom2+(1|mouse)+(1|cohort), data =gh, family = Gamma(link = "log"))  
summary(gx)#Model isn't working -- maybe is fine?

acf(resid(gx))
qqPlot(resid(gx))
hist(resid(gx))
plot(gx)#not great... 

durbinWatsonTest(resid(gx))
shapiro.test(resid(gx))#normal


hist(lh$LH) #not normal 
lx<-lmer(LH~dom2+(1|mouse)+(1|cohort)+(1|glicko_rank), data =lh)
summary(lx)# # nothing is significant 

acf(resid(lx))
qqPlot(resid(lx))
hist(resid(lx))
plot(lx)#wtf?

durbinWatsonTest(resid(lx))
shapiro.test(resid(lx))#not normal


hist(pro$Prolactin) #not normal 
px<-glmer(Prolactin~dom2+(1|mouse)+(1|cohort), data =pro, family = Gamma(link = "log"))
summary(px)# #subordinate lower p < .0001

acf(resid(px))
qqPlot(resid(px))
hist(resid(px))
plot(px)#not great... 

durbinWatsonTest(resid(px))
shapiro.test(resid(px))#normal


hist(tsh$TSH) #not normal 
tx<-glmer(TSH~dom2+(1|mouse)+(1|cohort), data =tsh, family = Gamma(link = "log"))
summary(tx)# # subordinates higher  p = 0.00237 

acf(resid(tx))
qqPlot(resid(tx))
hist(resid(tx))
plot(tx)#not great... 

durbinWatsonTest(resid(tx))
shapiro.test(resid(tx))#normal
