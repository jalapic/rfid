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

m$Secretin <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m$Secretin))

#join rank data
#we didn't have plasma for C10M5
mdf <- m %>% full_join(rank) %>% filter(ID != "C10M5")
head(mdf)


#pre data 

pre<- mdf %>% filter(time == "Pre") %>% select(-Secretin)
head(pre)

pre.long <- pre %>% pivot_longer(cols = 2:10, names_to="analyte") 
pre.long1 <- pre.long %>% filter(analyte !='Cpeptide2')%>% filter(analyte !='Insulin')%>% filter(analyte !='Leptin')

ci <- pre.long %>% filter(analyte !='Amylin')%>% filter(analyte !='IL6')%>% filter(analyte !='Ghrelin')%>% 
  filter(analyte !='MCP1_CCL2') %>% filter(analyte !="PYY") %>% filter(analyte !="TNFa")

ggplot(pre.long1, aes(dom, value))+
  geom_boxplot() +
  geom_jitter()+
  facet_wrap(~analyte,  scales="free_y")+
  theme_minimal()

ggplot(ci, aes(dom, value))+
  geom_boxplot() +
  geom_jitter()+
  facet_wrap(~analyte)+
  theme_minimal()


#post 
post<- mdf %>% filter(time == "Post") %>% select(-Secretin)
head(post)


post.long <- post %>% pivot_longer(cols = 2:10, names_to="analyte")  

post.long1 <- post.long %>% filter(analyte !='Cpeptide2')%>% filter(analyte !='Insulin')%>% filter(analyte !='Leptin')

ci <- post.long %>% filter(analyte !='Amylin')%>% filter(analyte !='IL6')%>% filter(analyte !='Ghrelin')%>% 
  filter(analyte !='MCP1_CCL2') %>% filter(analyte !="PYY") %>% filter(analyte !="TNFa")

ggplot(post.long1, aes(dom, value))+
  geom_boxplot() +
  geom_jitter()+
  facet_wrap(~analyte, scales = "free_y")+
  theme_minimal()

ggplot(ci, aes(dom, value))+
  geom_boxplot() +
  geom_jitter()+
  facet_wrap(~analyte)+
  theme_minimal()


library(lmerTest)
ins <- mdf %>% select(Insulin, time, cohort, mouse, dom) 
cpep <- mdf %>% select(Cpeptide2, time, cohort, mouse, dom)
il6 <-  mdf %>% select(IL6, time, cohort, mouse, dom)
lep <- mdf %>% select(Leptin, time, cohort, mouse, dom)
pyy <- mdf %>% select(PYY, time, cohort, mouse, dom)
ghr <- mdf %>% select(Ghrelin, time, cohort, mouse, dom)
MC <- mdf %>% select(MCP1_CCL2, time, cohort, mouse, dom)
amy <- mdf %>% select(Amylin, time, cohort, mouse, dom)
tnf <-mdf %>% select(TNFa, time, cohort, mouse, dom) %>% filter(time == 'Post')

sec <-mdf %>% select(Secretin, time, cohort, mouse, dom)
hist(lep$Leptin)

insx<-glmer(Insulin~dom+time+(1|mouse)+(1|cohort), data =ins,family = Gamma(link = "log"))
summary(insx) # sign pre and post, no effect of group housing

cpepx<-glmer(Cpeptide2~dom+time+(1|mouse)+(1|cohort), data =cpep,family = Gamma(link = "log"))
summary(cpepx)#sign pre and post, increase with group housing

il6x<-glmer(IL6~dom+time+(1|cohort), data =il6,family = Gamma(link = "log"))
summary(il6x) # just pre significant not post, no effect of group housing

lepx<-glmer(Leptin~dom+time+(1|mouse)+(1|cohort), data =lep,family = Gamma(link = "log"))
summary(lepx) # different pre to post group housing but no effect on status 

pyyx<-glmer(PYY~dom+time+(1|mouse)+(1|cohort), data =pyy,family = Gamma(link = "log"))
summary(pyyx) # different pre to post group housing but no effect on status 

ghrx<-glmer(Ghrelin~dom+time+(1|mouse)+(1|cohort), data =ghr,family = Gamma(link = "log"))
summary(ghrx) #sign pre and post, no effect on group housing

hist(MC$MCP1_CCL2)
MCx<-lmer(MCP1_CCL2~dom+time+(1|mouse)+(1|cohort), data =MC)
summary(MCx) #nothing is significant

amyx<-glmer(Amylin~dom+(1|mouse)+(1|cohort), data =amy,family = Gamma(link = "log"))
summary(amyx) #nothing is significant

tnfx<-glmer(TNFa~dom+(1|mouse)+(1|cohort), data =tnf,family = Gamma(link = "log"))
summary(tnfx) #sign pre group housing. 


secx<-glmer(Secretin~dom+time+(1|mouse)+(1|cohort), data =sec,family = Gamma(link = "log"))
summary(secx) #sign pre group housing, decreases with group housing?


## individual differences 
head(mdf)

df.long <- mdf %>% pivot_longer(cols = 2:11, names_to="analyte")  %>% filter(dom != "Subdominant")%>% 
  filter(dom != "3") %>%   filter(dom != "4") %>%   filter(dom != "5")



a.list <- df.long %>% split(.$analyte)

stat <- a.list %>% 
  map(~group_by(., dom, time)) %>% 
  map(~mutate(.,mean_post = mean(value),
                sd_post = sd(value),
                n = n(),
                median_post = median(value))) %>% 
  map(~mutate(.,semx = sd_post/sqrt(n))) %>% 
  map(~filter(.,!is.na(semx))) %>% 
  map(~mutate(., lower_meanp = mean_post + qt((1-0.95)/2, n - 1) * semx,
               upper_meanp = mean_post - qt((1-0.95)/2, n - 1) * semx)) %>% 
  map(~ungroup(.))



df.s <- do.call(rbind,stat)

head(df.s)
str(df.s)
df.s$dom <- as.factor(df.s$dom)
df.s$ID2 <- paste(df.s$cohort, df.s$mouse)
df.s$time <- factor(df.s$time, levels = c("Pre", "Post"))


ggplot(df.s, aes(x=time , y = value, group = dom)) +
  geom_line(aes(group = ID2), color= "gray", size = .6, alpha=.5) +
  geom_point(aes(group = dom), color ="gray", alpha = 2, size = .6)+
  # geom_ribbon(aes(ymin = lower_meanp,
                   # ymax = upper_meanp, group=dom,fill=dom)) +
  geom_line(aes(y=mean_post, group = dom, color = dom), size=1.5)+
  labs(   x = "",
          y=  "Concentration (pg/ml)") +
  scale_color_manual(values = viridis::viridis(3)) +
   facet_wrap(~ analyte, scales ="free") +
  # facet_wrap(vars(analyte, dom), scales = "free")+
  newggtheme 
