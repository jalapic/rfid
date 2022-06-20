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


m.long <-mdf %>% pivot_longer(cols = 2:11, names_to="analyte") 
m.long$ID <- substr(m.long$ID, 5,9)
m.long$ID <- gsub("_", "", m.long$ID)

m.long$time <- factor(m.long$time, levels = c("Pre", "Post"))
###
 ggplot(m.long, aes(x=time , y = value)) +
  geom_line(aes(group = ID, color=dom), size = .75, alpha = .75) +
  geom_point(aes(color=dom), alpha = .75, size = 1)+
  scale_color_manual(values = c("#238A8DFF", "#FDE725FF"), name = "Rank")+
  labs(x = "",
          y=  "Concentration (pg/ml)") +
  facet_wrap(~analyte, scales = "free_y")+
  theme(axis.text.x = element_text(vjust = 1, size = 15),
        axis.text.y = element_text(hjust = 0.5, size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.title = element_text(size = 20),
        axis.text= element_text(color="#3C3C3C", size=15),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 15)
  )



#pre data 

pre<- mdf %>% filter(time == "Pre") 
head(pre)

pre.long <- pre %>% pivot_longer(cols = 2:11, names_to="analyte") 
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

ci %>% group_by(dom, analyte) %>% summarise(mean = mean(analyte, na.rm =F))


#post 
post<- mdf %>% filter(time == "Post")
head(post)


post.long <- post %>% pivot_longer(cols = 2:11, names_to="analyte")  

post.long1 <- post.long %>% filter(analyte !='Cpeptide2')%>% filter(analyte !='Insulin')%>% filter(analyte !='Leptin')

ci <- post.long %>% filter(analyte !='Amylin')%>% filter(analyte !='IL6')%>% filter(analyte !='Ghrelin')%>% 
  filter(analyte !='MCP1_CCL2') %>% filter(analyte !="PYY") %>% filter(analyte !="TNFa")

ggplot(ci, aes(dom, value))+
  geom_boxplot() +
  geom_jitter()+
  facet_wrap(~analyte, scales = "free_y")+
  theme_minimal()


pre <- m.long %>% filter(time == "Pre")

post <- m.long %>% filter(time == "Post")

diff <- m.long %>%pivot_wider(., names_from = time, values_from = value) %>% 
  mutate(diff = Pre - Post)

ggplot(diff, aes(dom, diff,color =dom, fill = dom))+
  geom_boxjitter(outlier.color = NA, jitter.shape = 21,
                 alpha = 0.4,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  facet_wrap(~analyte, scales = "free_y")+
  labs(title = "",
       x = "",
       y = " Difference in Concentration (pg/ml)") +
  scale_color_manual(values = viridis::viridis(4)) +
  scale_fill_manual(values = viridis::viridis(4))+theme_classic() +
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



library(lmerTest)
d.list <- diff %>% split(.$analyte)


names(d.list)

ins <- d.list$Insulin
cpep <- d.list$Cpeptide2
il6 <-  d.list$IL6
lep <- d.list$Leptin
pyy <- d.list$PYY
ghr <- d.list$Ghrelin
MC <- d.list$MCP1_CCL2
amy <- d.list$Amylin
tnf <-d.list$TNFa
sec <- d.list$Secretin
hist(lep$Leptin)

insx<-lmer(diff~dom+(1|mouse)+(1|cohort), data =sec)
summary(insx) 
acf(resid(insx))
qqPlot(resid(insx))
hist(resid(insx))
plot(insx)

durbinWatsonTest(resid(insx))
shapiro.test(resid(insx))#normal

ins <- mdf %>% dplyr::select(Insulin, time, cohort, mouse, dom) %>% filter(time == 'Pre')
insx2<-glmer(Insulin~dom+(1|mouse)+(1|cohort), data =ins,family = Gamma(link = "log"))
summary(insx2) 

acf(resid(insx2))
qqPlot(resid(insx2))
hist(resid(insx2))
plot(insx2)

durbinWatsonTest(resid(insx2))
shapiro.test(resid(insx2))#normal


cpepx<-lmer(diff~dom+(1|mouse)+(1|cohort), data =cpep)
summary(cpepx)

acf(resid(cpepx))
qqPlot(resid(cpepx))
hist(resid(cpepx))
plot(cpepx)

durbinWatsonTest(resid(cpepx))
shapiro.test(resid(cpepx))#normal

#Pre group housing 
cpep2 <- mdf %>% dplyr::select(Cpeptide2, time, cohort, mouse, dom)

cpepx2<-glmer(Cpeptide2~dom+(1|mouse)+(1|cohort), data =cpep2,family = Gamma(link = "log"))
summary(cpepx2)

ggplot(cpep2,aes(time,Cpeptide2))+
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(~dom)


acf(resid(cpepx2))
qqPlot(resid(cpepx2))
hist(resid(cpepx2))
plot(cpepx2)# super weird 

durbinWatsonTest(resid(cpepx2))
shapiro.test(resid(cpepx2))#normal

il6x<-glmer(IL6~dom+(1|mouse)+(1|cohort), data =il6,family = Gamma(link = "log"))
summary(il6x) 

il62 <-  mdf %>% dplyr::select(IL6, time, cohort, mouse, dom)%>% filter(time == 'Pre')
il6x2<-glmer(IL6~dom+(1|mouse)+(1|cohort), data =il62,family = Gamma(link = "log"))
summary(il6x2)

acf(resid(il6x2))
qqPlot(resid(il6x2))
hist(resid(il6x2))
plot(il6x2)# super weird 

durbinWatsonTest(resid(il6x2))
shapiro.test(resid(il6x2))#normal













lepx<-glmer(Leptin~dom+(1|mouse)+(1|cohort), data =lep,family = Gamma(link = "log"))
summary(lepx) # different pre to post group housing but no effect on status 

lep2 <- mdf %>% dplyr::select(Leptin, time, cohort, mouse, dom)
lepx2<-glmer(Leptin~dom+time+(1|mouse)+(1|cohort), data =lep2,family = Gamma(link = "log"))
summary(lepx2) 

ggplot(lep2,aes(time,Leptin))+
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(~dom)

hist(pyy$PYY)

pyyx<-lmer(PYY~dom+(1|mouse)+(1|cohort), data =pyy)
summary(pyyx)

pyy <- mdf %>% dplyr::select(PYY, time, cohort, mouse, dom)%>% filter(time == 'Pre')

ghrx<-glmer(Ghrelin~dom+(1|mouse)+(1|cohort), data =ghr,family = Gamma(link = "log"))
summary(ghrx) 

acf(resid(ghrx))
qqPlot(resid(ghrx))
hist(resid(ghrx))
plot(ghrx)# super weird 

durbinWatsonTest(resid(ghrx))
shapiro.test(resid(ghrx))#normal


ghr2 <- mdf %>% dplyr::select(Ghrelin, time, cohort, mouse, dom)%>% filter(time == 'Pre')

ghrx2<-glmer(Ghrelin~dom+(1|mouse)+(1|cohort), data =ghr2,family = Gamma(link = "log"))
summary(ghrx2) 


acf(resid(ghrx2))
qqPlot(resid(ghrx2))
hist(resid(ghrx2))
plot(ghrx2)# super weird 

durbinWatsonTest(resid(ghrx2))
shapiro.test(resid(ghrx2))#normal


hist(MC$MCP1_CCL2)
MCx<-lmer(MCP1_CCL2~dom+time+(1|mouse)+(1|cohort), data =MC)
summary(MCx)
MC <- mdf %>% dplyr::select(MCP1_CCL2, time, cohort, mouse, dom)





amyx<-glmer(Amylin~dom+(1|mouse)+(1|cohort), data =amy,family = Gamma(link = "log"))
summary(amyx) #nothing is significant

amy2 <- mdf %>% dplyr::select(Amylin, time, cohort, mouse, dom)
amyx<-glmer(Amylin~dom+time+(1|mouse)+(1|cohort), data =amy2,family = Gamma(link = "log"))
summary(amyx) #nothing is significant

ggplot(amy2,aes(time,Amylin))+
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(~dom)

acf(resid(amyx))
qqPlot(resid(amyx))
hist(resid(amyx))
plot(amyx)# super weird 

durbinWatsonTest(resid(amyx))
shapiro.test(resid(amyx))#normal


tnfx<-glmer(TNFa~dom+(1|mouse)+(1|cohort), data =tnf,family = Gamma(link = "log"))
summary(tnfx) #nothing significant 

tnf <-mdf %>% dplyr::select(TNFa, time, cohort, mouse, dom) 
tnfx<-glmer(TNFa~dom+time+(1|mouse)+(1|cohort), data =tnf,family = Gamma(link = "log"))
summary(tnfx)  

sec <-mdf %>% dplyr::select(Secretin, time, cohort, mouse, dom) %>% filter(time == "Post")
hist(sec$Secretin)
secx<-glmer(Secretin~dom+(1|mouse)+(1|cohort), data =sec,family = Gamma(link = "log"))
summary(secx) #sign pre group housing, decreases with group housing?
ggplot(sec,aes(time,Secretin))+
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(~dom)

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
  facet_wrap(vars(analyte, dom), scales = "free")
  # newggtheme 


# just getting pre and post with insulin and cpeptide
mdf.long <- mdf %>% pivot_longer(cols = 2:10, names_to="analyte")  

ci <- mdf.long %>% filter(analyte !='Amylin')%>% filter(analyte !='IL6')%>% 
  filter(analyte !='Ghrelin')%>% filter(analyte !='MCP1_CCL2') %>% 
  filter(analyte !="PYY") %>% filter(analyte !="TNFa")%>% 
  filter(analyte !="Secretin")%>% filter(analyte !="Leptin")

a.list <- ci %>% split(.$analyte)


stat <- a.list %>% 
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

ci <- ci %>% full_join(df.s)

ci$ID2 <- paste(ci$cohort, ci$mouse)

ggplot(ci, aes(dom, value))+
  geom_boxplot() +
  geom_line(aes(group = cohort), size = .6, alpha=.5) +
  geom_point(alpha = 1, size = 1.5)+
  facet_grid(~analyte, scales = "free_y")+
  theme_minimal()


corr <- ci %>% select(ID,time, cohort, mouse, glicko_rank, dom, analyte, value, ID2)
corr

corr <- corr %>% 
  group_by(., dom, time) %>% 
  mutate(.,mean_post = mean(value),
         sd_post = sd(value),
         n = n(),
         median_post = median(value)) %>% 
  mutate(.,semx = sd_post/sqrt(n)) %>% 
  filter(.,!is.na(semx)) %>% 
  mutate(., lower_meanp = mean_post + qt((1-0.95)/2, n - 1) * semx,
         upper_meanp = mean_post - qt((1-0.95)/2, n - 1) * semx) %>% 
  ungroup(.)

corr1 <- corr %>% pivot_wider(names_from = analyte, values_from = value) %>% filter(time == "Post")
corr1$time <- factor(corr1$time, levels = c("Pre", "Post"))





ggplot(corr1, aes(Insulin, Cpeptide2,color = dom))+
  geom_point()+
  stat_smooth(method = "lm", se= F, aes(color = dom))+
  facet_wrap(~cohort)+
   theme_minimal()


ggplot(corr1, aes(Insulin, Cpeptide2, shape = time, color = dom))+
  geom_point()+
  geom_line(glicko_rank)+
  facet_wrap(~cohort)+
  theme_minimal()
