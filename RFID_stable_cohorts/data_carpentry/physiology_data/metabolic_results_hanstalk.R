
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

m.g <- m.long %>% filter(time == "Post")
head(m.g)


id <- read_csv("RFID_stable_cohorts/data_raw/id_data.csv")
head(id)

idx <- id %>% select(cohort, mouse, weight_GH10, weight_AT, lightsoff)
head(idx)


m.gw <- m.g %>% full_join(idx)
head(m.gw)

m.gwx <- m.gw %>% group_by(ID) %>% mutate(cm = value/weight_GH10)
m.gwx <- na.omit(m.gwx)
m.gwx$dom <- ifelse(m.gwx$dom == "Dominant", "DOM", "SUB")

m.gwx$analyte <- gsub("MCP1_CCL2", "MCP1", m.gwx$analyte)

ggplot(m.gwx, aes(x=dom , y = cm, group = dom)) +
  # geom_boxjitter(aes(fill = dom), outlier.color = NA, jitter.shape = 21,
                 # alpha = 0.5,
                 # jitter.width = 0.030, errorbar.draw = TRUE,
                 # position = position_dodge(0.85))+
  geom_point(aes(color = dom), size = 3, alpha = 0.5)+
  geom_line(aes(group = cohort), size = .75, alpha = .75) +
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  labs(x = "",
       y=  "Concentration (pg/ml)/ weight (g)") +
  facet_wrap(~analyte, scales = "free_y", ncol = 5)+
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
        strip.text.x = element_text(size = 15),
        legend.position = "none"
  )



ggplot(m.gwx, aes(x=dom , y = cm, group = dom)) +
  geom_boxjitter(aes(fill = dom), outlier.color = NA, jitter.shape = 21,
  alpha = 0.5,
  jitter.height = 0.020, jitter.width = 0.030, errorbar.draw = TRUE,
  position = position_dodge(0.85))+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  labs(x = "",
       y=  "Concentration (pg/ml)/ weight (g)") +
  facet_wrap(~analyte, scales = "free_y", ncol = 5)+
  scale_y_continuous(expand = expansion(.3)) +
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
        strip.text.x = element_text(size = 15),
        legend.position = "none", 
        
  )



## Stats 
library(lmerTest)


d.list <- m.gwx %>% split(.$analyte)

ins <- d.list$Insulin
cpep <- d.list$Cpeptide2
il6 <-  d.list$IL6
lep <- d.list$Leptin
pyy <- d.list$PYY
ghr <- d.list$Ghrelin
MC <- d.list$MCP1
amy <- d.list$Amylin
tnf <-d.list$TNFa
sec <- d.list$Secretin


hist(ins$value)
hist(ins$cm)

#ins  0.00559 ** 
insx<-glmer(value~dom+(1|mouse)+(1|cohort), data =ins,family = Gamma(link = "log"))
summary(insx) 

insx2<-glmer(cm~dom+(1|cohort)+(1|mouse), data =ins,family = Gamma(link = "log"))
summary(insx2) 

insx3<-glmer(cm~dom+lightsoff+(1|mouse)+(1|cohort), data =ins,family = Gamma(link = "log"))
summary(insx3) 

insx4<-glmer(cm~dom+lightsoff+(cohort|mouse), data =ins,family = Gamma(link = "log"))
summary(insx4) #  0.0199 *

acf(resid(insx2))
car::qqPlot(resid(insx2))
hist(resid(insx2))
plot(insx2)

car::durbinWatsonTest(resid(insx2))
shapiro.test(resid(insx2))#normal


#cpep  0.00912 ** 
hist(cpep$value)
hist(cpep$cm)

cpepx<-glmer(value~dom+(1|mouse)+(1|cohort), data =cpep,family = Gamma(link = "log"))
summary(cpepx) 

cpepx2<-glmer(cm~dom+(1|mouse)+(1|cohort), data =cpep,family = Gamma(link = "log"))
summary(cpepx2) 

cpepx3<-glmer(cm~dom+(1|lightsoff)+(1|mouse)+(1|cohort), data =cpep,family = Gamma(link = "log"))
summary(cpepx3) 

cpepx4<-glmer(cm~dom+(cohort|mouse), data =cpep,family = Gamma(link = "log"))
summary(cpepx4) 

acf(resid(cpepx2))
car::qqPlot(resid(cpepx2))
hist(resid(cpepx2))
plot(cpepx2)

car::durbinWatsonTest(resid(cpepx2))
shapiro.test(resid(cpepx2))#normal


#il6 not sign 
hist(il6$value)
hist(il6$cm)

il6x<-glmer(value~dom+(1|mouse)+(1|cohort), data =il6,family = Gamma(link = "log"))
summary(il6x) 

il6x2<-glmer(cm~dom+(1|mouse)+(1|cohort), data =il6,family = Gamma(link = "log"))
summary(il6x2) 

il6x3<-glmer(cm~dom+lightsoff+(1|mouse)+(1|cohort), data =il6,family = Gamma(link = "log"))
summary(il6x3) 

il6x4<-glmer(cm~dom++lightsoff+(cohort|mouse), data =il6,family = Gamma(link = "log"))
summary(il6x4) 

acf(resid(il6x2))
car::qqPlot(resid(il6x2))
hist(resid(il6x2))
plot(il6x2)

car::durbinWatsonTest(resid(il6x2))
shapiro.test(resid(il6x2))#normal


#lep #none ssign 
hist(lep$value)
hist(lep$cm)

lepx<-glmer(value~dom+(1|mouse)+(1|cohort), data =lep,family = Gamma(link = "log"))
summary(lepx) 

lepx2<-glmer(cm~dom+(1|mouse)+(1|cohort), data =lep,family = Gamma(link = "log"))
summary(lepx2) 

lepx3<-glmer(cm~dom+lightsoff+(1|mouse)+(1|cohort), data =lep,family = Gamma(link = "log"))
summary(lepx3) 

lepx4<-glmer(cm~dom+(1|lightsoff)+(cohort|mouse), data =lep,family = Gamma(link = "log"))
summary(lepx4) 
# lightsoff    0.46673    0.17454   2.674  0.00749 **
# 
acf(resid(lepx2))
car::qqPlot(resid(lepx2))
hist(resid(lepx2))
plot(lepx2)

car::durbinWatsonTest(resid(lepx2))
shapiro.test(resid(lepx2))#normal




#pyy #none ssign 
hist(pyy$value)
hist(pyy$cm)

pyyx<-glmer(value~dom+(1|mouse)+(1|cohort), data =pyy,family = Gamma(link = "log"))
summary(pyyx) 

pyyx2<-glmer(cm~dom+(1|mouse)+(1|cohort), data =pyy,family = Gamma(link = "log"))
summary(pyyx2) 

pyyx2<-glmer(cm~dom+lightsoff +(1|mouse)+(1|cohort), data =pyy,family = Gamma(link = "log"))
summary(pyyx2) 


pyyx4<-glmer(cm~dom+lightsoff +(cohort|mouse), data =pyy,family = Gamma(link = "log"))
summary(pyyx4) 
# lightsoff    0.076263   0.005015  15.206  < 2e-16 ***

acf(resid(pyyx2))
car::qqPlot(resid(pyyx2))
hist(resid(pyyx2))
plot(pyyx2)

car::durbinWatsonTest(resid(pyyx2))
shapiro.test(resid(pyyx2))#normal



#ghr #none ssign 
hist(ghr$value)
hist(ghr$cm)

ghrx<-glmer(value~dom+(1|mouse)+(1|cohort), data =ghr,family = Gamma(link = "log"))
summary(ghrx) 

ghrx2<-glmer(cm~dom+(1|mouse)+(1|cohort), data =ghr,family = Gamma(link = "log"))
summary(ghrx2) 

ghrx3<-glmer(cm~dom+lightsoff+(1|mouse)+(1|cohort), data =ghr,family = Gamma(link = "log"))
summary(ghrx3) 

ghrx4<-glmer(cm~dom+lightsoff+(cohort|mouse), data =ghr,family = Gamma(link = "log"))
summary(ghrx4) 


acf(resid(ghrx2))
car::qqPlot(resid(ghrx2))
hist(resid(ghrx2))
plot(ghrx2)

car::durbinWatsonTest(resid(ghrx2))
shapiro.test(resid(ghrx2))#normal


#MC #none ssign 
hist(MC$value)
hist(MC$cm)

MCx<-glmer(value~dom+(1|mouse)+(1|cohort), data =MC,family = Gamma(link = "log"))
summary(MCx) 

MCx2<-glmer(cm~dom+(1|mouse)+(1|cohort), data =MC,family = Gamma(link = "log"))
summary(MCx2) 

MCx3<-glmer(cm~dom+lightsoff+(1|mouse)+(1|cohort), data =MC,family = Gamma(link = "log"))
summary(MCx3) 

MCx3<-glmer(cm~dom+lightsoff+(cohort|mouse), data =MC,family = Gamma(link = "log"))
summary(MCx3) 

# lightsoff     0.4849     0.2507   1.934   0.0531 .

acf(resid(MCx2))
car::qqPlot(resid(MCx2))
hist(resid(MCx2))
plot(MCx2)

car::durbinWatsonTest(resid(MCx2))
shapiro.test(resid(MCx2))#normal



#amy #none ssign 
hist(amy$value)
hist(amy$cm)

amyx<-glmer(value~dom+(1|mouse)+(1|cohort), data =amy,family = Gamma(link = "log"))
summary(amyx) 

amyx2<-glmer(cm~dom+(1|mouse)+(1|cohort), data =amy, family = Gamma(link = "log"))
summary(amyx2) 

acf(resid(amyx2))
car::qqPlot(resid(amyx2))
hist(resid(amyx2))
plot(amyx2)

car::durbinWatsonTest(resid(amyx2))
shapiro.test(resid(amyx2))#normal

#tnf # 0.0687 . 
hist(tnf$value)
hist(tnf$cm)

tnfx<-glmer(value~dom+(1|mouse)+(1|cohort), data =tnf,family = Gamma(link = "log"))
summary(tnfx) 

tnfx2<-glmer(cm~dom+(1|mouse)+(1|cohort), data =tnf, family = Gamma(link = "log"))
summary(tnfx2) 

tnfx3<-glmer(cm~dom+lightsoff+(1|mouse)+(1|cohort), data =tnf, family = Gamma(link = "log"))
summary(tnfx3) 

tnfx4<-glmer(cm~dom+(1|lightsoff)+(1|mouse)+(1|cohort), data =tnf, family = Gamma(link = "log"))
summary(tnfx4)
# domSUB        0.2120     0.1259   1.684 0.092168 . 

acf(resid(tnfx2))
car::qqPlot(resid(tnfx2))
hist(resid(tnfx2))
plot(tnfx2)

car::durbinWatsonTest(resid(tnfx2))
shapiro.test(resid(tnfx2))#normal


#sec # non sign
hist(sec$value)
hist(sec$cm)

secx<-glmer(value~dom+(1|mouse)+(1|cohort), data =sec,family = Gamma(link = "log"))
summary(secx) 

secx2<-glmer(cm~dom+(1|mouse)+(1|cohort), data =sec, family = Gamma(link = "log"))
summary(secx2) 

secx3<-glmer(cm~dom+lightsoff+(1|mouse)+(1|cohort), data =sec, family = Gamma(link = "log"))
summary(secx3) 


secx3<-glmer(cm~dom+lightsoff+(cohort|mouse), data =sec, family = Gamma(link = "log"))
summary(secx3) 

secx4<-glmer(cm~dom+(1|lightsoff)+(1|mouse)+(1|cohort), data =sec, family = Gamma(link = "log"))
summary(secx4) 

acf(resid(secx2))
car::qqPlot(resid(secx2))
hist(resid(secx2))
plot(secx2)

car::durbinWatsonTest(resid(secx2))
shapiro.test(resid(secx2))#normal





lep$dom2 <- ifelse(lep$dom == "DOM", 1, 0)
cor.test(lep$weight_GH10, lep$dom2)

lw <- lmer(value ~dom*weight_GH10 + (1|lightsoff) +(1|mouse) +(1|cohort), data = lep)
summary(lw)

lw <- lm(value ~dom*weight_GH10 , data = lep)
summary(lw)


ggplot(lep, aes(weight_GH10, value, color = dom)) +
  geom_point(size = 2)+
  stat_smooth(method = "lm", se= F, aes(color = dom))+
  scale_color_viridis(discrete = TRUE)+
  ylab("Leptin concentration pg/ml")+
  xlab("Final weight (g)")+
  theme_minimal()+
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
        strip.text.x = element_text(size = 15))


ggplot(lep, aes(weight_AT, value, color = dom)) +
  geom_point()+
  stat_smooth(method = "lm", se= F, aes(color = dom))+
  theme_minimal()+
  scale_color_viridis(discrete = TRUE)+
  ylab("Leptin concentration pg/ml")+
  xlab("eWAT (g)")+
  theme_minimal()+
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
        strip.text.x = element_text(size = 15))


lw <- lmer(value ~dom +weight_AT + (1|lightsoff) +(1|mouse) +(1|cohort), data = lep)
summary(lw)


id <- read_csv("RFID_stable_cohorts/data_raw/id_data.csv")
head(id)

rank <- read_csv("RFID_stable_cohorts/data_clean/socialbehavior/rank_data.csv")
head(rank)


rank$dom <- ifelse(rank$glicko_rank == 1, "Dominant", rank$glicko_rank)
rank$dom <- ifelse(rank$glicko_rank == 2, "Subdominant", rank$dom)
rank$dom <- ifelse(rank$glicko_rank == 6, "Subordinate", rank$dom)

colnames(rank)[3]<- "mouse"

id$mouse <- as.character(id$mouse)


all <- id %>% full_join(rank)
head(all)
colnames(all)

all <- all[,c(1,3,6,8,9,11,16,17)]
all

dstat <- all %>% filter(dom != "3") %>% filter(dom != "4") %>% filter(dom !="5")
head(dstat)

dstat$AT_corr <- ((dstat$weight_AT/dstat$weight_GD1))
dstat$growth <- (((dstat$weight_GD1 - dstat$weight_PD1)/dstat$weight_PD1)*100)



dx <- dstat %>% select(1:3,9,10)

l <- lep %>% full_join(dx) %>% na.omit(.)



ggplot(l, aes(growth, value, color = dom)) +
  geom_point(size = 2)+
  stat_smooth(method = "lm", se= F, aes(color = dom))+
  scale_color_viridis(discrete = TRUE)+
  ylab("Leptin concentration pg/ml")+
  xlab("Precent Growth")+
  theme_minimal()+
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
        strip.text.x = element_text(size = 15))


ggplot(l, aes(AT_corr, value, color = dom)) +
  geom_point()+
  stat_smooth(method = "lm", se= F, aes(color = dom))+
  theme_minimal()+
  scale_color_viridis(discrete = TRUE)+
  ylab("Leptin concentration pg/ml")+
  xlab("eWAT(g)/Final weight (g)")+
  theme_minimal()+
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
        strip.text.x = element_text(size = 15))



lx <- lm(value ~dom*growth, data = l)
summary (lx)


lx <- lmer(value~growth +lightsoff+(1|cohort), data = l)
summary (lx)



ggplot(l, aes(weight_GH10, weight_AT, color = dom)) +
  geom_point()+
  stat_smooth(method = "lm", se= F, aes(color = dom))+
  theme_minimal()+
  scale_color_viridis(discrete = TRUE)+
  ylab("eWAT (g)")+
  xlab("Final weight (g)")+
  theme_minimal()+
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
        strip.text.x = element_text(size = 15))
