library(tidyverse)


source('RFID_stable_cohorts/data_carpentry/physiology_data/metabolic_results.R')
head(mdf)

colnames(mdf)
m.long <-mdf %>% pivot_longer(cols = 2:11, names_to="analyte") 
m.long$ID <- substr(m.long$ID, 5,9)
m.long$ID <- gsub("_", "", m.long$ID)

head(m.long)


df <- read_csv("RFID_stable_cohorts/data_raw/id_data.csv")
head(df) 

df1 <- df %>% dplyr::select(1,3,11, 13)

mat <- m.long %>% full_join(df1) %>% dplyr::select(1:4, 7:12)

lat <- mat %>% filter(analyte == 'Leptin') %>% filter(time == 'Post')

lat_p <- ggplot(lat, aes(weight_AT, value, color = as.factor(glicko_rank), fill = as.factor(glicko_rank)))+
  geom_point(size = 3, shape = 21, alpha = 0.6)+
  geom_smooth(method = "lm", alpha = 0.2, size = 1.2,se =F)+
  scale_color_manual(name="GH Rank",values = viridis::viridis(2)) +
  scale_fill_manual(name="GH Rank",values = viridis::viridis(2))+
  labs(y = "GH Plasma Leptin Concentration (pg/ml)",
       x = "eWAT Weight (g)") +
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

ggsave("RFID_stable_cohorts/imgs/l_WAT.png",lat_p, height = 6, width  = 6, dpi = 300)

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
        axis.text= element_text(color="#3C3C3C", size=20),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 20)
  )

m.long$time <- factor(m.long$time, levels = c("Pre", "Post"))
m.long$time  <- ifelse(m.long$time == "Pre", "PH", 'GH')
m.long$time <- factor(m.long$time, levels = c("PH", "GH"))

a.list <- m.long %>% split(.$analyte)
lapply(a.list,head)


df <- a.list[[1]]

a.list <- a.list %>% map(~mutate(.,y_max = max(value + 10)))


prepost <- function (df){
  # source('RFID_stable_cohorts/data_carpentry/figures')
  # library(ggplot2)

p1 <- ggplot(df, aes(x=dom , y = value, color =interaction(dom,time), fill = interaction(dom,time))) +
  geom_boxjitter(outlier.color = NA, jitter.shape = 21,jitter.size = 2,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  
  # geom_line(aes(group = cohort),position = position_dodge( 0.0), size = .6, color = "black") +
  # geom_point(aes(group = ID),position = position_dodge(  0.0), size = 3, alpha =0.4) +
  facet_wrap(~time)+
  xlab("")+
  ylab("Concentration (pg/ml)")+
  scale_color_manual(values = c("#BD1100","#DD5F20", "#E15037", "#FD8E3F"))+
  scale_fill_manual(values = c( "#BD1100", "#DD5F20", "#E15037", "#FD8E3F"))+
  expand_limits(y = c(NA, max(df$value)+50))+
  theme(axis.text.x = element_text(vjust = 1, size = 15),
        axis.text.y = element_text(hjust = 0.5, size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.title = element_text(size = 15),
        axis.text= element_text(color="#3C3C3C", size=15),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 15),
        legend.position = "none")

return(p1)
}


lapply(a.list, prepost)

labels = c("Amylin", "C-Peptide2", "Ghrelin", "IL6", "Insulin", "Leptin","MCP1/CCL2", "PYY", "Secretin", "TNF-alpha")

plots <- lapply(a.list, prepost)

met2 <- egg::ggarrange(plots=plots, labels = labels, nrow = 2,  ncol = 5, 
               label.args = list(gp = grid::gpar(font = 1, cex =1.5)))



p1 <- plots[[1]]+ggtitle("Amylin")+ theme(plot.title = element_text(hjust = 0.5, size = 20)) 
p2 <- plots[[2]]+ggtitle("C-Peptide2")+  theme(plot.title = element_text(hjust = 0.5, size = 20)) 
p3 <- plots[[3]]+ggtitle("Ghrelin")+  theme(plot.title = element_text(hjust = 0.5, size = 20)) 
p4 <- plots[[4]]+ggtitle("IL6")+  theme(plot.title = element_text(hjust = 0.5, size = 20)) 
p5 <- plots[[5]]+ggtitle("Insulin")+  theme(plot.title = element_text(hjust = 0.5, size = 20)) 
p6 <- plots[[6]]+ggtitle("Leptin")+  theme(plot.title = element_text(hjust = 0.5, size = 20)) 
p7 <- plots[[7]]+ggtitle("MCP1/CCL2")+  theme(plot.title = element_text(hjust = 0.5, size = 20)) 
p8 <- plots[[8]]+ggtitle("PYY")+  theme(plot.title = element_text(hjust = 0.5, size = 20)) 
p9 <- plots[[9]]+ggtitle("Secretin")+  theme(plot.title = element_text(hjust = 0.5, size = 20)) 
p10 <- plots[[10]]+ggtitle("TNF-alpha")+ theme(plot.title = element_text(hjust = 0.5, size = 20)) 

met <- gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,ncol=5)

print(met)
ggsave("RFID_stable_cohorts/imgs/metabolic_prepost.png", met, height = 10, width= 30, dpi = 600)


###statistics 
library(lmerTest)



ins <- a.list$Insulin
cpep <- a.list$Cpeptide2
il6 <-  a.list$IL6
lep <- a.list$Leptin
pyy <- a.list$PYY
ghr <- a.list$Ghrelin
MC <- a.list$MCP1_CCL2
amy <- a.list$Amylin
tnf <-a.list$TNFa
sec <- a.list$Secretin


cin <- ins %>% rbind(cpep)
head(cin)

ccin <- cin %>% dplyr::select(-y_max) %>% pivot_wider(names_from = analyte, values_from = value) %>% unique()



cinp <- ggplot(ccin, aes(Cpeptide2, Insulin, color = as.factor(glicko_rank), fill = as.factor(glicko_rank)))+
  geom_point(size = 3, shape = 21, alpha = 0.6)+
  geom_smooth(method = "lm", alpha = 0.2, size = 1.2,se =F)+
  scale_color_manual(name="GH Rank",values = viridis::viridis(2)) +
  scale_fill_manual(name="GH Rank",values = viridis::viridis(2))+
  labs(y = "GH Plasma Insulin Concentration (pg/ml)",
       x = "GH Plasma C-Peptide2 Concentration (pg/ml)") +
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

ggsave("RFID_stable_cohorts/imgs/Ins_Cpep.png",cinp, height = 6, width  = 6, dpi = 300)
#ins
hist(ins$value)
insx<-glmer(value~dom+time+(1|mouse)+(1|cohort), data =ins,family = Gamma(link = "log"))
summary(insx) 
acf(resid(insx))
qqPlot(resid(insx))
plot(insx)
shapiro.test(resid(insx))#normal

#post 
ins_post <- ins %>% filter(time == "Post")
hist(ins_post$value)
ins_post<-glmer(value~dom+(1|mouse)+(1|cohort), data =ins_post ,family = Gamma(link = "log"))
summary(ins_post) 
acf(resid(ins_post))
qqPlot(resid(ins_post))
plot(ins_post)
shapiro.test(resid(ins_post))#normal

#pre 
ins_pre <- ins %>% filter(time == "Pre")
hist(ins_p$value)
ins_p<-glmer(value~dom+(1|mouse)+(1|cohort), data =ins_pre ,family = Gamma(link = "log"))
summary(ins_p) 
acf(resid(ins_p))
qqPlot(resid(ins_p))
plot(ins_p)
shapiro.test(resid(ins_p))#normal



#cpep
hist(cpep$value)
cpepx<-glmer(value~dom+time+(1|mouse)+(1|cohort), data =cpep, family = Gamma(link = "log"))
summary(cpepx) 
acf(resid(cpepx))
qqPlot(resid(cpepx))
plot(cpepx)
shapiro.test(resid(cpepx))#normal

#post 
cpost <- cpep %>% filter(time == "Post")
hist(cpost$value)
c_post<-glmer(value~dom+(1|mouse)+(1|cohort), data =cpost ,family = Gamma(link = "log"))
summary(c_post) 
acf(resid(c_post))
qqPlot(resid(c_post))
plot(c_post)
shapiro.test(resid(c_post))#normal


#pre 
c_pre <- cpep %>% filter(time == "Pre")
hist(c_pre$value)
c_p<-glmer(value~dom+(1|mouse)+(1|cohort), data =c_pre ,family = Gamma(link = "log"))
summary(c_p) 
acf(resid(c_p))
qqPlot(resid(c_p))
plot(c_p)#scary 
shapiro.test(resid(c_p))


#il6
hist(il6$value)
il6x<-glmer(value~dom+time+(1|mouse)+(1|cohort), data =il6, family = Gamma(link = "log"))
summary(il6x) 
acf(resid(il6x))
qqPlot(resid(il6x))
plot(il6x)
shapiro.test(resid(il6x))#normal

#post 
ipost <- il6 %>% filter(time == "Post")
hist(cpost$value)
i_post<-glmer(value~dom+(1|mouse)+(1|cohort), data =ipost ,family = Gamma(link = "log"))
summary(i_post) 
acf(resid(i_post))
qqPlot(resid(i_post))
plot(i_post)
shapiro.test(resid(i_post))#normal


#pre 
i_pre <- il6 %>% filter(time == "Pre")
hist(i_pre$value)
i_p<-glmer(value~dom+(1|mouse)+(1|cohort), data =i_pre ,family = Gamma(link = "log"))
summary(i_p) 
acf(resid(i_p))
qqPlot(resid(i_p))
plot(i_p) 
shapiro.test(resid(i_p))


#lep
hist(lep$value)
lepx<-glmer(value~dom+time+(1|mouse)+(1|cohort), data =lep, family = Gamma(link = "log"))
summary(lepx) 
acf(resid(lepx))
qqPlot(resid(lepx))
plot(lepx)
shapiro.test(resid(lepx))#normal

#post 
lpost <- lep %>% filter(time == "Post")
hist(lpost$value)
l_post<-glmer(value~dom+(1|mouse)+(1|cohort), data =lpost ,family = Gamma(link = "log"))
summary(l_post) 
acf(resid(l_post))
qqPlot(resid(l_post))
plot(l_post)
shapiro.test(resid(l_post))#normal


#pre 
l_pre <- lep %>% filter(time == "Pre")
hist(l_pre$value)
l_p<-glmer(value~dom+(1|mouse)+(1|cohort), data =l_pre ,family = Gamma(link = "log"))
summary(l_p) 
acf(resid(l_p))
qqPlot(resid(l_p))
plot(l_p) 
shapiro.test(resid(l_p))

#pyy
hist(pyy$value)

pyyx<-glmer(value~dom+time+(1|mouse)+(1|cohort), data =pyy, family = Gamma(link = "log"))
summary(pyyx) 
acf(resid(pyyx))
qqPlot(resid(pyyx))
plot(pyyx)
shapiro.test(resid(pyyx))#normal

#post 
ppost <- pyy %>% filter(time == "Post")
hist(ppost$value)
p_post<-glmer(value~dom+(1|mouse)+(1|cohort), data =ppost ,family = Gamma(link = "log")) # THIS IS NOT WORKING 
summary(p_post) 
acf(resid(p_post))
qqPlot(resid(p_post))
plot(p_post)
shapiro.test(resid(p_post))#normal


#pre 
p_pre <- pyy %>% filter(time == "Pre")
hist(p_pre$value)
p_p<-glmer(value~dom+(1|mouse)+(1|cohort), data =p_pre ,family = Gamma(link = "log"))
summary(p_p) 
acf(resid(p_p))
qqPlot(resid(p_p))
plot(p_p) 
shapiro.test(resid(p_p))



#MC
hist(MC$value)
MCx<-glmer(value~dom+time+(1|mouse)+(1|cohort), data =ghr, family = Gamma(link = "log"))
summary(MCx) 
acf(resid(MCx))
qqPlot(resid(MCx))
plot(MCx)
shapiro.test(resid(MCx))#normal

#post 
MCpost <- MC %>% filter(time == "Post")
hist(MCpost$value)
MC_post<-glmer(value~dom+(1|mouse)+(1|cohort), data =MCpost ,family = Gamma(link = "log")) 
summary(MC_post) 
acf(resid(MC_post))
qqPlot(resid(MC_post))
plot(MC_post)
shapiro.test(resid(MC_post))#normal


#pre 
MC_pre <- MC %>% filter(time == "Pre")
hist(MC_pre$value)
MC_p<-glmer(value~dom+(1|mouse)+(1|cohort), data =MC_pre ,family = Gamma(link = "log"))
summary(MC_p) 
acf(resid(MC_p))
qqPlot(resid(MC_p))
plot(MC_p) 
shapiro.test(resid(MC_p))


#ghr
hist(ghr$value)
ghrx<-glmer(value~dom+time+(1|mouse)+(1|cohort), data =ghr, family = Gamma(link = "log"))
summary(ghrx) 
acf(resid(ghrx))
qqPlot(resid(ghrx))
plot(ghrx)
shapiro.test(resid(ghrx))#normal

#post 
gpost <- ghr %>% filter(time == "Post")
hist(gpost$value)
g_post<-glmer(value~dom+(1|mouse)+(1|cohort), data =gpost ,family = Gamma(link = "log")) 
summary(g_post) 
acf(resid(g_post))
qqPlot(resid(g_post))
plot(g_post)
shapiro.test(resid(g_post))#normal


#pre 
g_pre <- ghr %>% filter(time == "Pre")
hist(g_pre$value)
g_p<-glmer(value~dom+(1|mouse)+(1|cohort), data =g_pre ,family = Gamma(link = "log"))
summary(g_p) 
acf(resid(g_p))
qqPlot(resid(g_p))
plot(g_p) 
shapiro.test(resid(g_p))

#amy
hist(amy$value)
amyx<-glmer(value~dom+time+(1|mouse)+(1|cohort), data =amy, family = Gamma(link = "log"))
summary(amyx) 
acf(resid(amyx))
qqPlot(resid(amyx))
plot(amyx)
shapiro.test(resid(amyx))#normal

#post 
amypost <- amy %>% filter(time == "Post")
hist(amypost$value)
amy_post<-glmer(value~dom+(1|mouse)+(1|cohort), data =amypost ,family = Gamma(link = "log")) 
summary(amy_post) 
acf(resid(amy_post))
qqPlot(resid(amy_post))
plot(amy_post)
shapiro.test(resid(amy_post))#normal


#pre 
amy_pre <- amy %>% filter(time == "Pre")
hist(amy_pre$value)
amy_p<-glmer(value~dom+(1|mouse)+(1|cohort), data =amy_pre ,family = Gamma(link = "log"))
summary(amy_p) 
acf(resid(amy_p))
qqPlot(resid(amy_p))
plot(amy_p) 
shapiro.test(resid(amy_p))

#tfn
hist(tnf$value)
tnfx<-glmer(value~dom+time+(1|mouse)+(1|cohort), data =tnf, family = Gamma(link = "log"))
summary(tnfx) 
acf(resid(tnfx))
qqPlot(resid(tnfx))
plot(tnfx)
shapiro.test(resid(tnfx))#normal

#post 
tnfpost <- tnf %>% filter(time == "Post")
hist(tnfpost$value)
tnf_post<-glmer(value~dom+(1|mouse)+(1|cohort), data =tnfpost ,family = Gamma(link = "log")) 
summary(tnf_post) 
acf(resid(tnf_post))
qqPlot(resid(tnf_post))
plot(tnf_post)
shapiro.test(resid(tnf_post))#normal


#pre 
tnf_pre <- tnf %>% filter(time == "Pre")
hist(tnf_pre$value)
tnf_p<-glmer(value~dom+(1|mouse)+(1|cohort), data =tnf_pre ,family = Gamma(link = "log"))
summary(tnf_p) 
acf(resid(tnf_p))
qqPlot(resid(tnf_p))
plot(tnf_p) 
shapiro.test(resid(tnf_p))

#sec
hist(sec$value)
secx<-glmer(value~dom+time+(1|mouse)+(1|cohort), data =sec, family = Gamma(link = "log"))
summary(secx) 
acf(resid(secx))
qqPlot(resid(secx))
plot(secx)
shapiro.test(resid(secx))#normal

#post 
secpost <- sec%>% filter(time == "Post")
hist(secpost$value)
sec_post<-glmer(value~dom+(1|mouse)+(1|cohort), data =secpost ,family = Gamma(link = "log")) 
summary(sec_post) 
acf(resid(sec_post))
qqPlot(resid(tnf_post))
plot(tnf_post)
shapiro.test(resid(sec_post))#normal


#pre 
sec_pre <- sec %>% filter(time == "Pre")
hist(sec_pre$value)
sec_p<-glmer(value~dom+(1|mouse)+(1|cohort), data =sec_pre ,family = Gamma(link = "log"))
summary(sec_p) 
acf(resid(sec_p))
qqPlot(resid(sec_p))
plot(sec_p) 
shapiro.test(resid(sec_p))

#############
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



head(m)

m$BDNF <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m$BDNF))
m$GH <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m$GH))
m$Prolactin <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m$Prolactin))
m$TSH <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m$TSH))
m$LH <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m$LH))


df <- m %>% left_join(rank)
head(df)

ed <- df %>% dplyr::select(1:11,15) %>% filter(time  == 'Post')

ed.long <-  ed %>% pivot_longer(cols = 2:8, names_to="analyte") 
ed.long$ID <- substr(ed.long$ID, 5,9)
ed.long$ID <- gsub("_", "", ed.long$ID)


ed.list <- ed.long %>% split(.$analyte)

grubbs.flag(ed.list$TSH$value)

raw <- ggplot(ed.long, aes(x=dom , y = value, color =dom, fill = dom)) +
  geom_boxjitter(outlier.color = NA, jitter.shape = NA,
                 alpha = 0.4,
                 jitter.height = 0, jitter.width = 0, errorbar.draw = TRUE,
                 position = position_dodge(0.5))+
  
  geom_line(aes(group = cohort),position = position_dodge( 0.0), size = .6, color = "black") +
  geom_point(aes(group = ID),position = position_dodge(  0.0), size = 3, alpha =0.4) +
  facet_wrap(~analyte, scales = "free_y")+
  xlab("")+
  ylab("Concentration (pg/ml)")+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  theme(axis.text.x = element_text(vjust = 1, size = 15),
        axis.text.y = element_text(hjust = 0.5, size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.title = element_text(size = 15),
        axis.text= element_text(color="#3C3C3C", size=15),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 15),
        legend.position = "none")

ggsave("RFID_stable_cohorts/imgs/raw_endocrine.png", raw, height = 10, width = 30)



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
# 
  m1 <- m1 %>% filter(BDNF <20) %>% filter(LH <350) %>% filter(Prolactin < 10000)


df <- m1 %>% left_join(rank)
head(df)

ed <- df %>% dplyr::select(1:11,15) %>% filter(time  == 'Post')

ed.long <-  ed %>% pivot_longer(cols = 2:8, names_to="analyte") 
ed.long$ID <- substr(ed.long$ID, 5,9)
ed.long$ID <- gsub("_", "", ed.long$ID)

ed.long$time <- ifelse(ed.long$time == "Post", "GH", "PH")
ed.long$time<- factor (ed.long$time, levels = c("PH", "GH"))

ed.list <- ed.long %>% split(.$analyte)

ins


endo <- function (df){
filtered <- ggplot(df, aes(x=dom , y = value, color =dom, fill = dom)) +
  geom_boxjitter(outlier.color = NA, jitter.shape = 21,jitter.size = 2,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85))+
  # geom_line(aes(group = cohort),position = position_dodge( 0.0), size = .6, color = "black") +
  # geom_point(aes(group = ID),position = position_dodge( 0.03), size = 3, alpha =0.4) +
  facet_wrap(~analyte, scales = "free_y")+
  xlab("")+
  ylab("Concentration (pg/ml)")+
  scale_color_manual(values = c("#BD1100","#000004", '#ed6925'))+
  scale_fill_manual(values = c("#BD1100","#000004", '#ed6925'))+
  expand_limits(y = c(NA, max(df$value)+15))+
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
        strip.text.x = element_text(size = 20),
        legend.position = "none")
return(filtered)

}

plots <- lapply(ed.list,endo)

met2 <- egg::ggarrange(plots=plots, nrow = 1,  ncol = 7)

ggsave("RFID_stable_cohorts/imgs/_filtered_major_endocrine_withoutlines.png", met2, height = 5, width = 35)




df <- read_csv("RFID_stable_cohorts/data_raw/id_data.csv")
head(df) 

df1 <- df %>% dplyr::select(1,3,11, 13)



#stats
library(lmerTest)

#ACTH
a <- ed.list$ACTH
a2 <- a %>% mutate(dom2 = factor(dom, levels = c("Subdominant", "Subordinate", "Dominant")))

hist(a$value)
ax<-lmer(value~dom+(1|mouse)+(1|cohort), data =a)
summary(ax) 
acf(resid(ax))
qqPlot(resid(ax))
plot(ax)
shapiro.test(resid(ax))#normal


ax2<-lmer(value~dom2+(1|mouse)+(1|cohort), data =a2)
summary(ax2) 
acf(resid(ax2))
qqPlot(resid(ax2))
plot(ax2)
shapiro.test(resid(ax2))#normal

#bdnf
b <- ed.list$BDNF
b2 <- b %>% mutate(dom2 = factor(dom, levels = c("Subdominant", "Subordinate", "Dominant")))
hist(b$value)
bx<-glmer(value~dom+(1|mouse)+(1|cohort), data =b, family = Gamma(link = "log"))
summary(bx) 
acf(resid(bx))
qqPlot(resid(bx))
plot(bx)
shapiro.test(resid(bx))#normal



bx2<-glmer(value~dom2+(1|mouse)+(1|cohort), data =b2, family = Gamma(link = "log"))
summary(bx2) 
acf(resid(bx2))
qqPlot(resid(bx2))
plot(bx2)
shapiro.test(resid(bx2))#normal


#FSH
f <- ed.list$FSH
f2 <- f %>% mutate(dom2 = factor(dom, levels = c("Subdominant", "Subordinate", "Dominant")))
hist(f$value)
fx<-glmer(value~dom+(1|mouse)+(1|cohort), data =f, family = Gamma(link = "log"))
summary(fx) 
acf(resid(fx))
qqPlot(resid(fx))
plot(fx)
shapiro.test(resid(fx))#normal



fx2<-glmer(value~dom2+(1|mouse)+(1|cohort), data =f2, family = Gamma(link = "log"))
summary(fx2) 
acf(resid(fx2))
qqPlot(resid(fx2))
plot(fx2)
shapiro.test(resid(fx2))#normal

#GH
g <- ed.list$GH

g2 <- g %>% mutate(dom2 = factor(dom, levels = c("Subdominant", "Subordinate", "Dominant")))
hist(g$value)
gx<-glmer(value~dom+(1|mouse)+(1|cohort), data =g, family = Gamma(link = "log"))
summary(gx) 
acf(resid(gx))
qqPlot(resid(gx))
plot(gx)
shapiro.test(resid(gx))#normal



gx2<-glmer(value~dom2+(1|mouse)+(1|cohort), data =g2, family = Gamma(link = "log"))
summary(gx2) 
acf(resid(gx2))
qqPlot(resid(gx2))
plot(gx2)
shapiro.test(resid(gx2))#normal

#lh
l <- ed.list$LH
l2 <- l %>% mutate(dom2 = factor(dom, levels = c("Subdominant", "Subordinate", "Dominant")))
hist(l$value)
lx<-glmer(value~dom+(1|mouse)+(1|cohort), data =l, family = Gamma(link = "log"))
summary(lx) 
acf(resid(lx))
qqPlot(resid(lx))
plot(lx)
shapiro.test(resid(lx))#normal



lx2<-glmer(value~dom2+(1|mouse)+(1|cohort), data =l2, family = Gamma(link = "log"))
summary(lx2) 
acf(resid(lx2))
qqPlot(resid(lx2))
plot(lx2)
shapiro.test(resid(lx2))#normal


#prolactin
p <- ed.list$Prolactin
p2 <- p %>% mutate(dom2 = factor(dom, levels = c("Subdominant", "Subordinate", "Dominant")))
hist(p$value)
px<-glmer(value~dom+(1|mouse)+(1|cohort), data =p, family = Gamma(link = "log"))
summary(px) 
acf(resid(px))
qqPlot(resid(px))
plot(px)
shapiro.test(resid(px))#normal



px2<-glmer(value~dom2+(1|mouse)+(1|cohort), data =p2, family = Gamma(link = "log"))
summary(px2) 
acf(resid(px2))
qqPlot(resid(px2))
plot(px2)
shapiro.test(resid(px2))#normal


#TSH
t <- ed.list$TSH
t2 <- t %>% mutate(dom2 = factor(dom, levels = c("Subdominant", "Subordinate", "Dominant")))
hist(t$value)
tx<-glmer(value~dom+(1|mouse)+(1|cohort), data =t, family = Gamma(link = "log"))
summary(tx) 
acf(resid(tx))
qqPlot(resid(tx))
plot(tx)
shapiro.test(resid(tx))#normal



tx2<-glmer(value~dom2+(1|mouse)+(1|cohort), data =t2, family = Gamma(link = "log"))
summary(tx2) 
acf(resid(tx2))
qqPlot(resid(tx2))
plot(tx2)
shapiro.test(resid(tx2))#normal

