library(tidyverse)

rank <- read_csv("RFID_stable_cohorts/data_clean/socialbehavior/rank_data.csv")
head(rank)


arc <- read_csv("RFID_stable_cohorts/data_raw/RNAscope/ARC_summary.csv", locale=locale(encoding="latin1"))
head(arc)
colnames(arc)
#count inside col 10:21
#count touch  col 22: 33
arc$mouse <- arc$id

arcx <- arc %>% pivot_longer(., cols = 10:21, names_to = 'target') %>% 
  mutate(prop_inside = value/Accepted) 

inside <- arcx%>% select(cohort, mouse, section, Accepted, target, value, prop_inside)

all <- rank %>% full_join(inside) %>% na.omit(.)

ta <- arc%>% full_join(rank) %>% na.omit(.) %>% unique(.)
table(ta$ds_rank, ta$area)


colnames(ta)


arcx <- ta %>% pivot_longer(., cols = 23:34, names_to = 'target') %>% 
  mutate(prop_touching = value/Accepted) 
colnames(arcx)

axx <- arcx %>% mutate(dom_group = as.factor(ifelse(.$ds_rank == 1, "DOM", "SUB")))
axx$area <- gsub("E", "A", axx$area)
axx$target <- gsub(" Count Touching", "", axx$target)


ggplot(axx, aes(area, prop_touching, color = dom_group))+
  geom_boxjitter(aes(fill = dom_group), outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  facet_wrap(~target, scales = "free")+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  ylab("positive/total cells")+
  theme_classic()+
  theme(text = element_text(size=20)) 









ggplot(all, aes(as.factor(ds_rank), prop_inside)) +
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(~target, scales = "free")+ 
  theme_minimal()




arcxx <- arc %>% pivot_longer(., cols = 23:34, names_to = 'target') %>% 
  mutate(prop_touching = value/Accepted) 

outside <- arcxx%>% dplyr::select(cohort, mouse, section, Accepted, target, area, value, prop_touching)

all2 <- rank %>% full_join(outside) %>% na.omit(.)

colnames(all2)



lepr <- all2x %>% filter(target == "LepRb")

str(lepr)
str(lep)

lepr <-  lepr %>% mutate(count = value) %>% select(mouse,cohort,count, prop_touching)

 xx <- lepr %>% full_join(lep) %>% na.omit(.)

 ggplot(xx, aes(prop_touching, value, color = dom)) +
   geom_point()+
   stat_smooth(method = "lm", se= F, aes(color = dom))+
   theme_minimal()+
   scale_color_viridis(discrete = TRUE)+
   xlab("Ghrelin concentration (pg/ml)")+
   ylab("ARC Ghrl postive/total cells")+
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
 
 
 bg <- all2x %>% filter(target == "Ghrl")
 
 
  bg<-  bg %>% mutate(count = value) %>% select(mouse,cohort,count, prop_touching)
 
 xx <- bg %>% full_join(ghr) %>% na.omit(.)
 
 
 
 

all2x <- all2 %>% mutate(dom_group = as.factor(ifelse(.$ds_rank == 1, "DOM", "SUB")))
all2x$target <- gsub(" Count Touching", "", all2x$target)

all2x$area <- gsub("E", "A", all2x$area)


ggplot(all2, aes(as.factor(ds_rank), prop_touching)) +
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(~target, scales = "free")+ 
  theme_minimal()

ggplot(all2, aes(dom_group, prop_touching)) +
  geom_boxplot(outlier.shape = NA)+
  geom_jitter()+
  facet_wrap(~target, scales = "free_y")+ 
  theme_minimal()


ggplot(all2x, aes(dom_group, prop_touching, color = interaction(area)))+
geom_boxplot(outlier.shape=NA)+ 
  geom_point(size = 2, alpha = 0.6, position = position_jitterdodge())+
  facet_wrap(~target, scales = "free_y")+ 
  theme_minimal()


ta <- unique(all2x)

table(ta$dom_group, ta$area)

all2x <- all2x %>% filter(area == "M")




library(viridis)

ap <- ggplot(all2x, aes(dom_group, prop_touching, fill = dom_group))+
  gc

all2$target <- factor(all2$target, levels = c("CRH", "TRH", "GHRH", "Kiss1", "LepRb", "AgRP", "NPY", "Ghrl", "POMC", "INR", "GHSR1a", "MC4R"))

ap
ggsave("RFID_stable_cohorts/imgs/ARC_raw.png", ap,height = 9, width =8 , dpi = 300)




library(lme4)

head(arc)

arclm <- arc[,c(1:3, 22:33)]
arclmx <- arclm %>% full_join(rank)%>% mutate(dom_group = as.factor(ifelse(.$ds_rank == 1, "DOM", "SUB")))
colnames(arclmx)


al <- all2x %>% split(.$target)

# lm_result_list <- list()
# 
# library(lme4)
# library(lmerTest)
# # library(brms)
# # library(tidybayes)
# 
# 
# for(x in 1:length(al)){
#   al[,c(1:11)] -> df
#   md <- names(al)
#    names(al) <- "target"
# 
#    lmer(value~ dom_group +(1|cohort) , data = df) -> mod1
# 
#   summary(mod1)
# 
#   df
#   rbind(summary(lm(df[,12] ~ df[,1]))$coefficients[2,])%>%
#     as.data.frame() %>%
#     cbind(key = c("DOM-SUB")) %>%
#     mutate(target = md) -> lm_result_list[[x]]
# }
# 
# lm_result_list$

table(all2x$target)

#CRH
crh <- al$CRH
hist(crh$prop)

cm <-lmer(prop~dom_group+(1|mouse)+(1|cohort), data =crh)
summary(cm)

 
#TRH
trh <- al$TRH
hist(trh$prop)

tm <-lmer(prop~dom_group+(mouse|cohort), data =trh)
summary(tm)


#ghrh
ghh <- al$GHRH
hist(ghh$prop)

gm <-lmer(prop~dom_group+(1|cohort), data =ghh)
summary(gm)



#ghrl # sign 
gh <- al$Ghrl
hist(gh$prop)
gm2 <-lmer(prop~dom_group+(1|cohort), data =gh)
summary(gm2)




#kiss #sign 
kiss<- al$Kiss1
hist(kiss$prop)

km <-lmer(prop~dom_group+(1|cohort), data =kiss)
summary(km)


#agrp  ##### sign 
ag <- al$AgRP
hist(ag$prop)

agm <-lmer(prop~dom_group+(1|cohort), data =ag)
summary(agm)



#npy
npy <- al$NPY
hist(npy$prop)

nm <-lmer(prop~dom_group+(1|cohort), data =npy)
summary(nm)



#pomc # sig
pomc <- al$POMC
hist(pomc$prop)

pm <-lmer(prop~dom_group+(1|cohort), data =pomc)
summary(pm)



#lep #sign
lep <- al$LepRb
hist(lep$prop)
lm <-lmer(prop~dom_group+(1|cohort), data =lep)
summary(lm)


#ins
ins <- al$INR
hist(ins$prop)

inm <-lmer(prop~dom_group+(1|cohort), data =ins)
summary(inm)


#GHSR1a ## sign
ghr <- al$GHSR1a
hist(ghr$prop)

ghrm <-lmer(prop~dom_group+(1|cohort), data =ghr)
summary(ghrm)


#MC4R
mc <- al$MC4R
hist(mc$prop)

mcm <-lmer(prop~dom_group+(1|cohort), data =mc)
summary(mcm)


