library(tidyverse)

rank <- read_csv("RFID_stable_cohorts/data_clean/socialbehavior/rank_data.csv")
head(rank)


arc <- read_csv("RFID_stable_cohorts/data_raw/RNAscope/ARC.csv", locale=locale(encoding="latin1"))
head(arc)
colnames(arc)
#count inside col 10:21
#count touch  col 22: 33
arc$mouse <- arc$id

arcx <- arc %>% pivot_longer(., cols = 10:21, names_to = 'target') %>% 
  mutate(prop_inside = value/Accepted) 

inside <- arcx%>% select(cohort, mouse, section, Accepted, target, value, prop_inside)

all <- rank %>% full_join(inside) %>% na.omit(.)




ggplot(all, aes(as.factor(ds_rank), prop_inside)) +
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(~target, scales = "free")+ 
  theme_minimal()



arcxx <- arc %>% pivot_longer(., cols = 23:34, names_to = 'target') %>% 
  mutate(prop_touching = value/Accepted) 

outside <- arcxx%>% select(cohort, mouse, section, Accepted, target, area, value, prop_touching)

all2 <- rank %>% full_join(outside) %>% na.omit(.)

colnames(all2)

all2x <- all2 %>% mutate(dom_group = as.factor(ifelse(.$ds_rank == 1, "DOM", "SUB")))
all2x$target <- gsub(" Count Touching", "", all2x$target)

all2x$area <- gsub("E", "A", all2x$area)


ggplot(all2, aes(as.factor(ds_rank), prop_touching)) +
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(~target, scales = "free")+ 
  theme_minimal()

ggplot(all2x, aes(dom_group, prop_touching)) +
  geom_boxplot(outlier.shape = NA)+
  geom_jitter()+
  facet_wrap(~target, scales = "free_y")+ 
  theme_minimal()


ggplot(all2x, aes(dom_group, prop_touching, color = interaction(area)))+
geom_boxplot(outlier.shape=NA)+ 
  geom_point(size = 2, alpha = 0.6, position = position_jitterdodge())+
  facet_wrap(~target, scales = "free_y")+ 
  theme_minimal()


all2x <- all2x %>% filter(area == "M")




library(viridis)

ggplot(all2x, aes(dom_group, prop_touching, color = dom_group))+
  geom_boxjitter(aes(fill = dom_group), outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  facet_wrap(~target, scales = "free")+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  theme_classic()+
  theme(legend.position = "none", text = element_text(size=20)) +
  scale_y_continuous(expand = expansion(.3)) 




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
hist(crh$value)

cm <-lmer(value~dom_group+(1|mouse)+(1|cohort), data =crh)
summary(cm)

 
#TRH
trh <- al$TRH
hist(trh$value)

tm <-lmer(value~dom_group+(mouse|cohort), data =trh)
summary(tm)


#ghrh
ghh <- al$GHRH
hist(ghh$value)

gm <-lmer(value~dom_group+section+(1|cohort), data =ghh)
summary(gm)



#ghrl
gh <- al$Ghrl
hist(gh$value)
gm2 <-lmer(value~dom_group+section+(1|cohort), data =gh)
summary(gm2)




#kiss #sign 
kiss<- al$Kiss1
hist(kiss$value)

km <-lmer(value~dom_group+ section+(1|cohort), data =kiss)
summary(km)


#agrp  ##### sign 
ag <- al$AgRP
hist(ag$value)

agm <-lmer(value~dom_group+(1|cohort), data =ag)
summary(agm)



#npy
npy <- al$NPY
hist(npy$value)

nm <-lmer(value~dom_group+(1|cohort), data =npy)
summary(nm)



#pomc # sig
pomc <- al$POMC
hist(pomc$value)

pm <-lmer(value~dom_group+(1|cohort), data =pomc)
summary(pm)



#lep
lep <- al$LepRb
hist(lep$value)
lm <-lmer(value~dom_group+(1|cohort), data =lep)
summary(lm)


#ins
ins <- al$INR
hist(ins$value)

inm <-lmer(value~dom_group+(1|cohort), data =ins)
summary(inm)


#GHSR1a
ghr <- al$GHSR1a
hist(ghr$value)

ghrm <-lmer(value~dom_group+(1|cohort), data =ghr)
summary(ghrm)


#MC4R
mc <- al$MC4R
hist(mc$value)

mcm <-lmer(value~dom_group+(1|cohort), data =mc)
summary(mcm)
