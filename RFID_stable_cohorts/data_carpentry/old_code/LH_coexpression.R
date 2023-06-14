library(tidyverse)


## Read in Data

temp <- list.files(path="RFID_stable_cohorts/data_raw/RNAscope/LH_counts/",pattern="*.csv")
xfiles <- lapply(temp, function(x) read.csv(paste0("RFID_stable_cohorts/data_raw/RNAscope/LH_counts/",x),fileEncoding="latin1"))                 
lapply(xfiles, head)
lapply(xfiles, colnames)

#just looking for count.touching 24:35
# 12, 11,10, 9,8, 6, 2


xfiles <- xfiles %>% map(~select(., 1:6,9,24:35))


lapply(xfiles, head)
lapply(xfiles, tail)
lapply(xfiles, colnames)


ll <- xfiles %>% map(~pivot_longer(., cols =8:19, names_to = 'target')) %>% 
  map(~group_by(.,cohort, mouse,target))  %>% map(~mutate(.,nn =(sum(value!=0)))) %>% map(~group_by(.,cohort, mouse,target))  
  map(~mutate(., N = nrow(.))) %>% 
  map(~mutate(., prop = nn/max(N))) %>% 
  map(~select(., 1:2,6, 10,11,12 )) %>% 
  map(~unique(.))

lapply(ll, head)


dx <- do.call(rbind, ll)
table(dx$target)
table(dx$cohort)

dxx <- dx %>% as.data.frame()

all <- rank %>% cbind(dxx)

head(all)



allx <- all %>% mutate(dom_group = as.factor(ifelse(.$ds_rank == 1, "DOM", "SUB")))
allx$target <- gsub(".Count.Touching", "", allx$target)

ggplot(allx, aes(dom_group, prop, color = dom_group))+
  geom_boxplot()+
  geom_jitter()+
facet_wrap(~target, scales = "free")+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  ylab("positive/ total cells")+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none", text = element_text(size=20)) +
  scale_y_continuous(expand = expansion(.3))


ggplot(allx, aes(dom_group, prop, color = dom_group))+
  geom_boxjitter(aes(fill = dom_group), outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85))  +
  facet_wrap(~target, scales = "free", ncol =3)+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  ylab("positive/ total cells")+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none", text = element_text(size=20)) +
  scale_y_continuous(expand = expansion(.3)) 




al <- allx %>% split(.$target)
#CRH
crh <- al$CRH
hist(crh$prop)

cm <-lmer(prop~dom_group+(1|mouse)+(1|cohort), data =crh)
summary(cm)


#TRH
trh <- al$TRH
hist(trh$prop)

tm <-lmer(prop~dom_group+(cohort|mouse), data =trh)
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
