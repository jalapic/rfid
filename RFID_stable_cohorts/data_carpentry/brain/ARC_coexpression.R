library(tidyverse)


## Read in Data

temp <- list.files(path="RFID_stable_cohorts/data_raw/RNAscope/ARC_counts/",pattern="*.csv")
xfiles <- lapply(temp, function(x) read.csv(paste0("RFID_stable_cohorts/data_raw/RNAscope/ARC_counts/",x),fileEncoding="latin1"))                 
lapply(xfiles, head)
lapply(xfiles, colnames)

#just looking for count.touching 24:35

xfiles <- xfiles %>% map(~select(., 1:5,24:35))

lapply(xfiles, head)
lapply(xfiles, tail)
lapply(xfiles, colnames)

#get multiple targets
ll2 <- xfiles %>% map(~group_by(.,Number)) %>% 
  map(~mutate(.,AgRP_NPY = `AgRP.Count.Touching` + `NPY.Count.Touching`)) %>% 
  map(~mutate(.,AgRP_NPY_LepRb = `AgRP.Count.Touching` + `NPY.Count.Touching` +`LepRb.Count.Touching`)) %>% 
  map(~mutate(.,POMC_LEPRB = `POMC.Count.Touching` + `LepRb.Count.Touching`))




lapply(ll2, head)


ll2x<- ll2 %>% map(~select(.,1:5,19:20))





ll3 <- xfiles %>% map(~group_by(.,Number)) %>% 
  map(~mutate(.,AgRP_NPY = `AgRP.Count.Touching` + `NPY.Count.Touching`)) %>% 
  map(~mutate(.,AgRP_NPY_Ghrelin_GHSR1a = `AgRP.Count.Touching` + `NPY.Count.Touching` +`Ghrl.Count.Touching` + GHSR1a.Count.Touching )) %>% 
  map(~mutate(.,POMC_MC4R = `POMC.Count.Touching` + `MC4R.Count.Touching`))

lapply(ll3, head)
lapply(ll3, colnames)

ll2x<- ll3 %>% map(~select(.,1:5,18:20))








#get number of rows with non zeros


lapply(xfiles, head)
lapply(xfiles, colnames)




ll <- xfiles %>% map(~pivot_longer(., cols =6:17, names_to = 'target')) %>% 
  map(~group_by(.,mouse, cohort, target)) %>% map(~mutate(.,nn =(sum(value!=0)))) %>% 
  map(~mutate(., N = nrow(.))) %>% 
  map(~mutate(., prop = nn/max(Number))) %>%  
  map(~select(., mouse, cohort, area, target, nn, prop)) %>% 
  map(~unique(.))

lapply(ll, head)

dx <- do.call(rbind, ll)
table(dx$target)

dx <- dx %>% filter(target != 'CRH.Area.Percent....')

rank <- read_csv("RFID_stable_cohorts/data_clean/socialbehavior/rank_data.csv")
head(rank)


all <- rank %>% full_join(dx) %>% na.omit(.)

head(all)



allx <- all %>% mutate(dom_group = as.factor(ifelse(.$ds_rank == 1, "DOM", "SUB")))
allx$target <- gsub(".Count.Touching", "", allx$target)






ggplot(allx, aes(dom_group, prop, color = interaction(area)))+
  geom_boxplot(outlier.shape=NA)+ 
  geom_point(size = 2, alpha = 0.6, position = position_jitterdodge())+
  facet_wrap(~target, scales = "free_y")+ 
  theme_minimal()





all2x <- allx %>% filter( area == "M")


# all2x$target <- factor(all2x$target, level = c("CRH", "TRH", "GHRH", "Kiss1", "LepRb", "AgRP", "NPY", "Ghrl", "POMC", "INR", "GHSR1a", "MC4R"))

ggplot(all2x, aes(dom_group, prop, color = dom_group))+
  geom_boxjitter(aes(fill = dom_group), outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 , jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  facet_wrap(~target, scales = "free", ncol =3)+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  ylab("positive/ total cells")+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none", text = element_text(size=20)) +
  scale_y_continuous(expand = expansion(.3)) 


lapply(ll2, colnames)

ll2x <- ll2x %>% map(~pivot_longer(., cols =6:8, names_to = 'target')) %>% 
  map(~group_by(.,target)) %>% map(~mutate(.,nn =(sum(value >=2)))) %>% 
  map(~mutate(., prop = nn/max(Number))) %>% 
  map(~select(., mouse, cohort, area, target, nn, prop)) %>% 
  map(~unique(.))


dx2 <- do.call(rbind, ll2x)
dx2 <- dx2 %>% filter(target != 'CRH.Area.Percent....')

ally <- rank %>% full_join(dx2) %>% na.omit(.)

head(ally)

library(viridis)

ally <- ally %>% mutate(dom_group = as.factor(ifelse(.$ds_rank == 1, "DOM", "SUB"))) 
ally$target <- gsub(".Count.Touching", "", ally$target)

all2y <- ally %>% filter( area == "M")

all2y$target <- gsub("_", "+", all2y$target)
all2y$target <- gsub('LEPRB', "LepRb", all2y$target)

ggplot(all2y, aes(dom_group, prop, color = dom_group))+
  geom_boxjitter(aes(fill = dom_group), outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  facet_wrap(~target, scales = "free")+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  ylab("positive/ total cells")+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none", text = element_text(size=20)) +
  scale_y_continuous(expand = expansion(.3)) 



ally


o <- ally %>% filter(target == "AgRP_NPY")
head(o)

a <- ally %>% filter(target == "POMC_MC4R")
head(a)

library(lmerTest)

hist(a$prop)
m <- lmer(ds_rank~prop+(1|mouse), data = a)
summary(m)

m <- lm(prop~dom, data = a)
summary(m)



