library(tidyverse)


## Read in Data

temp <- list.files(path="RFID_stable_cohorts/data_raw/RNAscope/ARC_counts/",pattern="*.csv")
xfiles <- lapply(temp, function(x) read.csv(paste0("RFID_stable_cohorts/data_raw/RNAscope/ARC_counts/",x),fileEncoding="latin1"))                 
lapply(xfiles, head)
lapply(xfiles, colnames)

#just looking for count.touching 24:35

xfiles <- xfiles %>% map(~select(., 1:5,8,24:35))

lapply(xfiles, head)
lapply(xfiles, tail)
lapply(xfiles, colnames)

#getting mutilple targets 

ll3 <- xfiles %>% map(~group_by(.,cohort, mouse, Number)) %>% 
  map(~dplyr::mutate(.,AgRP_NPY_Ghrelin_GHSR1a = `AgRP.Count.Touching` + `NPY.Count.Touching` +`Ghrl.Count.Touching` + GHSR1a.Count.Touching )) %>% 
  map(~dplyr::mutate(.,POMC_MC4R = `POMC.Count.Touching` + `MC4R.Count.Touching`)) %>% 
  map(~dplyr::mutate(.,AgRP_NPY_LepRb = `AgRP.Count.Touching` + `NPY.Count.Touching` +`LepRb.Count.Touching`)) %>% 
  map(~dplyr::mutate(.,POMC_MC4R_LepRb = `POMC.Count.Touching` + `MC4R.Count.Touching`+`LepRb.Count.Touching`))

lapply(ll3, head)
lapply(ll3, colnames)

ll2x<- ll3 %>% map(~select(.,1,2,3,4,5,6,19:22))

# 6898 Accepted 

dx <- do.call(rbind, ll2x)
dx2 <- dx %>% pivot_longer(7:10) %>% filter(area == "M") %>% unique(.)




rank <- read_csv("RFID_stable_cohorts/data_clean/socialbehavior/rank_data.csv")
head(rank)


all <- rank %>% full_join(dx2) %>% filter(ds_rank %in% c(1,6))

head(all)

allx <- all %>% group_by(cohort, mouse,section,name) %>% mutate(total = sum(value)) %>% ungroup(.)

allx <- allx %>%group_by(cohort, mouse,section, name) %>% 
  mutate(.,total2 = sum(Accepted)) %>% mutate(prop = total/total2) %>% ungroup(.)

range(allx$prop, na.rm =T)

allx <- allx %>% select(1:5,11,15)

allx <- allx %>% unique(.) %>%  mutate(dom_group = as.factor(ifelse(.$ds_rank == 1, "DOM", "SUB")))
allx$name <- gsub("_", "+", allx$name)

gh <- allx %>% filter(name == "AgRP+NPY+Ghrelin+GHSR1a")
lo <- allx %>% filter(name =="AgRP+NPY+LepRb")
lp <- allx %>% filter(name == "POMC+MC4R+LepRb")
p <- allx %>% filter(name == "POMC+MCR4")

mod2 <- lmer(prop~dom_group+(1|mouse), data =lp)
summary(mod2)

allx2 <- na.omit(allx)
co  <- ggplot(gh, aes(dom_group, prop, fill = dom_group))+
  geom_boxjitter(aes(fill = dom_group), outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
   facet_wrap(~name,scales = "free")+
  scale_color_manual(values = c("#0071A2", "#FF6600"))+
  scale_fill_manual(values = c("#0071A2", "#FF6600"))+
  ylab("positive/ total cells")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none", text = element_text(size=20)) +
  scale_y_continuous(expand = expansion(.2)) 

co
ggsave("RFID_stable_cohorts/imgs/coexp_ghrelin.png", co ,height = 5, width = 5, dpi = 300)

## 
b <- read_csv('RFID_stable_cohorts/data_clean/all_blood_Post.csv')
bx <- b %>% full_join(rank) %>% pivot_longer(c(2:11,19:25))  %>%dplyr::select(c(3,4,5,8,9,10))
means <- bx %>% group_by(dom, name) %>% summarise(mean = mean(value,na.rm =T)) %>% as.data.frame(.)
bxx <- bx  %>% full_join(means)
bxx$val <- ifelse(is.na(bxx$value), bxx$mean, bxx$value)

bxx2 <- bxx %>%  select(-mean, -value) %>% pivot_wider(names_from = name, values_from = val) 

gh <- allx %>% filter(name == "AgRP+NPY+Ghrelin+GHSR1a")
lo <- allx %>% filter(name == "AgRP+NPY+LepRb")
lp <- allx %>% filter(name == "POMC+MC4R+LepRb")
p <- allx %>% filter(name == "POMC+MC4R")

# allx <- all %>% group_by(cohort, mouse,section,name) %>% mutate(total = sum(value)) %>% ungroup(.)
gr <- bxx2 %>% select(cohort, mouse, dom, Ghrelin) %>% full_join(gh)
gr2 <- gr %>% na.omit(.)

lo <- bxx2 %>% select(cohort, mouse, dom, Leptin) %>% full_join(lo)
lo2 <- lo %>% na.omit(.)
la <- bxx2 %>% select(cohort, mouse, dom, Leptin) %>% full_join(lp)
la2 <- la %>% na.omit(.)

tg <- bxx2 %>% select(cohort, mouse, dom, TSH) %>% full_join(gh)
tg2 <- tg %>% na.omit(.)

to <- bxx2 %>% select(cohort, mouse, dom, TSH) %>% full_join(lo)
to2 <- to %>% na.omit(.)
ta <- bxx2 %>% select(cohort, mouse, dom, TSH) %>% full_join(lp)
ta2 <- ta %>% na.omit(.)


ghy <- bxx2 %>% select(cohort, mouse, dom, GH) %>% full_join(lo)
ghy <- ghy %>% na.omit(.)

gl <- ggplot(ghy, aes(prop,GH, color = dom))+
  geom_point() +
  geom_smooth(method = "lm", se = F)+
  xlab("AgRP+NPY+Ghrelin+GHSR1a/total cells")+
  ylab("Plasma Growth Hormone (pg/ml)")+
  scale_color_manual(values = c("#0071A2", "#FF6600"))+
  scale_fill_manual(values = c("#0071A2", "#FF6600"))+
  theme_bw()+theme(legend.position = "none", text = element_text(size=15)) 

gl
ggsave("RFID_stable_cohorts/imgs/line_growthhormone.png", gl ,height = 5, width = 5, dpi = 300)




ggplot(tg2, aes(prop, TSH, color = dom))+
  geom_point() +
  geom_smooth(method = "lm", se = F)+
  xlab("AgRP+NPY+Ghrelin+GHSR1a/total cells")+
  ylab("Plasma TSH (pg/ml")

tt <- ggplot(to2, aes(prop, TSH, color = dom))+
  geom_point() +
  geom_smooth(method = "lm", se = F)+
  xlab("AgRP+NPY+LepRb/total cells")+
  ylab("Plasma TSH (pg/ml")+
  scale_color_manual(values = c("#0071A2", "#FF6600"))+
  scale_fill_manual(values = c("#0071A2", "#FF6600"))+
  theme_bw()+theme(legend.position = "none", text = element_text(size=15)) 

tt2 <- ggplot(ta2, aes(prop, TSH, color = dom))+
  geom_point() +
  geom_smooth(method = "lm", se = F)+
  xlab("POMC+MC4R+LepRb/total cells")+
  ylab("Plasma TSH (pg/ml")+
  scale_color_manual(values = c("#0071A2", "#FF6600"))+
  scale_fill_manual(values = c("#0071A2", "#FF6600"))+
  theme_bw()+theme(legend.position = "none", text = element_text(size=15)) 

tt3 <- gridExtra::grid.arrange(tt, tt2,nrow =1)

ggsave("RFID_stable_cohorts/imgs/line_tsh.png", tt3 ,height = 4, width = 8, dpi = 300)


ll <- ggplot(lo2, aes(prop,Leptin, color = dom))+
  geom_point() +
  geom_smooth(method = "lm", se = F)+
  xlab("AgRP+NPY+LepRb/total cells")+
  ylab("Plasma Leptin (pg/ml)")+
  scale_color_manual(values = c("#0071A2", "#FF6600"))+
  scale_fill_manual(values = c("#0071A2", "#FF6600"))+
  theme_bw()+theme(legend.position = "none", text = element_text(size=15)) 


ll2 <- ggplot(la2, aes(prop,Leptin, color = dom))+
  geom_point() +
  geom_smooth(method = "lm", se = F)+
  xlab("POMC+MC4R+LepRb/total cells")+
  ylab("Plasma Leptin (pg/ml)")+
  scale_color_manual(values = c("#0071A2", "#FF6600"))+
  scale_fill_manual(values = c("#0071A2", "#FF6600"))+
  theme_bw()+theme(legend.position = "none", text = element_text(size=15)) 

ll3 <- gridExtra::grid.arrange(ll, ll2,nrow =1)

ggsave("RFID_stable_cohorts/imgs/line_leptin.png", ll3 ,height = 4, width = 8, dpi = 300)


ml <- mx %>% dplyr::select(-glicko_rank) %>% filter(day %in%c(8))%>% full_join(la2) %>% na.omit(.) %>% unique(.)

x2 <- ggplot(ml, aes(total2,prop, color = dom)) + geom_point() +
  geom_smooth(method = "lm", se = F)+
  ylab("POMC+MC4R+LepRb/total cells")+
  xlab("Day 10 Cage Transitions")+
  scale_color_manual(values = c("#0071A2", "#FF6600"))+
  scale_fill_manual(values = c("#0071A2", "#FF6600"))+
  theme_bw()+theme(legend.position = "none", text = element_text(size=15)) 

mod <- lmer(total2~prop+dom+(cohort|mouse), data = ml)
summary(mod)

mlo<- mx %>% dplyr::select(-glicko_rank) %>% filter(day %in%c(8))%>% full_join(lo2) %>% na.omit(.) %>% unique(.)

x1 <- ggplot(mlo, aes(total2,prop, color = dom)) + geom_point() +
  geom_smooth(method = "lm", se = F)+
  ylab("AgRP+NPY+LepRb/total cells")+
  xlab("Day 10 Cage Transitions")+
  scale_color_manual(values = c("#0071A2", "#FF6600"))+
  scale_fill_manual(values = c("#0071A2", "#FF6600"))+
  theme_bw()+theme(legend.position = "none", text = element_text(size=15)) 



mlgf<- mx %>% dplyr::select(-glicko_rank) %>% filter(day %in%c(8))%>% full_join(gr2) %>% na.omit(.) %>% unique(.)

x4 <- ggplot(mlgf, aes(total2,prop, color = dom)) + geom_point() +
  geom_smooth(method = "lm", se = F)+
  ylab("AgRP+NPY+Ghrelin+GHSR1a/total cells")+
  xlab("Day 10 Cage Transitions")+
  scale_color_manual(values = c("#0071A2", "#FF6600"))+
  scale_fill_manual(values = c("#0071A2", "#FF6600"))+
  theme_bw()+theme(legend.position = "none", text = element_text(size=15)) 
x4

x3 <- gridExtra::grid.arrange(x4, x1, x2,nrow =1)
ggsave("RFID_stable_cohorts/imgs/line_movement.png", x3 ,height = 5, width = 15, dpi = 300)

library(lmerTest)
lm <- lmer(Ghrelin~prop +dom+(cohort|mouse), data = gr2)
summary(lm)

lm2 <- lmer(TSH~prop +dom+(cohort|mouse), data = tg2)
summary(lm2) # not sign 


lm3 <- lmer(Leptin[,1] +dom+(cohort|mouse), data = lo2)
summary(lm3) # not working 

lm3 <- lmer(Leptin+dom+(cohort|mouse), data = la2)
summary(lm3)





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



ll3 <- xfiles %>% map(~group_by(.,cohort, mouse, Number)) %>% 
  map(~dplyr::mutate(.,AgRP_NPY_Ghrelin_GHSR1a = `AgRP.Count.Touching` + `NPY.Count.Touching` +`Ghrl.Count.Touching` + GHSR1a.Count.Touching )) %>% 
  map(~dplyr::mutate(.,POMC_MC4R = `POMC.Count.Touching` + `MC4R.Count.Touching`)) %>% 
  map(~dplyr::mutate(.,AgRP_NPY_LepRb = `AgRP.Count.Touching` + `NPY.Count.Touching` +`LepRb.Count.Touching`)) %>% 
  map(~dplyr::mutate(.,POMC_MC4R_LepRB = `POMC.Count.Touching` + `MC4R.Count.Touching`+`LepRb.Count.Touching`))

lapply(ll3, head)
lapply(ll3, colnames)


### RAW DATA
ll2x<- ll3 %>% map(~select(.,1,2,3,4,5,6:18))



# 6898 Accepted 

dx <- do.call(rbind, ll2x)
colnames(dx)

dx2 <- dx %>% pivot_longer(7:18) %>% filter(area == "M") %>% unique(.)


rank <- read_csv("RFID_stable_cohorts/data_clean/socialbehavior/rank_data.csv")
head(rank)


all <- rank %>% full_join(dx2) %>% filter(ds_rank %in% c(1,6))

head(all)
table(allx$ds_rank)



allx <- all %>% group_by(cohort, mouse,section,name) %>% mutate(total = sum(value)) %>% ungroup(.)

allx <- allx %>%group_by(cohort, mouse,section, name) %>% 
  mutate(.,total2 = sum(Accepted)) %>% mutate(prop = total/total2) %>% ungroup(.)

range(allx$prop, na.rm =T)

allx <- allx %>% select(1:5,12,16)

allx <- allx %>% unique(.) %>%  mutate(dom_group = as.factor(ifelse(.$ds_rank == 1, "DOM", "SUB")))
allx$name <- gsub(".Count.Touching", "", allx$name)


allx2 <- na.omit(allx)
l <- allx2 %>% filter(name == 'INR')
mod <- lmer(prop~dom_group+(cohort|mouse), data = l)
summary(mod)


raw  <- ggplot(allx2, aes(dom_group, prop, fill = dom_group))+
  geom_boxjitter(aes(fill = dom_group),outlier.color = NA, jitter.shape = 21,
                alpha = 0.5,
                jitter.height = 0.0, jitter.width = 0.030, errorbar.draw = TRUE,
                position = position_dodge(0.85)) +
  facet_wrap(factor(name,levels = c("CRH", "TRH", "GHRH", "Kiss1", "LepRb", "AgRP", "NPY", "Ghrl", "POMC", "INR", "GHSR1a", "MC4R")) ~ ., scales = 'free', ncol =3)+
  scale_color_manual(values = c("#0071A2", "#FF6600"))+
  scale_fill_manual(values = c("#0071A2", "#FF6600"))+
  theme_bw()+
  theme(legend.position = "none", text = element_text(size=20))+xlab("")+ 
  ylab("postive/total cells") +
  scale_y_continuous(expand = expansion(.2)) 

raw

ggsave("RFID_stable_cohorts/imgs/ARC_raw2.png", raw ,height = 12, width = 11, dpi = 300)






