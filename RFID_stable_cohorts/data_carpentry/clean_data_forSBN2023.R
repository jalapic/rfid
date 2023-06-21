library(tidyverse)


#ARC
df.a <- read.csv("RFID_stable_cohorts/data_raw/RNAscope/ARC_summary.csv", fileEncoding="latin1")
df.ax <- df.a %>% dplyr::select(c(1:4,23:34)) %>% mutate(region = "ARC") %>% 
  filter(area == 'M') 
df.ax$mouse <- df.ax$id
df.ax <- df.ax %>% dplyr::select(-area, -id)

#PVN
df.p <- read.csv("RFID_stable_cohorts/data_raw/RNAscope/PVN_summary.csv", fileEncoding="latin1")
df.px <- df.p %>% dplyr::select(c(1:4,24:35))  %>% mutate(region = "PVN") %>% pivot_longer(5:16) %>% 
  pivot_wider(names_from = side, values_from = value) %>% mutate(total = left +right) %>% 
  dplyr::select(cohort, mouse, section,region, name, total) %>% 
  pivot_wider(names_from = name, values_from = total)

#VMH
df.v <- read.csv("RFID_stable_cohorts/data_raw/RNAscope/VmH_summary.csv", fileEncoding="latin1")
df.vx <- df.v %>% dplyr::select(c(1:4,24:35))  %>% mutate(region = "VMH") %>% pivot_longer(5:16) %>% 
  pivot_wider(names_from = side, values_from = value) %>% mutate(total = left +right) %>% 
  dplyr::select(cohort, mouse, section,region, name, total) %>% 
  pivot_wider(names_from = name, values_from = total)

#LH
df.l <- read.csv("RFID_stable_cohorts/data_raw/RNAscope/LH_summary.csv", fileEncoding="latin1")
df.lx <- df.v %>% dplyr::select(c(1:4,24:35))  %>% mutate(region = "LH") %>% pivot_longer(5:16) %>% 
  pivot_wider(names_from = side, values_from = value) %>% mutate(total = left +right) %>% 
  dplyr::select(cohort, mouse, section,region, name, total) %>% 
  pivot_wider(names_from = name, values_from = total)

#combine
colnames(df.ax)
colnames(df.px)
df.brain <- df.ax %>% rbind(df.px,df.vx,df.lx)
df.brain2 <- df.ax %>% rbind(df.px,df.vx,df.lx)
df.brain2 <- df.brain2 %>% pivot_longer(3:14) %>% pivot_wider(names_from = section, values_from = value) %>% 
  mutate(avg = (`1`+`2`)/2) %>% dplyr::select(-`1`, -`2`) %>%  
  pivot_wider(names_from = name, values_from = avg) 

df.brain2$cm <- paste(df.brain2$cohort, df.brain2$mouse, sep = "-")

#rank data 
df.blood<- readRDS("RFID_stable_cohorts/data_clean/RFID_blood_fordiscriminateanalysis.RDS")

#dataframe for analysis
dfd <- df.blood %>% full_join(df.brain)  
dfd <- dfd %>% filter(grepl("pre", ID))

colnames(dfd)
dfd$cm <- paste(dfd$cohort, dfd$mouse, sep = "-")
#rank data cols 14:18
#tracking cols 26:28
#metabolic blood cols 2:11
#endocrine blood cols 19:25
#metabolic brain cols 30:42

library(mixOmics)
rank <- dfd %>% dplyr::select(c(15:18,43)) %>% unique(.) %>%  column_to_rownames('cm')
track10 <-dfd %>% dplyr::select(c(26:28,43)) %>% filter(day == 10) %>% group_by(cm) %>% mutate(total2 = sum(total)) %>% dplyr::select(total2, cm) %>% unique(.) %>%  column_to_rownames('cm')
endo.blood <- dfd %>% dplyr::select(c(19:25,43)) %>% unique(.) %>%  column_to_rownames('cm')
met.blood <- dfd %>% dplyr::select(c(2:11,43)) %>% unique(.) %>%  column_to_rownames('cm')
met.a <- df.brain2 %>% filter(region == "ARC") %>% unique(.) %>% column_to_rownames("cm") %>% 
  dplyr::select(c(4:15))
met.p <- df.brain2 %>% filter(region == "PVN") %>% unique(.) %>% column_to_rownames("cm")%>% 
  dplyr::select(c(4:15))
met.v <- df.brain2 %>% filter(region == "VMH") %>% unique(.) %>% column_to_rownames("cm")%>% 
  dplyr::select(c(4:15))
met.l <- df.brain2 %>% filter(region == "LH") %>% unique(.) %>% column_to_rownames("cm")%>% 
  dplyr::select(c(4:15))

met.bloodx <- met.blood[c(2,6,7,8,9,14,15,17),]
endo.bloodx <- endo.blood[c(2,6,7,8,9,14,15,17),]
rank2 <- rank[c(2,6:9,14,15,17),] %>% dplyr::select(dom) %>%  as.data.frame()
rank2$dom <- as.factor(rank2$dom)
trackx <- track10 %>%rownames_to_column(., var = "cm") 
trackx <- trackx[c(2,6:9,14,15,17),]
trackxx <- trackx %>% dplyr::select(total2)

X <- list(a = met.a,l = met.l,p =met.p, v =met.v,b= met.bloodx)

Y <- rank2$ds_rank
result.diablo <- block.plsda(X, Y) # run the method

plotIndiv(result.diablo) # plot the samples
plotVar(result.diablo) 


met.a.spca <- spca((met.a), keepX=c(6,6)) # 1 Run the method
plotIndiv(met.a.spca)               # 2 Plot the samples
plotVar(met.a.spca)  

plotLoadings(met.a.spca, comp = 1) #TRH, MC4R, LepRb, Ghrh, Inr, 2 = GHRH, LepRB
plotLoadings(met.a.spca, comp = 2) #Agrp, trh, npy, Ghrl, Ghsr1a, 2 = GHR1a, Ghrl
# get rid of CRH, kiss1 for arc 

met.p.spca <- spca((met.p), keepX=c(6,6)) # 1 Run the method
plotIndiv(met.p.spca)               # 2 Plot the samples
plotVar(met.p.spca)  

plotLoadings(met.p.spca, comp = 1) #CRH, agrp, pomc, ghrl, inr, ghsr1a
plotLoadings(met.p.spca, comp = 2) #ghrl, crh, mc4r, kiss1, leprb, trh
#get rid of npy & ghrh

met.v.spca <- spca((met.v), keepX=c(6,6)) # 1 Run the method
plotIndiv(met.v.spca)               # 2 Plot the samples
plotVar(met.v.spca)  

plotLoadings(met.v.spca, comp = 1) #kiss, ghrh, trh, ghrl, inr, ghsr1a
plotLoadings(met.v.spca, comp = 2) #ghrl,  kiss1, leprb, pomc, crh, npy
#get rid of agrp & mc4r

met.l.spca <- spca((met.l), keepX=c(6,6)) # 1 Run the method
plotIndiv(met.l.spca)               # 2 Plot the samples
plotVar(met.l.spca)  

plotLoadings(met.l.spca, comp = 1) #kiss, ghrh, trh, ghrl, inr, ghsr1a
plotLoadings(met.l.spca, comp = 2) #ghrl,  kiss1, leprb, pomc, crh, npy
#get rid of agrp & mc4r

met.b.spca <- spca((met.bloodx), keepX=c(4,4)) # 1 Run the method
plotIndiv(met.b.spca)               # 2 Plot the samples
plotVar(met.b.spca)  

plotLoadings(met.b.spca, comp = 1) #Pyy, ghrelin, TNFa, amylin, IL6, MCP1, 
plotLoadings(met.b.spca, comp = 2) #ghrelin, secretin, tnfa,leptin, Insulin, cepeptide2

X <- list(a = met.a,l = met.l,p =met.p, v =met.v,b= met.bloodx)
Y <- rank2$dom
result.diablo <- block.plsda(X, Y) # run the method
plotIndiv(result.diablo)               # 2 Plot the samples
plotVar(result.diablo) 

plotLoadings(result.diablo) #Pyy, ghrelin, TNFa, amylin, IL6, MCP1, 
plotLoadings(result.diablo, comp = 2) #ghrelin, secretin, tnfa,leptin, Insulin, cepeptide2

list.keepX <- list(a = c(5, 5),l = c(5, 5),p =c(5, 5), v = c(5, 5),b = c(5, 5))

MyResult.diablo <- block.plsda(X, Y)
plotIndiv(MyResult.diablo, 
          ind.names = FALSE, 
          legend=TRUE, cex=c(1,2),
          title = 'Results with DIABLO')

plotVar(MyResult.diablo)



plotDiablo(MyResult.diablo, ncomp = 1)
plotDiablo(MyResult.diablo, ncomp = 2)

plotLoadings(MyResult.diablo, comp = 1, contrib = "max")
plotLoadings(MyResult.diablo, comp = 2, contrib = "max")
plotLoadings(MyResult.diablo, comp = 1, contrib = "min")
plotLoadings(MyResult.diablo, comp = 2, contrib = "min")


# just oxergenic data 
met.ax <- dfd %>% dplyr::select(30:43) %>%  filter(region == "ARC") %>% 
  unique(.) %>% dplyr::select(-region) 
met.ax$dup <- c('a', 'b','a', 'b','a', 'b','a', 'b','a', 'b','a', 'b','a','a', 'b')
met.ax$cm2 <- paste0(met.ax$cm, met.ax$dup)
mma <- as.matrix(met.ax[,1:12])
rownames(mma) <- met.ax$cm2

met.lx <- dfd %>% dplyr::select(30:43) %>%  filter(region == "LH") %>% 
  unique(.) %>% dplyr::select(-region) 
met.lx$dup <- c('a', 'b','a', 'b','a', 'b','a', 'b','a', 'b','a', 'b','a',"b",'a', 'b')
met.lx$cm2 <- paste0(met.lx$cm, met.lx$dup)
mml <- as.matrix(met.lx[,1:12])
rownames(mml) <- met.lx$cm2

b <- read_csv('RFID_stable_cohorts/data_clean/all_blood_Post.csv')

bg <- b %>% dplyr::select(Ghrelin, ACTH, FSH, LH, GH, LH, mouse, cohort,dom) 
bg$cm <- paste(bg$cohort, bg$mouse, sep = "-")
bg[is.na(bg)] <- 0
bg2 <- bg %>% rbind(bg) %>% arrange(cm)
bg2 <- bg2[c(3,4,13:18,23,24,31:34,5,6),]
bg2$dup <- rep(c('a', 'b'), times = 8)
bg2$cm2 <- paste0(bg2$cm, bg2$dup)
bg2 <- bg2[c(1:13,15,16),]
bg3 <- bg2[,c(1:5)]
mbg<- sapply(bg3, as.integer)
rownames(mbg) <- bg2$cm2
mgbx <- mbg %>% as.data.frame()

mag <- mma[,c(3,6,7,8,11)]
magx <- mag %>% as.data.frame()
mlg <- mml[c(1:12,14:16),c(3,6,7,8,11)]
mlgx <-  mlg %>% as.data.frame()

rank <- dfd %>% dplyr::select(c(15:18,43)) %>% unique(.)
rank2 <- rank[c(2,6:9,14,15,17),] 
rankx <- rank2 %>% rbind(rank2) %>% arrange(cm)
rankx$dup <- c('a', 'b','a', 'b','a', 'b','a', 'b','a', 'b','a', 'b','a',"b",'a', 'b')
rankx$cm2 <- paste0(rankx$cm, rankx$dup)
  rankxx <- rankx %>% dplyr::select(dom) %>%  as.data.frame()
  row.names(rankxx) <- rankx$cm2
rankxx$dom <- as.factor(rankxx$dom)
rankxx <- rankxx[c(1:13,15,16),]

X <- list(a = magx,l = mlgx,b= mgbx)
lapply(X, dim)
Y <- rankxx$dom

list.keepX <- list(a = c(2, 2),l = c(2, 2),b = c(2, 2))

MyResult.diablo <- block.plsda(X, Y)

plotIndiv(MyResult.diablo, 
          ind.names = FALSE, 
          legend=TRUE, cex=c(1,2),
          title = 'Results with DIABLO')

plotVar(MyResult.diablo)

plotDiablo(MyResult.diablo, ncomp = 1)
plotDiablo(MyResult.diablo, ncomp = 2)
