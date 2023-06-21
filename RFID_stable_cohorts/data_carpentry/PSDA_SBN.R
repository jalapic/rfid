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
df<- readRDS("RFID_stable_cohorts/data_clean/RFID_blood_fordiscriminateanalysis.RDS")
dfd <- df %>% filter(grepl("pre", ID))

dfd <- dfd %>% full_join(df.brain) %>% filter(region == "ARC") 

colnames(dfd)
dfd$cm <- paste(dfd$cohort, dfd$mouse, sep = "-")
#rank data cols 14:18
#tracking cols 26:28
#metabolic blood cols 2:11
#endocrine blood cols 19:25
#metabolic brain cols 30:42

library(mixOmics)
rank <- dfd %>% dplyr::select(c(13:18,29)) %>% unique(.) %>%  column_to_rownames('cm')
track <-dfd %>% dplyr::select(c(26:29)) 
track$day2 <- paste0("Day", track$day)
track <- track %>% group_by(cm,day2) %>% mutate(total2 = sum(total)) %>% 
  dplyr::select(day2,total2, cm) %>% dplyr::group_by(cm,total2, day2) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>% 
  pivot_wider(names_from = day2, values_from = total2) %>%  unique(.) %>%  column_to_rownames('cm')
blood <- dfd %>% dplyr::select(c(2:11,19:25,29)) %>% unique(.) %>%  column_to_rownames('cm')
met.a <-dfd %>% dplyr::select(c(29:42)) %>% unique(.)  


met.bloodx <- met.blood[c(2,6,7,8,9,14,15,17),]
endo.bloodx <- endo.blood[c(2,6,7,8,9,14,15,17),]
rank2 <- rank[c(2,6:9,14,15,17),] %>% dplyr::select(dom) %>%  as.data.frame()
rank2$dom <- as.factor(rank2$dom)
trackx <- track10 %>%rownames_to_column(., var = "cm") 
trackx <- trackx[c(2,6:9,14,15,17),]
trackxx <- trackx %>% dplyr::select(total2)



# just oxergenic data 
met.ax <- dfd %>% dplyr::select(29:42)  %>% 
  unique(.) 
met.ax$dup <- c('a', 'b','a', 'b','a', 'b','a', 'b','a', 'b','a', 'b','a',"b",'a', 'b')
met.ax$cm2 <- paste0(met.ax$cm, met.ax$dup)
mma <- as.matrix(met.ax[,3:14])
rownames(mma) <- met.ax$cm2

met.lx <- dfd %>% dplyr::select(30:43) %>%  filter(region == "LH") %>% 
  unique(.) %>% dplyr::select(-region) 
met.lx$dup <- c('a', 'b','a', 'b','a', 'b','a', 'b','a', 'b','a', 'b','a',"b",'a', 'b')
met.lx$cm2 <- paste0(met.lx$cm, met.lx$dup)
mml <- as.matrix(met.lx[,1:12])
rownames(mml) <- met.lx$cm2

b <- read_csv('RFID_stable_cohorts/data_clean/all_blood_Post.csv')

bg <- b %>% dplyr::select(2,4:10,12,13, mouse, cohort,dom) 
bg$cm <- paste(bg$cohort, bg$mouse, sep = "-")
bg[is.na(bg)] <- 0
bg2 <- bg %>% rbind(bg) %>% arrange(cm)
bg2 <- bg2[c(3,4,13:18,23,24,31:34,5,6),]
bg2$dup <- rep(c('a', 'b'), times = 8)
bg2$cm2 <- paste0(bg2$cm, bg2$dup)
# bg2 <- bg2[c(1:13,15,16),]
bg3 <- bg2[,c(1:8)]
mbg<- sapply(bg3, as.integer)
rownames(mbg) <- bg2$cm2
mgbxax <- mbg %>% as.data.frame()



#track
tracka <- track %>% rownames_to_column(., var = "cm")
tracka$dup <- c("a","a","a","a","a","a","a","a")
tracka$cm2 <- paste0(tracka$cm, tracka$dup)
tracka <- tracka %>% dplyr::select(c(4:13,15)) %>% column_to_rownames("cm2")

trackb <- track %>% rownames_to_column(., var = "cm")
trackb$dup <- c("b","b","b","b","b","b","b","b")
trackb$cm2 <- paste0(trackb$cm, trackb$dup)
trackb <- trackb %>% dplyr::select(c(4:13,15)) %>% column_to_rownames("cm2")

trackx <- tracka %>% rbind(trackb)


rownames(trackxx)
rownames(mgbx)
rownames(mma)
trackxx <- trackx[c(1,9,3,11,4,12,5,13,6,14,7,15,8,16,2,10),]


rank <- rank %>% rownames_to_column(., var = "cm")
rankx <- rank %>% rbind(rank)
rankx$dup <- c("a","a","a","a","a","a","a","a","b","b","b","b","b","b","b","b")
rankx$cm2 <- paste0(rankx$cm, rankx$dup)
# rankxx <- rankx %>% dplyr::select(dom) %>%  as.data.frame()
row.names(rankx) <- rankx$cm2
# rankxx$dom <- as.factor(rankxx$dom)
rankxx <- rankx[c(1,9,3,11,4,12,5,13,6,14,7,15,8,16,2,10),]

colnames(mma)
mogx <- mma %>% as.data.frame(.) %>%  dplyr::select(3,6,7,8,11) %>% cbind(mgbx) 

mogxx <- mogx[,c(3:9)]
  
magx <- mma %>% as.data.frame(.) %>% dplyr::select(2,5,9,10,12)%>% cbind(mgbxax) 
magxx <- magx[,c(1:6,9,8,10,12,13)]

X <- list(Orexgenic = mogx,Anorexgenic = magx,Movement=trackxx)
lapply(X, dim)
Y <- rankxx$dom

list.keepX <- list(Orexgenic = c(4, 4),Anorexgenic= c(5, 5),Movement = c(5, 5))

MyResult.diablo <- block.splsda(X, Y,keepX = list.keepX)

plotIndiv(MyResult.diablo, 
          ind.names = FALSE, 
          legend=TRUE, cex=c(1,2),
          title = 'Results with DIABLO')

plotVar(MyResult.diablo)

plotDiablo(MyResult.diablo, ncomp = 1)
plotDiablo(MyResult.diablo, ncomp = 2)



library(CCA)

X <- magxx %>% scale()

Y <- trackxx[,c(1:4,6:10)]%>% scale()

model2 <- cc(X, Y)
plt.cc(model2, var.label = TRUE)


cc_results <- cancor(X,Y)

cc_results$xcoef
cc_results$ycoef 

cc_results$cor


CC1_X <- as.matrix(X) %*% cc_results$xcoef[, 1]
CC1_Y <- as.matrix(Y) %*% cc_results$ycoef[, 1]


CC2_X <- as.matrix(X) %*% cc_results$xcoef[, 2]
CC2_Y <- as.matrix(Y) %*% cc_results$ycoef[, 2]


cor(CC1_X,CC1_Y)
cor(CC2_X,CC2_Y)

assertthat::are_equal(cc_results$cor[1], 
                      cor(CC1_X,CC1_Y)[1])

cca_df <- rank[c(1:9,11:19),] %>% 
  mutate(CC1_X=CC1_X,
         CC1_Y=CC1_Y,
         CC2_X=CC2_X,
         CC2_Y=CC2_Y)

cca_df %>% 
  ggplot(aes(x=dom,y=CC1_X, color=dom))+
  geom_boxplot(width=0.5)+
  geom_jitter(width=0.15)+
  theme(legend.position="non")+
  theme_bw() +
  ggtitle('Blood')


cca_df %>% 
  ggplot(aes(x=CC1_X,y=CC1_Y, color=dom))+
  geom_point() +theme_bw() 






##anormaga <- mma[,c(2,4,5)]
magxa <- maga %>% as.data.frame()
mlga <- mml[c(1:13,15:16),c(2,4,5)]
mlgxa <-  mlga %>% as.data.frame()

colnames(b)
bg <- b %>% dplyr::select(IL6, Leptin, TSH,TNFa, mouse, cohort,dom) 
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
mgbxa <- mbg %>% as.data.frame()

X <- list(a = magxa,l = mlgxa,b= mgbxa)
lapply(X, dim)
Y <- rankxx$dom

#CCA
library(CCA)
X <- magx %>% scale()
Y <- mgbx %>% scale()

model2 <- cc(X, Y)
plt.cc(model2, var.label = TRUE)


cc_results <- cancor(X,Y)

cc_results$xcoef
cc_results$ycoef 

cc_results$cor


CC1_X <- as.matrix(X) %*% cc_results$xcoef[, 1]
CC1_Y <- as.matrix(Y) %*% cc_results$ycoef[, 1]


CC2_X <- as.matrix(X) %*% cc_results$xcoef[, 2]
CC2_Y <- as.matrix(Y) %*% cc_results$ycoef[, 2]


cor(CC1_X,CC1_Y)
cor(CC2_X,CC2_Y)

assertthat::are_equal(cc_results$cor[1], 
                      cor(CC1_X,CC1_Y)[1])

cca_df <- rankxx %>% 
  mutate(CC1_X=CC1_X,
         CC1_Y=CC1_Y,
         CC2_X=CC2_X,
         CC2_Y=CC2_Y)


cca_df %>% 
  ggplot(aes(x=dom,y=CC1_Y, color=dom))+
  geom_boxplot(width=0.5)+
  geom_jitter(width=0.15)+
  theme(legend.position="non")+
  theme_bw() +
  ggtitle('Long-term markers: ARC + Blood')


cca_df %>% 
  ggplot(aes(x=CC1_X,y=CC1_Y, color=dom))+
  geom_point() +theme_bw() 



library(GGally)
ggpairs(magx)
ggpairs(mgbx)
