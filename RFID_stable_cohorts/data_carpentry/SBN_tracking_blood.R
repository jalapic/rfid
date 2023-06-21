#LDA 

library(MASS)
library(tidyverse)
library(mixOmics)
#rank data 
df.blood<- readRDS("RFID_stable_cohorts/data_clean/RFID_blood_fordiscriminateanalysis.RDS")

#dataframe for analysis
dfd <- df.blood %>% filter(grepl("pre", ID))

colnames(dfd)
dfd$cm <- paste(dfd$cohort, dfd$mouse, sep = "-")
rank <- dfd %>% dplyr::select(c(15:18,29)) %>% unique(.)  %>%  column_to_rownames('cm')
#total transitions
total <- dfd %>% dplyr::select(c(13,14,18,26:29)) %>%filter(day %in% c(1:10)) %>%  group_by(cohort, mouse,day) %>% 
  mutate(total2 = sum(total)) %>% ungroup(.) %>% dplyr::select(4,7,8) %>% unique(.)
total$day2 <- paste("Day", total$day,sep = "")
tt <- total %>% dplyr::select(-day) %>% 
  pivot_wider(names_from = day2, values_from = total2) %>% 
  column_to_rownames('cm')

#food cage transitions 
total <- dfd %>% dplyr::select(c(13,14,18,26:29)) %>%filter(day %in% c(1:10)) %>% filter(zone ==1) %>% 
    group_by(cohort, mouse,day) %>% mutate(total2 = sum(total)) %>% ungroup(.) %>% dplyr::select(4,7,8) %>% unique(.)
total$day2 <- paste("Day", total$day,sep = "")
tf <- total %>% dplyr::select(-day) %>% 
  pivot_wider(names_from = day2, values_from = total2) %>% 
  column_to_rownames('cm')
tf

b <- read_csv('RFID_stable_cohorts/data_clean/all_blood_Post.csv')
bx <- b %>% full_join(rank) %>% pivot_longer(c(2:11,19:25))  %>%dplyr::select(c(3,4,8,9,10))
means <- bx %>% group_by(dom, name) %>% summarise(mean = mean(value,na.rm =T)) %>% as.data.frame(.)
bxx <- bx  %>% full_join(means)

bxx$val <- ifelse(is.na(bxx$value), bxx$mean, bxx$value )


bxx <- bxx %>% dplyr::group_by(cohort, mouse, dom, name,val) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  pivot_wider(names_from = name, values_from = val) 

bxx$cm <- paste(bxx$cohort, bxx$mouse, sep = "-")
# bxx <- bxx %>% filter(cm != "6-4")
mo <- bxx %>% dplyr::select(5,9,10,11,15,17,19,22) %>% column_to_rownames('cm')
ma <- bxx %>% dplyr::select(6,8,12,13,14,16,18,20,21,22) %>% column_to_rownames('cm')


mo <- mo[c(1:8,11,12,9,10,13,15,16,14,19,17,18),]
ma <- ma[c(1:8,11,12,9,10,13,15,16,14,19,17,18),]
rownames(tt)
rownames(mo)
library(GGally)
ggpairs(mo)

mo2 <- mo %>% dplyr::select(1:5) 
ma2 <- ma %>% dplyr::select(1,2,4,5,7,9) 

m <- mo2 %>% cbind(ma2) 
ma3 <- ma %>%  dplyr::select(1,2,4,6,9) 
ml <- ma  %>%  dplyr::select(3,5,7,8) 
mo3 <- m %>%  dplyr::select(1:5) 
ma4 <- ml %>% cbind(ma3)
X <- list("Anorexigenic" =ma4,"Orexigenic"=mo3,"Movement" =tt)
lapply(X, dim)
Y <- rank$ds

list.keepX <- list(Anorexigenic = c(5, 5),Orexigenic = c(4,4),Movement= c(2, 3
                                                                          ))

MyResult.diablo <- block.splsda(X, Y, keepX = list.keepX)

plotIndiv(MyResult.diablo, 
          ind.names = FALSE, 
          legend=TRUE, cex=c(1,2),
          title = 'Results with DIABLO')

plotVar(MyResult.diablo)

x <- plotDiablo(MyResult.diablo, ncomp = 1)
plotDiablo(MyResult.diablo, ncomp = 2)


# set the number of features to use for the X datasets

# run the method
result.sparse.diablo.tcga <-  block.splsda(X, Y, keepX = list.keepX) 

# plot the contributions of each feature to each dimension
plotLoadings(result.sparse.diablo.tcga, ncomp = 1) 
plotIndiv(result.sparse.diablo.tcga) # plot the samples
plotVar(result.sparse.diablo.tcga)


plotArrow(MyResult.diablo, ind.names = FALSE, legend = TRUE, 
          title = 'TCGA, DIABLO comp 1 - 2')






df <- rank %>% cbind(ma,tt,mo)
colnames(df)
library(lmerTest)
# Fit the multiple linear regression model
m <-  lm(ds~Cpeptide2+Insulin+Leptin+TSH+Amylin+Ghrelin+ACTH+LH+GH+Day7+Day8+Day9+Day10+TNFa+IL6, data =df)
summary(m)







lmo <-  lm(ds_rank~Ghrelin+ACTH+LH+GH+Day10, data= df)
summary(lmo)



lma <-  lm(ds~Cpeptide2+Insulin+Leptin+TSH+Amylin+Day8+Day9+Day10+TNFa+IL6, data= df)
summary(lma)

or <- mo %>% dplyr::select(Ghrelin,ACTH,LH,GH)
an <- ma %>% dplyr::select(c(1:5,8,9))
mt <- tt %>% dplyr::select(c(Day8,Day9,Day10))

ggpairs(or)
ggpairs(an)
ggpairs((mt))


#CCA
library(CCA)
X <- an[c(1:9,11:19),] %>% scale()
Y <- mt[c(1:9,11:19),] %>% scale()

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



library(CCA)
X <- or[c(1:9,11:19),] %>% scale()
Y <- mt[c(1:9,11:19),] %>% scale()

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


