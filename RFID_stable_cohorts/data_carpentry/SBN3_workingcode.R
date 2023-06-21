
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





# just oxergenic data 
met.ax <- dfd %>% dplyr::select(30:43) %>%  filter(region == "ARC") %>% 
  unique(.) %>% dplyr::select(-region) 

met.ax$dup <- c('a', 'b','a', 'b','a', 'b','a', 'b','a', 'b','a', 'b','a','a', 'b')
met.ax$cm2 <- paste0(met.ax$cm, met.ax$dup)
mma <- as.matrix(met.ax[,1:12])
rownames(mma) <- met.ax$cm2
add_rows <- mma[13, ] %>% t(.) %>% as.data.frame(.)
rownames(add_rows) <- "8-6b"
mma <- mma %>% as.data.frame(.) %>% rbind(add_rows)


met.lx <- dfd %>% dplyr::select(30:43) %>%  filter(region == "LH") %>% 
  unique(.) %>% dplyr::select(-region) 
met.lx$dup <- c('a', 'b','a', 'b','a', 'b','a', 'b','a', 'b','a', 'b','a',"b",'a', 'b')
met.lx$cm2 <- paste0(met.lx$cm, met.lx$dup)
mml <- as.matrix(met.lx[,1:12])
rownames(mml) <- met.lx$cm2
# add_rows <- mml[13, ] %>% t(.) %>% as.data.frame(.)
# rownames(add_rows) <- "8-6b"
# mml <- mml %>% as.data.frame(.) %>% rbind(add_rows)


b <- read_csv('RFID_stable_cohorts/data_clean/all_blood_Post.csv')

bx <- b %>% full_join(rankx) %>% pivot_longer(c(2:11,19:25)) %>% dplyr::select(c(3,4,6,8,12,13))
means <- bx %>% group_by(dom, name) %>% summarise(mean = mean(value,na.rm =T)) %>% as.data.frame(.)
bxx <- bx  %>% full_join(means)

bxx$val <- ifelse(is.na(bxx$value), bxx$mean, bxx$value )


bxx <- bxx %>% dplyr::group_by(cohort, mouse, dom, name,val) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  pivot_wider(names_from = name, values_from = val)


bg <- bxx %>% dplyr::select(Ghrelin,GH,LH,FSH,ACTH,Prolactin, mouse, cohort,dom) 
bg$cm <- paste(bg$cohort, bg$mouse, sep = "-")
bg2 <- bg %>% rbind(bg) %>% arrange(cm)

bg2 <- bg %>% rbind(bg) %>% arrange(cm)
bg2 <- bg2[c(3,4,13:18,23,24,31:34,5,6),]
bg2$dup <- rep(c('a', 'b'), times = 8)
bg2$cm2 <- paste0(bg2$cm, bg2$dup)
bg3 <- bg2[,c(1:6)]
mbg<- sapply(bg3, as.integer)
rownames(mbg) <- bg2$cm2
mgbx <- mbg %>% as.data.frame()

colnames(mma)
mag <- mma[,c(3,6,7,8,11)]
magx <- mag %>% as.data.frame()
mlg <- mml[,c(3,6,7,8,11)]
mlgx <-  mlg %>% as.data.frame()

rank <- dfd %>% dplyr::select(c(15:18,43)) %>% unique(.)
rank2 <- rank[c(2,6:9,14,15,17),] 
rankx <- rank2 %>% rbind(rank2) %>% arrange(cm)
rankx$dup <- c('a', 'b','a', 'b','a', 'b','a', 'b','a', 'b','a', 'b','a',"b",'a', 'b')
rankx$cm2 <- paste0(rankx$cm, rankx$dup)
# rankxx <- rankx %>% dplyr::select(dom) %>%  as.data.frame()
row.names(rankx) <- rankx$cm2
# rankxx$dom <- as.factor(rankxx$dom)
# rankxx <- rankxx[c(1:13,15,16),]
rankxx <- rankx[,4]

#CCA
library(CCA)
X <- magx %>%scale()
Y <- mgbx %>% scale()

model2 <- cc(X, Y)
plt.cc(model2, var.label = TRUE)

plt.var(model2, 1, 3, var.label = TRUE)
# suggest to take growth hormone releasing factor out of brain


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
  ggplot(aes(x=dom,y=CC1_X, color=dom))+
  geom_boxplot(width=0.5)+
  geom_jitter(width = 0.15) +
  theme(legend.position="non")+
  theme_bw() +
  ggtitle('Orexigenic: ARC + Blood')



cca_df %>% 
  ggplot(aes(x=CC1_X,y=CC1_Y, color=dom))+
  geom_point() +theme_bw() 




library(GGally)
ggpairs(magx)
ggpairs(mgbx)
matcor(magx, mgbx)
cc1 <- cc(magx, mgbx)

# display the canonical correlations
cc1$cor
cc1[3:4]

# compute canonical loadings
cc2 <- comput(magx, mgbx, cc1)

# display canonical loadings
cc2[3:6]


# tests of canonical dimensions
rho <- cc1$cor
## Define number of observations, number of variables in first set, and number of variables in the second set.
n <- dim(magx)[1]
p <- length(magx)
q <- length(mgbx)

## Calculate p-values using the F-approximations of different test statistics:
CCP::p.asym(rho, n, p, q, tstat = "Wilks")

s1 <- diag(sqrt(diag(cov(magx))))
CC1_X <- s1 %*% cc1$xcoef
s2 <- diag(sqrt(diag(cov(mgbx))))
CC1_Y <- s2 %*% cc1$ycoef

ggpairs(CC1_X,CC1_Y)


library(MASS)

x <- rankxx %>% cbind(magx,mgbx) 
#scale each predictor variable (i.e. first 4 columns)
x[2:6] <- scale(x[2:6])
#find mean of each predictor variable
apply(x[2:6], 2, mean)

#find standard deviation of each predictor variable
apply(x[2:6], 2, sd) 
#make this example reproducible
set.seed(1)

#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(x), replace=TRUE, prob=c(0.7,0.3))
train <- x[sample, ]
test <- x[!sample, ] 

#fit LDA model
model <- lda(dom~., data=train)

#view model output
model


predicted <- predict(model, test)

names(predicted)
head(predicted$class)
head(predicted$x)
head(predicted$posterior)


mean(predicted$class==test$Species)

