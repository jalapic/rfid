#sources
act <- readRDS("RFID_stable_cohorts/data_clean/activity.RDS")
rank <- read_csv("RFID_stable_cohorts/data_clean/socialbehavior/rank_data.csv")
rank$dom <- ifelse(rank$glicko_rank == 1, "Dominant", rank$glicko_rank)
rank$dom <- ifelse(rank$glicko_rank == 2, "Subdominant", rank$dom)
rank$dom <- ifelse(rank$glicko_rank == 6, "Subordinate", rank$dom)

rank$id <- as.character(rank$id)
colnames(rank)[3]<- "mouse"


help <- act %>% select(cohort, mouse, zone, ms)

l <- help %>% split(.$cohort)

al <- l %>% map(~group_by(., mouse)) %>% 
  map(~mutate(.,start = ms)) %>% 
  map(~mutate(.,end1 = lead(start))) %>% 
  map(~mutate(.,end = coalesce(end1, start))) %>% 
  map(~select(.,cohort, mouse, zone, ms, start, end))

lapply(al, head)
lapply(al, tail)


a_df <- do.call(rbind, al)

rank$mouse <- as.numeric(rank$mouse)

asx <- a_df%>% full_join(rank)
head(asx)
asx <- asx %>% select(cohort, mouse, zone, ms, start,end, glicko_rank,dom)

#saveRDS(asx, "RFID_stable_cohorts/data_clean/axs_corrected.rds")

c4p <- a_df %>% filter(cohort == 1 & ms < 6000000)
c4p

p <- ggplot() + 
  geom_segment(data=c4p, aes(x=start, xend=end, y=zone, yend=zone), size=15, color="navy") +
  theme_classic() +
  xlab("time since start (ms)") +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_y_continuous(breaks=seq(1,10,by=1))+ 
  facet_wrap(~mouse)
p


a_df %>% 
  filter(cohort == 4 & ms < 200000) %>%
ggplot() + 
  geom_segment(aes(x=start, xend=end, y=zone, yend=zone,
                   color = zone), 
               size=15) +
  theme_classic() +
  xlab("time since start (ms)") +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_y_continuous(breaks=seq(1,10,by=1))+ 
  facet_wrap(~mouse)




########################

## working out associations.

a_df_l <- split(a_df, a_df$cohort)
df <- a_df_l[[8]]
head(df)
range(df$ms)
range(df$zone)


### Trying to get DF of 'together' for mouse 1 and 2....


dfx <- df %>% filter(mouse==1  | mouse==2)


# get overlaps
get_overlaps <- function(df, id1=1, id2=2){
  DT = data.table(df)
  setkey(DT, start, end)
  oDT0 = foverlaps(DT[mouse==id1], DT[mouse==id2])
  oDT0[, `:=`(
    ostart = pmax(start, i.start),
    oend = pmin(end, i.end)
  )]
  oDT = oDT0[ostart < oend]
  return(oDT)
}


# together
together <- function(df){
  oDT <- get_overlaps(df)
  oDT[zone == i.zone, .(ids = '1-2', zone, ostart, oend)]
}

# together1
together1 <- function(df, ids='1-2'){
  oDT <- get_overlaps(df)
  oDT[zone == i.zone, .(ids, zone, ostart, oend)]
}

# # apart
# apart <- function(df){
#   oDT <- get_overlaps(df)
#   oDTapt <- oDT[zone != i.zone, .(mouse, zone, i.mouse, i.zone, ostart, oend)]
#   return(rbindlist(list(oDTapt[,c(1,2,5,6)],oDTapt[,3:6])))
# }
# 

### Example:
xx <- together(dfx)
together1(dfx)

xx$time <- xx$oend - xx$ostart
xx
sum(xx$time)
max(df$ms) - min(df$ms)

sum(xx$time) / (max(df$ms) - min(df$ms))


### Try to make above a function


table(df$mouse)

together_pct <- function(df, idA=NULL, idB=NULL){
# idA=1
# idB=2
dfx <- df %>% filter(mouse==idA  | mouse==idB)
oDT <- get_overlaps(dfx, id1=idA, id2=idB)
ids = paste(idA,idB,sep="-")
xx <- oDT[zone == i.zone, .(ids, zone, ostart, oend)]
xx$time <- xx$oend - xx$ostart
out <- sum(xx$time) / (max(df$ms) - min(df$ms))
return(out)
}

together_pct(df, idA=1, idB=2)
together_pct(df, idA=1, idB=3)
together_pct(df, idA=1, idB=5)
together_pct(df, idA=1, idB=6)
together_pct(df, idA=1, idB=7)

### doing this for all combinations of mice in cohort 8 ..

mice <- unique(df$mouse)
mat <- matrix(nrow=length(mice),ncol=length(mice), NA)
for(i in 1:length(mice)){
  for(j in 1:length(mice)){
mat[i,j]  <-  together_pct(df, idA=mice[[i]], idB=mice[[j]])
  }
}

mat
colnames(mat)<-rownames(mat)<-mice
round(mat,2)
diag(mat)<-NA
apply(mat,2,mean,na.rm=T)


### Put the above into a function:

mat_assoc <- function(df){

mice <- unique(df$mouse)
mat <- matrix(nrow=length(mice),ncol=length(mice), NA)
for(i in 1:length(mice)){
  for(j in 1:length(mice)){
    mat[i,j]  <-  together_pct(df, idA=mice[[i]], idB=mice[[j]])
  }
}

colnames(mat)<-rownames(mat)<-mice
mat <- round(mat,2)
diag(mat)<-NA
return(mat)
}


a_df_l <- split(a_df, a_df$cohort)

all_mats <- lapply(a_df_l, mat_assoc)

all_mats

### plotting average association by each animal
asscs <- lapply(all_mats, function(x) apply(x, 2, mean, na.rm=T))

asscs

fun1 <- function(x){
df<-  data.frame(x)
df$mouse <- rownames(df)
colnames(df)[1]<-"assoc"
return(df)
}

assoc.df <- data.table::rbindlist(Map(cbind, lapply(asscs,fun1),cohort=1:8))
assoc.df$mouse <- as.numeric(assoc.df$mouse)
assoc.df <- full_join(rank, assoc.df) %>% filter(cohort!=2)
assoc.df <- assoc.df[!is.na(assoc.df$ds_rank),] #some NAs in there for some reason

ggplot(assoc.df, aes(x=factor(ds_rank), 
                     y=assoc, 
                     color=factor(ds_rank),
                     fill=factor(ds_rank)))+
  geom_boxplot(alpha=.5)+
  geom_jitter(width=.05,alpha=.7)+
  theme_classic() +
  theme(legend.position = 'none')  +
  xlab("Rank") +
  ylab("Mean Percentage Association")




