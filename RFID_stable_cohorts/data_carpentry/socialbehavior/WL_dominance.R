## just get clean win /loss data 
library(compete)
library(PlayerRatings)
library(tidyverse)

## Read in Data

temp <- list.files(path="RFID_stable_cohorts/data_raw/social_behavior/",pattern="*.csv")
xfiles <- lapply(temp, function(x) read.csv(paste0("RFID_stable_cohorts/data_raw/social_behavior/",x)) )                 
lapply(xfiles, head)
lapply(xfiles, colnames)

lapply(xfiles, head)

lapply(xfiles, tail)


# Read in behaviors file
behavs <- read.csv("RFID_stable_cohorts/data_carpentry/socialbehavior/behavs.csv")
head(behavs)


# Check have all behaviors

## Get all Behavioral Combinations
behavsX <-
  unique(
    unlist(
      lapply(xfiles, 
             function(x) 
               paste(x$Animal.1..Behavior, x$Animal.2..Behavior, sep=" --- "))
    )
  )



behavsX %in% behavs$behavior # if have any FALSE need to update 'behavs' file.

# ##code for updating behaviors
# 
# newbehavs <- behavsX[!(behavsX %in% behavs$behavior)]
# newbehavs<- newbehavs[order(newbehavs)]
# newbehavs <- as.data.frame(newbehavs)
# colnames(newbehavs)<-"behavior"
# write.csv(newbehavs, "RFID_stable_cohorts/data_carpentry/socialbehavior/newbehavs.csv", row.names=F)

# Add in score column.... X = scores in behavs 
add_score <- function(df){
  df$behav <-  paste(df$Animal.1..Behavior, df$Animal.2..Behavior, sep=" --- ")
  df$score <- behavs$X[match(df$behav, behavs$behavior)]
  return(df)
}



xfiles <- lapply(xfiles, add_score)
lapply(xfiles, head)



# step 1.  Get Timestamp correct.
# note some datasets have seconds, others do not.
get_times <- function(dataf){
  dataf$ts <- strptime(as.character(dataf$Timestamp),'%m/%d/%Y %H:%M:%S')
  dataf <- dataf[order(dataf$ts),]
  return(dataf)
}

xfiles <- lapply(xfiles, get_times)
lapply(xfiles, head)


# Remove Starts,Ends,NA, when id1==id2  # Remove Starts,Ends,NA, when id1==id2
clean_data <- function(df){
  df <- df[!is.na(df$score),]
  df <- df[df$Start.End!="Start",]
  df <- df[df$Start.End!="End",]
  df <- df[df$Animal.1!=df$Animal.2,]
}

xfiles <- lapply(xfiles, clean_data)

lapply(xfiles, head)
lapply(xfiles,tail)
# get winner/loser
get_wl <- function(df){
  df$winner <- ifelse(df$score==0, df$Animal.2, df$Animal.1)
  df$loser <- ifelse(df$score==0, df$Animal.1, df$Animal.2)
  return(df)
}

xfiles <- lapply(xfiles, get_wl)
lapply(xfiles, head)
lapply(xfiles,tail)

# only keep time, cage, winner, loser, score
lapply(xfiles, colnames)
xfiles1 <- lapply(xfiles, function(x) x[,c('ts','Cage','winner','loser','score')])
lapply(xfiles1, head)
lapply(xfiles1, tail)


# Fix score
# doing in loop as doesn't seem to work with lapply, don't know why
## I am not sure what this does ?
for(i in 1:length(xfiles1)){
  xfiles1[[i]]$score <- ifelse(xfiles1[[i]]$score==0.5, 0.5, 1)
}

head(xfiles1[[1]])

# Make into large data frame
dfnames <- c(1,10,2:9)
wl_df<-xfiles1 %>% map2_df(.,dfnames, ~mutate(.x, cohort = .y)) 
head(wl_df)
tail(wl_df)
### add time

# library(lubridate)
# wl_df<- wl_df %>% 
#   mutate(date = date(wl_df$ts)) %>% 
#   mutate(hrs = hour(wl_df$ts)) %>% 
#   mutate(min = minute(wl_df$ts)) %>% 
#   mutate(sec = second(wl_df$ts)) %>% 
#   # add dates and times and remove timestamp
# head(wl_df)

write.csv(wl_df, "RFID_stable_cohorts/data_clean/socialbehavior/WL_data.csv", row.names=F)


#David scores

get_matrix <- function(df){
  df <-df[df$score==1,]
  mat <-compete::org_matrix(compete::get_wl_matrix(df[,c('winner','loser')]),
                            method = "ds")
  return(mat)
}

df <- read_csv("RFID_stable_cohorts/data_clean/socialbehavior/WL_data.csv")
head(df)

df$cohort <- factor(df$cohort, levels = c(1:10))
df.groupx <- df %>% split(.$cohort)

df.groupxx <- df.groupx %>% 
  map(~ select(., winner,loser,score)) %>% 
  map(~ as.data.frame(.))

mats <- df.groupxx %>% 
  map(get_matrix)

## David's Scores....

dscores <- lapply(mats, compete::ds)


dsdf <- as.data.frame(unlist(dscores))
dsdf$cohort <- substr(rownames(dsdf),1,2)
dsdf$cohort <- gsub("\\.", "", dsdf$cohort)
dsdf$id <- substr(rownames(dsdf),3,4)
dsdf$id <- gsub("\\.", "", dsdf$id)
colnames(dsdf)[1]<-"dscore"


dsdf <- dsdf %>% group_by(cohort) %>% mutate(rank = min_rank(-dscore))

head(dsdf)


#WL mats
mats

bimat <- lapply(mats, get_di_matrix)
bimat

matrixplot <- function(m, mylevs=NULL, lowcolor="white",highcolor="red1"){

  library(ggplot2)

  #make the df we will use for plotting
  m.dat <- reshape2::melt(m)
  m.dat <- data.frame(m.dat)
  m.dat <- m.dat[complete.cases(m.dat),] #removing NAs

  if(is.null(mylevs)) { mylevs = rownames(m)}

  #reorder the levels of the y-axis so plots properly
  m.dat$loser <- factor(m.dat$loser, levels=mylevs)
  m.dat$winner <- factor(m.dat$winner, levels = rev(mylevs))
  m.dat[m.dat == 0] <- NA


  #plot
  p1<-ggplot(m.dat, aes(loser, winner, fill = value)) +
    geom_tile(colour="black",
              size=0.5, stat="identity") +
    geom_text(data=m.dat, aes(loser, winner, label = value), color="black", size=5) +
    scale_fill_gradient(low = lowcolor, high = highcolor, space = "Lab",
                        na.value = "white", guide = "colourbar") +
    scale_x_discrete(expand = c(0, 0), position = "top") +
    scale_y_discrete(expand = c(0, 0)) +
    xlab("Loser") +
    ylab("Winner") +
    theme(axis.text.x = element_text(vjust = 1),
          axis.text.y = element_text(hjust = 0.5),
          text = element_text(size = 20),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill="white"),
          plot.background = element_rect(fill="white"),
          axis.text = element_text(color="#3C3C3C", size=rel(1.1)),
          legend.position = "none"
    )
  return(p1)
}

mat_plots <- lapply(mats, matrixplot)
mat <- egg::ggarrange(plots=mat_plots, widths = c(1,1), labels = c(1:10), 
                        label.args = list(gp = grid::gpar(font = 3, cex =2.5)))

ggsave("RFID_stable_cohorts/imgs/WL_mats.png", mat, height =18, width = 9, dpi = 300)

mat_plots <- lapply(bimat, matrixplot)
bimat <- egg::ggarrange(plots=mat_plots, widths = c(1,1), labels = c(1:10), 
                        label.args = list(gp = grid::gpar(font = 3, cex =2.5)))
ggsave("RFID_stable_cohorts/imgs/bimats.png", bimat, height =18, width = 9, dpi = 300)

## WITHOUT COHORT 6. 

m9 <- mats[c(1:5,7:10)]

bimat9 <- lapply(m9, get_di_matrix)
bimat9

mat_p9 <- lapply(m9, matrixplot)
mat9 <- egg::ggarrange(plots=mat_p9, labels = c(1:9),nrow = 3,  ncol = 3,  
                      label.args = list(gp = grid::gpar(font = 3, cex =2.5)))
ggsave("RFID_stable_cohorts/imgs/WL_mats_withoutcohort6.png", mat9, height =10, width =10 , dpi = 300)

mat_plots <- lapply(bimat9, matrixplot)
bimat <- egg::ggarrange(plots=mat_plots, labels = c(1:9), nrow = 3,  ncol = 3, 
                        label.args = list(gp = grid::gpar(font = 3, cex =2.5)))
ggsave("RFID_stable_cohorts/imgs/bimats_withoutcohort6.png", bimat, height =10, width = 10, dpi = 300)



#glicko
run_glicko <-
  function(x){
    x$event <- 1:nrow(x)
    x$winner <- as.character(x$winner)
    x$loser <- as.character(x$loser)
    x <- x[,c('event','winner','loser','score')]
    out <- PlayerRatings::glicko(x, history = T, cval = 1)
    return(out)
  }

pre.glicko <- lapply(xfiles1, run_glicko)

pre.glicko
length(pre.glicko)




#############
robj <- pre.glicko[[1]]

extract_ratings <- function(robj){
  x<-as.data.frame(unlist(robj$history))
  n<-sum(robj$ratings$Games)/2
  x.ratings<-x[,1:n]
  x.deviations<-x[,(1+n):(n+n)]
  #longform the data
  x.ratingsmelt<-reshape2::melt(x.ratings)
  ids<-rownames(x.ratings)       #to make id column
  x.ratingsmelt$ids<-rep(ids, n)  #making id column
  l.ids<-length(ids)
  x.ratingsmelt$event<-rep(1:n, each=l.ids)
  #add ranks
  xrn<-as.data.frame(x.ratings[n])
  colnames(xrn)<-c("finalrating")
  x.ratingsmelt$rank<-rank(-xrn$finalrating, ties.method="random")
  #make ids1 a factor with levels defined by rank
  x.ratingsmelt1 <- data.frame(ids=unique(x.ratingsmelt$ids),rank=unique(x.ratingsmelt$rank))
  x.ratingsmelt1 <- x.ratingsmelt1[order(x.ratingsmelt1$rank),]
  x.ratingsmelt$ids1 <- factor(x.ratingsmelt$ids,levels=x.ratingsmelt1$ids)
  return(x.ratingsmelt)
}

extract_ratings(pre.glicko[[1]])

### Do for all glickos.....
pre.glicko
pre.glicko.ratings <- lapply(pre.glicko, extract_ratings)

names(pre.glicko) <- c("1","10", "2", "3", "4", "5", "6", "7", "8", "9")
pre.glicko.ratings <- Map(cbind, pre.glicko.ratings, id = names(pre.glicko))


## Plot All.
glick.dt <- data.table::rbindlist(pre.glicko.ratings)
glick.dt

glick.dt <- glick.dt %>% filter(id != "6")

range(glick.dt$value)

mycolors=c("black", "grey", "orange", "red")
ltypes=c(1,2,3,1)
linewd=1
ylim1=1600
ylim2=3000

#need a final rank value - doing it with tidyverse seems fastest
library(tidyverse)
glick.dt %>% group_by(id) %>%
  filter(event==max(event)) %>%
  select(ids, finalrank = rank, id) -> finalranks

glick.dt <- glick.dt %>% full_join(finalranks)
glick.dt$finalrank <- as.character(glick.dt$finalrank) # won't allow a number for linetype
glick.dt$id <- factor(glick.dt$id, levels = c(1:10))


glicko_p <- ggplot(glick.dt, aes(x = event, y = value, col=finalrank, linetype=finalrank))+
  geom_line(lwd=1) +
  scale_color_manual(values = c("firebrick2", "darkorange3", "chocolate2","goldenrod2", "darkgray","black"))+
  facet_wrap(~id, ncol = 5)+
  ylab("Glicko Rating") +
  xlab("Event") +
  scale_y_continuous(breaks = c(seq(1600,2800,400)))+
  theme(plot.title = element_text(hjust = 0, vjust = 1, size = rel(1.7)),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid.major.y = element_line(color = "gray75",linetype = 'dotted'),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(size=rel(1.1)),
        text = element_text(color="gray20", size=15),
        axis.text = element_text(size=rel(1.0)),
        axis.text.x = element_text(color="gray20", size=rel(1.0)),
        axis.text.y = element_text(color="gray20", size=rel(1.0)),
        axis.title.x = element_text(size=rel(1.0), vjust=0),
        axis.title.y = element_text(size=rel(1.0), vjust=1),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")

ggsave("RFID_stable_cohorts/imgs/glicko_plots_without6.png", glicko_p, height = 12, width = 12, dpi = 300)





### Do for all glickos with out cohort 6 
pre.glicko <- pre.glicko[c(1:6,8:10)]
pre.glicko.ratings <- lapply(pre.glicko, extract_ratings)

names(pre.glicko) <- c("1","9", "2", "3", "4", "5", "6", "7", "8")
pre.glicko.ratings <- Map(cbind, pre.glicko.ratings, id = names(pre.glicko))


## Plot All.
glick.dt <- data.table::rbindlist(pre.glicko.ratings)
glick.dt


range(glick.dt$value)

mycolors=c("black", "grey", "orange", "red")
ltypes=c(1,2,3,1)
linewd=1
ylim1=1600
ylim2=3000

#need a final rank value - doing it with tidyverse seems fastest
library(tidyverse)
glick.dt %>% group_by(id) %>%
  filter(event==max(event)) %>%
  select(ids, finalrank = rank, id) -> finalranks

glick.dt <- glick.dt %>% full_join(finalranks)
glick.dt$finalrank <- as.character(glick.dt$finalrank) # won't allow a number for linetype
glick.dt$id <- factor(glick.dt$id, levels = c(1:9))


glicko_p <- ggplot(glick.dt, aes(x = event, y = value, col=finalrank, linetype=finalrank))+
  geom_line(lwd=1) +
  scale_color_manual(values = c("firebrick2", "darkorange3", "chocolate2","goldenrod2", "darkgray","black"))+
  facet_wrap(~id, ncol = 3)+
  ylab("Glicko Rating") +
  xlab("Event") +
  scale_y_continuous(breaks = c(seq(1600,2800,400)))+
  theme(plot.title = element_text(hjust = 0, vjust = 1, size = rel(1.7)),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid.major.y = element_line(color = "gray75",linetype = 'dotted'),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(size=rel(1.1)),
        text = element_text(color="gray20", size=15),
        axis.text = element_text(size=rel(1.0)),
        axis.text.x = element_text(color="gray20", size=rel(1.0)),
        axis.text.y = element_text(color="gray20", size=rel(1.0)),
        axis.title.x = element_text(size=rel(1.0), vjust=0),
        axis.title.y = element_text(size=rel(1.0), vjust=1),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")

ggsave("RFID_stable_cohorts/imgs/glicko_plots_without6.png", glicko_p, height = 12, width = 12, dpi = 300)





