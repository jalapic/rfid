setwd("C:/Users/tmmil/Downloads")

library(tidyverse)

temp <- list.files(pattern="*.csv")
xfiles <- lapply(temp, function(x) read.csv(paste0(x)) )  
xfiles <- xfiles[-1]
lapply(xfiles, head)
lapply(xfiles, colnames)


# Read in behaviors file
behavs <- read.csv("behavs.csv")
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

# get winner/loser
get_wl <- function(df){
  df$winner <- ifelse(df$score==0, df$Animal.2, df$Animal.1)
  df$loser <- ifelse(df$score==0, df$Animal.1, df$Animal.2)
  return(df)
}

xfiles <- lapply(xfiles, get_wl)
lapply(xfiles, head)


# only keep time, cage, winner, loser, score
lapply(xfiles, colnames)
xfiles1 <- lapply(xfiles, function(x) x[,c('ts','Cage','winner','loser','score')])
lapply(xfiles1, head)
lapply(xfiles1, tail)


# Fix score
# doing in loop as doesn't seem to work with lapply, don't know why
## I am not sure what this does ?
for(i in 1:length(xfiles1)){
  xfiles1[[i]]$score <- ifelse(myfiles1[[i]]$score==0.5, 0.5, 1)
}

head(xfiles1[[1]])

df1 <- xfiles1 %>% map2_df(.,names(.), ~mutate(.x, cohort = .y))
head(df1)

library(compete)

get_matrix <- function(df){
  df <-df[df$score==1,]
  mat <-compete::org_matrix(compete::get_wl_matrix(df[,c('winner','loser')]),
                            method = "ds")
  return(mat)
}

mats <- lapply(xfiles1, get_matrix)
mats

dscores <- lapply(mats, compete::ds)
dscores

## gilcko 
library(PlayerRatings)
x <- xfiles[[1]]


df1 <- x[order(x$ts),] #ensure in date order
df1$event <- 1:nrow(df1)
glick.df <- df1[, c(15,13,14,11)] #need event, actor, recipient, score
gl <- glicko(glick.df, history=T, cval=2)
gl

plot(gl,npl=12)


x <- xfiles[[2]]
df1 <- x[order(x$ts),] #ensure in date order
df1$event <- 1:nrow(df1)
glick.df <- df1[, c(15,13,14,11)] #need event, actor, recipient, score
gl <- glicko(glick.df, history=T, cval=2)
gl

plot(gl,npl=12)

