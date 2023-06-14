## load packages
library(tidyverse)
library(gridExtra)

## load rawdata
sep27 <- read.table("Cohorts1_2/rfidtracking_data/rawdata20210927.csv", 
  sep=";", header=TRUE)
sep27$data <- as.character(sep27$data)
sep27$date <- format(as.POSIXct(sep27$datetimestamp,format="%d.%m.%Y %H:%M:%S:%OS"),"%m.%d.%Y")
sep27$time <- sub("^\\S+\\s+", '', sep27$datetimestamp)

sep28 <- read.table("Cohorts1_2/rfidtracking_data/rawdata20210928.csv", 
  sep=";", header=TRUE)
sep28$data <- as.character(sep28$data)
sep28$date <- format(as.POSIXct(sep28$datetimestamp,format="%d.%m.%Y %H:%M:%S:%OS"),"%m.%d.%Y")
sep28$time <- sub("^\\S+\\s+", '', sep28$datetimestamp)

sep29 <- read.table("Cohorts1_2/rfidtracking_data/rawdata20210929.csv", 
  sep=";", header=TRUE)
sep29$data <- as.character(sep29$data)
sep29$date <- format(as.POSIXct(sep29$datetimestamp,format="%d.%m.%Y %H:%M:%S:%OS"),"%m.%d.%Y")
sep29$time <- sub("^\\S+\\s+", '', sep29$datetimestamp)


## load id data
id_data <- read_csv("Cohorts1_2/id_data.csv")

id_data$back_tag <- as.character(id_data$back_tag)
id_data$leg_tag <- as.character(id_data$leg_tag)


## create ms column and label mice/cohorts
dfx <- data.frame(data=c("900133000459712", "900133000459713",
                         "900133000459701", "900133000459702",
                         "900133000459703", "900133000459704",
                         "900133000459649", "900133000459650",
                         "900133000459647", "900133000459648",
                         "900133000459631", "900133000459632",
                         "900133000459633", "900133000459634",
                         "900133000459635", "900133000459636",
                         "900133000459637", "900133000459638",
                         "900133000459639", "900133000459640",
                         "900133000459642", "900133000459643",
                         "900133000459644", "900133000459645"),
                mouse = c(1,1,1,1,3,3,3,3,4,4,4,4,6,6,6,6,7,7,7,7,8,8,8,8),
                cohort = c(1,1,2,2,2,2,1,1,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2)
                )
write.csv(dfx,"Cohorts1_2/mouseids.csv", row.names = F)
dfx <- read_csv("Cohorts1_2/mouseids.csv")
dfx$data <- as.character(dfx$data)

df <- rbind(sep27,
            sep28,
            sep29)

xx <- strsplit(df$time, split=":")
xxx <- matrix(unlist(xx), ncol = 4, byrow = TRUE)
df$ms<-
  (3600000 * as.numeric(xxx[,1])) +
  (60000 * as.numeric(xxx[,2])) +
  (1000 * as.numeric(xxx[,3])) +
  (1 * as.numeric(xxx[,4]))

day_1 <- df %>% filter(date == "09.27.2021")

day_1$ms <- as.numeric(unlist(day_1$ms))

day_2 <- df %>% filter(date == "09.28.2021")

day_2$ms <- (as.numeric(unlist(day_2$ms)) + (8.64e+7 * 1))

day_3 <- df %>% filter(date == "09.29.2021")

day_3$ms <- (as.numeric(unlist(day_3$ms)) + (8.64e+7 * 2))

df <- rbind(day_1,
            day_2,
            day_3)

df$ms <- df$ms - 39124161
df$hour <- sub("\\:.*:.*:.*", "", df$time) 

m1.1 <- subset(df, data %in% dfx[dfx$mouse==1,]$data & data %in% dfx[dfx$cohort==1,]$data) %>% 
  mutate(mouse = 1, cohort = 1)
m1.3 <- subset(df, data %in% dfx[dfx$mouse==3,]$data & data %in% dfx[dfx$cohort==1,]$data) %>% 
  mutate(mouse = 3, cohort = 1)
m1.4 <- subset(df, data %in% dfx[dfx$mouse==4,]$data & data %in% dfx[dfx$cohort==1,]$data) %>% 
  mutate(mouse = 4, cohort = 1)
m1.6 <- subset(df, data %in% dfx[dfx$mouse==6,]$data & data %in% dfx[dfx$cohort==1,]$data) %>% 
  mutate(mouse = 6, cohort = 1)
m1.7 <- subset(df, data %in% dfx[dfx$mouse==7,]$data & data %in% dfx[dfx$cohort==1,]$data) %>% 
  mutate(mouse = 7, cohort = 1)
m1.8 <- subset(df, data %in% dfx[dfx$mouse==8,]$data & data %in% dfx[dfx$cohort==1,]$data) %>% 
  mutate(mouse = 8, cohort = 1)


m2.1 <- subset(df, data %in% dfx[dfx$mouse==1,]$data & data %in% dfx[dfx$cohort==2,]$data) %>% 
  mutate(mouse = 1, cohort = 2)
m2.3 <- subset(df, data %in% dfx[dfx$mouse==3,]$data & data %in% dfx[dfx$cohort==2,]$data) %>% 
  mutate(mouse = 3, cohort = 2)
m2.4 <- subset(df, data %in% dfx[dfx$mouse==4,]$data & data %in% dfx[dfx$cohort==2,]$data) %>% 
  mutate(mouse = 4, cohort = 2)
m2.6 <- subset(df, data %in% dfx[dfx$mouse==6,]$data & data %in% dfx[dfx$cohort==2,]$data) %>% 
  mutate(mouse = 6, cohort = 2)
m2.7 <- subset(df, data %in% dfx[dfx$mouse==7,]$data & data %in% dfx[dfx$cohort==2,]$data) %>% 
  mutate(mouse = 7, cohort = 2)
m2.8 <- subset(df, data %in% dfx[dfx$mouse==8,]$data & data %in% dfx[dfx$cohort==2,]$data) %>% 
  mutate(mouse = 8, cohort = 2)

## tube error functions
tube_errors <- function(df, pair1=c(1,2), pair2=c(3,4), pair3=c(5,6), pair4=c(7,8),
                        pair5=c(9,10), pair6=c(11,12), pair7=c(13,14), pair8=c(15,16)){
    
    ids <- c(pair1,pair2,pair3,pair4,pair5,pair6,pair7,pair8)
    codes <- c("1", "2","3", "4", "8","9")
    
    x <- codes[match(df$deviceid, ids)]
    
    row.inds <-
      c(intersect(which(x == "1"), which(lag(x) == "3")),
        intersect(which(x == "3"), which(lag(x) == "1")),
        intersect(which(x == "1"), which(lag(x) == "4")),
        intersect(which(x == "4"), which(lag(x) == "1")),
        intersect(which(x == "1"), which(lag(x) == "8")),
        intersect(which(x == "8"), which(lag(x) == "1")),
        intersect(which(x == "1"), which(lag(x) == "9")),
        intersect(which(x == "9"), which(lag(x) == "1")),
        intersect(which(x == "2"), which(lag(x) == "4")),
        intersect(which(x == "4"), which(lag(x) == "2")),
        intersect(which(x == "2"), which(lag(x) == "9")),
        intersect(which(x == "9"), which(lag(x) == "2")),
        intersect(which(x == "3"), which(lag(x) == "9")),
        intersect(which(x == "9"), which(lag(x) == "3")),
        intersect(which(x == "4"), which(lag(x) == "8")),
        intersect(which(x == "8"), which(lag(x) == "4"))
      )
    
    df$error <- FALSE
    df[row.inds,"error"]<-TRUE
    
    
    return(df)
  }


tube_errors2 <- function(df, pair1=c(1,2), pair2=c(3,4), pair3=c(5,6), pair4=c(7,8),
                        pair5=c(9,10), pair6=c(11,12), pair7=c(13,14), pair8=c(15,16)){
  
  ids <- c(pair1,pair2,pair3,pair4,pair5,pair6,pair7,pair8)
  codes <- c("19", "18","17", "16", "20","21")
  
  x <- codes[match(df$deviceid, ids)]
  
  row.inds <-
    c(intersect(which(x == "19"), which(lag(x) == "17")),
      intersect(which(x == "17"), which(lag(x) == "19")),
      intersect(which(x == "19"), which(lag(x) == "16")),
      intersect(which(x == "16"), which(lag(x) == "19")),
      intersect(which(x == "19"), which(lag(x) == "21")),
      intersect(which(x == "21"), which(lag(x) == "19")),
      intersect(which(x == "19"), which(lag(x) == "20")),
      intersect(which(x == "20"), which(lag(x) == "19")),
      intersect(which(x == "18"), which(lag(x) == "16")),
      intersect(which(x == "16"), which(lag(x) == "18")),
      intersect(which(x == "18"),  which(lag(x) == "20")),
      intersect(which(x == "20"), which(lag(x) == "18")),
      intersect(which(x == "17"), which(lag(x) == "20")),
      intersect(which(x == "20"), which(lag(x) == "17")),
      intersect(which(x == "16"), which(lag(x) == "21")),
      intersect(which(x == "21"), which(lag(x) == "16"))
    )
  
  df$error <- FALSE
  df[row.inds,"error"]<-TRUE
  
  
  return(df)
  }

## looking at tube errors
m1.1e <- as.data.frame(tube_errors(m1.1, pair1 = c(1,3), pair2 = c(1,4), pair3 = c(1,8), 
                                   pair4 = c(1,9), pair5 = c(2,4), pair6 = c(2,9), pair7 = c(3,9),
                                   pair8 = c(4,8))) %>% filter(error==TRUE)
m1.3e <- as.data.frame(tube_errors(m1.3, pair1 = c(1,3), pair2 = c(1,4), pair3 = c(1,8), 
                                   pair4 = c(1,9), pair5 = c(2,4), pair6 = c(2,9), pair7 = c(3,9),
                                    pair8 = c(4,8))) %>% filter(error==TRUE)
m1.4e <- as.data.frame(tube_errors(m1.4, pair1 = c(1,3), pair2 = c(1,4), pair3 = c(1,8), 
                                  pair4 = c(1,9), pair5 = c(2,4), pair6 = c(2,9), pair7 = c(3,9),
                                  pair8 = c(4,8))) %>% filter(error==TRUE)
m1.6e <- as.data.frame(tube_errors(m1.6, pair1 = c(1,3), pair2 = c(1,4), pair3 = c(1,8), 
                                  pair4 = c(1,9), pair5 = c(2,4), pair6 = c(2,9), pair7 = c(3,9),
                                  pair8 = c(4,8))) %>% filter(error==TRUE)
m1.7e <- as.data.frame(tube_errors(m1.7, pair1 = c(1,3), pair2 = c(1,4), pair3 = c(1,8), 
                                   pair4 = c(1,9), pair5 = c(2,4), pair6 = c(2,9), pair7 = c(3,9),
                                   pair8 = c(4,8))) %>% filter(error==TRUE)
m1.8e <- as.data.frame(tube_errors(m1.8, pair1 = c(1,3), pair2 = c(1,4), pair3 = c(1,8), 
                                   pair4 = c(1,9), pair5 = c(2,4), pair6 = c(2,9), pair7 = c(3,9),
                                   pair8 = c(4,8))) %>% filter(error==TRUE)

m1.1 <- as.data.frame(tube_errors(m1.1, pair1 = c(1,3), pair2 = c(1,4), pair3 = c(1,8), 
                                   pair4 = c(1,9), pair5 = c(2,4), pair6 = c(2,9), pair7 = c(3,9),
                                   pair8 = c(4,8))) 
m1.3 <- as.data.frame(tube_errors(m1.3, pair1 = c(1,3), pair2 = c(1,4), pair3 = c(1,8), 
                                   pair4 = c(1,9), pair5 = c(2,4), pair6 = c(2,9), pair7 = c(3,9),
                                   pair8 = c(4,8))) 
m1.4 <- as.data.frame(tube_errors(m1.4, pair1 = c(1,3), pair2 = c(1,4), pair3 = c(1,8), 
                                   pair4 = c(1,9), pair5 = c(2,4), pair6 = c(2,9), pair7 = c(3,9),
                                   pair8 = c(4,8))) 
m1.6 <- as.data.frame(tube_errors(m1.6, pair1 = c(1,3), pair2 = c(1,4), pair3 = c(1,8), 
                                   pair4 = c(1,9), pair5 = c(2,4), pair6 = c(2,9), pair7 = c(3,9),
                                   pair8 = c(4,8))) 
m1.7 <- as.data.frame(tube_errors(m1.7, pair1 = c(1,3), pair2 = c(1,4), pair3 = c(1,8), 
                                   pair4 = c(1,9), pair5 = c(2,4), pair6 = c(2,9), pair7 = c(3,9),
                                   pair8 = c(4,8)))
m1.8 <- as.data.frame(tube_errors(m1.8, pair1 = c(1,3), pair2 = c(1,4), pair3 = c(1,8), 
                                   pair4 = c(1,9), pair5 = c(2,4), pair6 = c(2,9), pair7 = c(3,9),
                                   pair8 = c(4,8))) 


m2.1e <- as.data.frame(tube_errors2(m2.1, pair1 = c(19,17), pair2 = c(19,21), pair3 = c(19,16), 
                                    pair4 = c(19,20), pair5 = c(18,16), pair6 = c(18,20), pair7 = c(17,20),
                                    pair8 = c(21,16))) %>% filter(error==TRUE)
m2.3e <- as.data.frame(tube_errors2(m2.3, pair1 = c(19,17), pair2 = c(19,21), pair3 = c(19,16), 
                                    pair4 = c(19,20), pair5 = c(18,16), pair6 = c(18,20), pair7 = c(17,20),
                                    pair8 = c(21,16))) %>% filter(error==TRUE)
m2.4e <- as.data.frame(tube_errors2(m2.4, pair1 = c(19,17), pair2 = c(19,21), pair3 = c(19,16), 
                                    pair4 = c(19,20), pair5 = c(18,16), pair6 = c(18,20), pair7 = c(17,20),
                                    pair8 = c(21,16))) %>% filter(error==TRUE)
m2.6e <- as.data.frame(tube_errors2(m2.6, pair1 = c(19,17), pair2 = c(19,21), pair3 = c(19,16), 
                                    pair4 = c(19,20), pair5 = c(18,16), pair6 = c(18,20), pair7 = c(17,20),
                                    pair8 = c(21,16))) %>% filter(error==TRUE)
m2.7e <- as.data.frame(tube_errors2(m2.7, pair1 = c(19,17), pair2 = c(19,21), pair3 = c(19,16), 
                                    pair4 = c(19,20), pair5 = c(18,16), pair6 = c(18,20), pair7 = c(17,20),
                                    pair8 = c(21,16))) %>% filter(error==TRUE)
m2.8e <- as.data.frame(tube_errors2(m2.8, pair1 = c(19,17), pair2 = c(19,21), pair3 = c(19,16), 
                                    pair4 = c(19,20), pair5 = c(18,16), pair6 = c(18,20), pair7 = c(17,20),
                                    pair8 = c(21,16))) %>% filter(error==TRUE)


m2.1 <- as.data.frame(tube_errors2(m2.1, pair1 = c(19,17), pair2 = c(19,21), pair3 = c(19,16), 
                                    pair4 = c(19,20), pair5 = c(18,16), pair6 = c(18,20), pair7 = c(17,20),
                                    pair8 = c(21,16))) 
m2.3 <- as.data.frame(tube_errors2(m2.3, pair1 = c(19,17), pair2 = c(19,21), pair3 = c(19,16), 
                                    pair4 = c(19,20), pair5 = c(18,16), pair6 = c(18,20), pair7 = c(17,20),
                                    pair8 = c(21,16))) 
m2.4 <- as.data.frame(tube_errors2(m2.4, pair1 = c(19,17), pair2 = c(19,21), pair3 = c(19,16), 
                                    pair4 = c(19,20), pair5 = c(18,16), pair6 = c(18,20), pair7 = c(17,20),
                                    pair8 = c(21,16))) 
m2.6 <- as.data.frame(tube_errors2(m2.6, pair1 = c(19,17), pair2 = c(19,21), pair3 = c(19,16), 
                                    pair4 = c(19,20), pair5 = c(18,16), pair6 = c(18,20), pair7 = c(17,20),
                                    pair8 = c(21,16))) 
m2.7 <- as.data.frame(tube_errors2(m2.7, pair1 = c(19,17), pair2 = c(19,21), pair3 = c(19,16), 
                                    pair4 = c(19,20), pair5 = c(18,16), pair6 = c(18,20), pair7 = c(17,20),
                                    pair8 = c(21,16))) 
m2.8 <- as.data.frame(tube_errors2(m2.8, pair1 = c(19,17), pair2 = c(19,21), pair3 = c(19,16), 
                                    pair4 = c(19,20), pair5 = c(18,16), pair6 = c(18,20), pair7 = c(17,20),
                                    pair8 = c(21,16))) 

## tube error plots
e_plot <- rbind(m1.1e, 
                m1.3e,
                m1.4e,
                m1.6e,
                m1.7e,
                m1.8e)

li <- strsplit(e_plot$datetimestamp,split=":")
hrs <- as.numeric(unlist(lapply(li, function(x) substr(x[[1]],12,13))))
mins <- as.numeric(unlist(lapply(li, function(x) x[2])))
secs <- as.numeric(unlist(lapply(li, function(x) x[3])))
ms <- as.numeric(unlist(lapply(li, function(x) x[4])))

time <- strptime(e_plot$datetimestamp, format="%d.%M.%Y %H:%M:%S:%OS")
times <- strftime(time, format="%H:%M:%S:%OS")

e_plot$times <- times
e_plot$hrs <- hrs
e_plot$mins <- mins
e_plot$bin <- ifelse(mins>30, paste0(e_plot$hrs,"b"), paste0(e_plot$hrs, "a"))
e_plot$totalmins <- (e_plot$hrs * 60) + e_plot$mins 

m1.eplot <- ggplot(e_plot, aes(x=totalmins)) + 
  geom_histogram(binwidth=1,fill='lightseagreen', color='darkgreen') +
  theme_classic()+
  xlab("Time of Day in Minutes") +
  geom_vline(xintercept=c(660, 1380), color='black', lwd=1,lty=2) + 
  facet_grid(rows = vars(mouse), cols = vars(date)) +
  ylab("frequency of errors") +
  ggtitle("cohort 1 tube error plot")

show(m1.eplot)


e_plot2 <- rbind(m2.1e, 
                m2.3e,
                m2.4e,
                m2.6e,
                m2.7e,
                m2.8e)

li2 <- strsplit(e_plot2$datetimestamp,split=":")
hrs2 <- as.numeric(unlist(lapply(li2, function(x) substr(x[[1]],12,13))))
mins2 <- as.numeric(unlist(lapply(li2, function(x) x[2])))
secs2 <- as.numeric(unlist(lapply(li2, function(x) x[3])))
ms2 <- as.numeric(unlist(lapply(li2, function(x) x[4])))

time2 <- strptime(e_plot2$datetimestamp, format="%d.%M.%Y %H:%M:%S:%OS")
times2 <- strftime(time2, format="%H:%M:%S:%OS")

e_plot2$times <- times2
e_plot2$hrs <- hrs2
e_plot2$mins <- mins2
e_plot2$bin <- ifelse(mins2>30, paste0(e_plot2$hrs,"b"), paste0(e_plot2$hrs, "a"))
e_plot2$totalmins <- (e_plot2$hrs * 60) + e_plot2$mins 

m2.eplot <- ggplot(e_plot2, aes(x=totalmins)) + 
  geom_histogram(binwidth=1,fill='lightseagreen', color='darkgreen') +
  theme_classic()+
  xlab("Time of Day in Minutes") +
  geom_vline(xintercept=c(660, 1380), color='black', lwd=1,lty=2) + 
  facet_grid(rows = vars(mouse), cols = vars(date)) +
  ylab("frequency of errors") +
  ggtitle("cohort 2 tube error plot")

show(m2.eplot)

grid.arrange(m1.eplot,
             m2.eplot)
## activity plots
m1.act <- rbind(m1.1,
            m1.3,
            m1.4,
            m1.6,
            m1.7,
            m1.8)

li <- strsplit(m1.act$datetimestamp,split=":")
hrs <- as.numeric(unlist(lapply(li, function(x) substr(x[[1]],12,13))))
mins <- as.numeric(unlist(lapply(li, function(x) x[2])))
secs <- as.numeric(unlist(lapply(li, function(x) x[3])))
ms <- as.numeric(unlist(lapply(li, function(x) x[4])))

time <- strptime(m1.act$datetimestamp, format="%d.%M.%Y %H:%M:%S:%OS")
times <- strftime(time, format="%H:%M:%S:%OS")

m1.act$times <- times
m1.act$hrs <- hrs
m1.act$mins <- mins
m1.act$bin <- ifelse(mins>30, paste0(m1.act$hrs,"b"), paste0(m1.act$hrs, "a"))
m1.act$totalmins <- (m1.act$hrs * 60) + m1.act$mins 

m1.actplot <- ggplot(m1.act, aes(x=totalmins)) + 
  geom_histogram(binwidth=1,fill='lightseagreen', color='darkgreen') +
  theme_classic()+
  xlab("Time of Day in Minutes") +
  geom_vline(xintercept=c(660, 1380), color='black', lwd=1,lty=2) + 
  facet_grid(rows = vars(mouse), cols = vars(date)) +
  ylab("activity level") +
  ggtitle("cohort 1 activity plot")

show(m1.actplot)


m2.act <- rbind(m2.1,
            m2.3,
            m2.4,
            m2.6,
            m2.7,
            m2.8)

li2 <- strsplit(m2.act$datetimestamp,split=":")
hrs2 <- as.numeric(unlist(lapply(li2, function(x) substr(x[[1]],12,13))))
mins2 <- as.numeric(unlist(lapply(li2, function(x) x[2])))
secs2 <- as.numeric(unlist(lapply(li2, function(x) x[3])))
ms2 <- as.numeric(unlist(lapply(li2, function(x) x[4])))

time2 <- strptime(m2.act$datetimestamp, format="%d.%M.%Y %H:%M:%S:%OS")
times2 <- strftime(time2, format="%H:%M:%S:%OS")

m2.act$times <- times2
m2.act$hrs <- hrs2
m2.act$mins <- mins2
m2.act$bin <- ifelse(mins2>30, paste0(m2.act$hrs,"b"), paste0(m2.act$hrs, "a"))
m2.act$totalmins <- (m2.act$hrs * 60) + m2.act$mins 

m2.actplot <- ggplot(m2.act, aes(x=totalmins)) + 
  geom_histogram(binwidth=1,fill='lightseagreen', color='darkgreen') +
  theme_classic()+
  xlab("Time of Day in Minutes") +
  geom_vline(xintercept=c(660, 1380), color='black', lwd=1,lty=2) + 
  facet_grid(rows = vars(mouse), cols = vars(date)) +
  ylab("activity level") +
  ggtitle("cohort 2 activity plot")

show(m2.actplot)

grid.arrange(m1.actplot,
             m2.actplot)

## error durations 
interval3 <- function(x){
  x1 <- x[-c(1:2)]
  out <-  x1[seq(1, length(x1), 3)] - x[seq(1, length(x), 3)] 
  return(out)
  }

## trying 
m1.1_inds = which(m1.1$error == "TRUE")

m1.1_rows <- lapply(m1.1_inds, function(x) (x-1):(x+1))

m1.1_interval3 <- m1.1[unlist(m1.1_rows),]

m1.1ed <- interval3(m1.1_interval3$ms)

hist(m1.1ed)

m1.1_zone_true <- m1.1 %>% filter(error==TRUE) %>%
  mutate(error_duration = m1.1ed)
m1.1_zone_false <- m1.1 %>% filter(error==FALSE) %>%
  mutate(error_duration=0)

m1.1x <- rbind(m1.1_zone_true, m1.1_zone_false)

m1.1x <- arrange(m1.1x, ms)

table(m1.1x$error_duration)

m1.1xp <- ggplot(m1.1_zone_true, aes(x=error_duration)) +
  geom_histogram(color="darkblue", fill="darkblue", bins=100) +
  facet_wrap(~date) +
  theme_classic() +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  ggtitle("mouse 1.1 error durations") +
  xlab("error durations in ms")

show(m1.1xp)









