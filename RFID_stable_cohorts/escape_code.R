library(tidyverse)

### this is tyler making your code dirty 

ids <- read_csv("RFID_stable_cohorts/raw_data/id_data.csv")
head(ids)


id_data <- ids[,1:4]



sep27 <- read.table(
  "Cohorts1_2/rfidtracking_data/rawdata20210927.csv", 
  sep=";", header=TRUE)
sep27$data <- as.character(sep27$data)
sep27$date <- format(as.POSIXct(sep27$datetimestamp,format="%d.%m.%Y %H:%M:%S:%OS"),"%m.%d.%Y")
sep27$time <- sub("^\\S+\\s+", '', sep27$datetimestamp)


sep28 <- read.table(
  "Cohorts1_2/rfidtracking_data/rawdata20210928.csv", 
  sep=";", header=TRUE)
sep28$data <- as.character(sep28$data)
sep28$date <- format(as.POSIXct(sep28$datetimestamp,format="%d.%m.%Y %H:%M:%S:%OS"),"%m.%d.%Y")
sep28$time <- sub("^\\S+\\s+", '', sep28$datetimestamp)

sep29 <- read.table(
  "Cohorts1_2/rfidtracking_data/rawdata20210929.csv", 
  sep=";", header=TRUE)
sep29$data <- as.character(sep29$data)
sep29$date <- format(as.POSIXct(sep29$datetimestamp,format="%d.%m.%Y %H:%M:%S:%OS"),"%m.%d.%Y")
sep29$time <- sub("^\\S+\\s+", '', sep29$datetimestamp)

id_data <- read_csv("Cohorts1_2/id_data.csv")

id_data$back_tag <- as.character(id_data$back_tag)
id_data$leg_tag <- as.character(id_data$leg_tag)




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

day_3$ms <- (as.numeric(unlist(day_2$ms)) + (8.64e+7 * 2))

df <- rbind(day_1,
            day_2,
            day_3)

df$ms <- df$ms - 39124161
df$hour <- sub("\\:.*:.*:.*", "", df$time) 

m1.4 <- subset(df, data %in% dfx[dfx$mouse==4,]$data & data %in% dfx[dfx$cohort==1,]$data) %>% 
  mutate(mouse = 4, cohort = 1)
m1.6 <- subset(df, data %in% dfx[dfx$mouse==6,]$data & data %in% dfx[dfx$cohort==1,]$data) %>% 
  mutate(mouse = 6, cohort = 1)
m1.8 <- subset(df, data %in% dfx[dfx$mouse==8,]$data & data %in% dfx[dfx$cohort==1,]$data) %>% 
  mutate(mouse = 8, cohort = 1)

table(m1.4$data)
table(m1.6$data)
table(m1.8$data)

table(m1.4$hour)
table(m1.6$hour)
table(m1.8$hour)


tube_errors <- function(df, pair1=c(1,2), pair2=c(3,4), pair3=c(5,6), pair4=c(7,8),
                          pair5=c(9,10), pair6=c(11,12), pair7=c(13,14), pair8=c(15,16)){
    
    ids <- c(pair1,pair2,pair3,pair4,pair5,pair6,pair7,pair8)
    codes <- c("1", "2","3", "4", "8","9")
    
    x <- codes[match(df$deviceid, ids)]
    
    row.inds <-
      c(intersect(which(x == "1"), which(lag(x) == "3")),
        intersect(which(x == "3"), which(lag(x) == "1")),
        intersect(which(x == "1"),which(lag(x) == "4")),
        intersect(which(x == "4"), which(lag(x) == "1")),
        intersect(which(x == "1"), which(lag(x) == "8")),
        intersect(which(x == "8"), which(lag(x) == "1")),
        intersect(which(x == "1"),which(lag(x) == "9")),
        intersect(which(x == "9"), which(lag(x) == "1")),
        intersect(which(x == "2"), which(lag(x) == "4")),
        intersect(which(x == "4"), which(lag(x) == "2")),
        intersect(which(x == "2"),which(lag(x) == "9")),
        intersect(which(x == "9"), which(lag(x) == "2")),
        intersect(which(x == "3"), which(lag(x) == "9")),
        intersect(which(x == "9"), which(lag(x) == "3")),
        intersect(which(x == "4"),which(lag(x) == "8")),
        intersect(which(x == "8"), which(lag(x) == "4"))
      )
    
    df$error <- FALSE
    df[row.inds,"error"]<-TRUE
    
    
    return(df)
  }


m1.4e <- as.data.frame(tube_errors(m1.4, pair1 = c(1,3), pair2 = c(1,4), pair3 = c(1,8), 
                                  pair4 = c(1,9), pair5 = c(2,4), pair6 = c(2,9), pair7 = c(3,9),
                                  pair8 = c(4,8))) %>% filter(error==TRUE)
m1.6e <- as.data.frame(tube_errors(m1.6, pair1 = c(1,3), pair2 = c(1,4), pair3 = c(1,8), 
                                  pair4 = c(1,9), pair5 = c(2,4), pair6 = c(2,9), pair7 = c(3,9),
                                  pair8 = c(4,8))) %>% filter(error==TRUE)
m1.8e <- as.data.frame(tube_errors(m1.8, pair1 = c(1,3), pair2 = c(1,4), pair3 = c(1,8), 
                                   pair4 = c(1,9), pair5 = c(2,4), pair6 = c(2,9), pair7 = c(3,9),
                                   pair8 = c(4,8))) %>% filter(error==TRUE)

e_plot <- rbind(m1.4e,
                m1.6e,
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

e_plot <- ggplot(e_plot, aes(x=totalmins)) + 
  geom_histogram(binwidth=1,fill='lightseagreen', color='darkgreen') +
  theme_classic()+
  xlab("Time of Day in Minutes") +
  geom_vline(xintercept=c(660, 1380), color='black', lwd=1,lty=2) + 
  facet_grid(rows = vars(mouse), cols = vars(date)) +
  ylab("frequency of errors") +
  ggtitle("tube error plot")
show(e_plot)

zz <- rbind(m1.4,
            m1.6,
            m1.8)

li <- strsplit(zz$datetimestamp,split=":")
hrs <- as.numeric(unlist(lapply(li, function(x) substr(x[[1]],12,13))))
mins <- as.numeric(unlist(lapply(li, function(x) x[2])))
secs <- as.numeric(unlist(lapply(li, function(x) x[3])))
ms <- as.numeric(unlist(lapply(li, function(x) x[4])))

time <- strptime(zz$datetimestamp, format="%d.%M.%Y %H:%M:%S:%OS")
times <- strftime(time, format="%H:%M:%S:%OS")

zz$times <- times
zz$hrs <- hrs
zz$mins <- mins
zz$bin <- ifelse(mins>30, paste0(zz$hrs,"b"), paste0(zz$hrs, "a"))
zz$totalmins <- (zz$hrs * 60) + zz$mins 

act_plot <- ggplot(zz, aes(x=totalmins)) + 
  geom_histogram(binwidth=1,fill='lightseagreen', color='darkgreen') +
  theme_classic()+
  xlab("Time of Day in Minutes") +
  geom_vline(xintercept=c(660, 1380), color='black', lwd=1,lty=2) + 
  facet_grid(rows = vars(mouse), cols = vars(date)) +
  ylab("activity level") +
  ggtitle("activity plot")

show(act_plot)
















