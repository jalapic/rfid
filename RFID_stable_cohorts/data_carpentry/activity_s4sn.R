library(tidyverse)


## loading in raw data from tracking behavior folder
temp <- list.files(path="RFID_stable_cohorts/data_raw/tracking_behavior/",pattern="*.csv")
xfiles <- lapply(temp, function(x) read_csv2(paste0("RFID_stable_cohorts/data_raw/tracking_behavior/",x)) )                 
lapply(xfiles, head)
lapply(xfiles, colnames)
myfiles1 <- Map(cbind, xfiles)

## separating lists by trial (11 lists per trial)
c12 <- myfiles1[c(1:11)] 
c12_df <- do.call('rbind', c12)
c12_df$cohort <- ifelse(c12_df$deviceid %in% c(1,2,3,4,8,9), 1,2 )



c34 <- myfiles1[c(12:22)] 
c34_df <-  do.call('rbind',c34)
c34_df$cohort <- ifelse(c34_df$deviceid %in% c(1,2,3,4,8,9), 3,4 )

c56 <- myfiles1[c(23:33)] 
c56_df <-  do.call('rbind',c56)
c56_df$cohort <- ifelse(c56_df$deviceid %in% c(1,2,3,4,8,9), 5,6 )

c78 <- myfiles1[c(34:44)] 
c78_df <-  do.call('rbind',c78)
c78_df$cohort <- ifelse(c78_df$deviceid %in% c(1,2,3,4,8,9), 7,8 )

l <- list("c12" = c12_df , "c34" = c34_df, "c56" = c56_df, "c78" = c78_df)
l <- lapply(l, head)

## loading in id data 
ids <- read_csv("RFID_stable_cohorts/data_raw/id_data.csv")
id_data <- ids[,1:5]
id_data$back_tag <- as.character(id_data$back_tag)
id_data$leg_tag <- as.character(id_data$leg_tag)

id <- id_data %>%
  pivot_longer(4:5)

colnames(id) <- c("cohort", "pre_id", "mouse", "name", "data")

## creating a mouse column by combing overlapping tags in id data and tracking data
id1 <- id %>%
  filter(cohort==1)
c1_df <- c12_df %>%
  filter(cohort==1)
m1.1 <- subset(c1_df, data %in% id1[id1$mouse==1,]$data) %>% mutate(mouse = 1)
m1.3 <- subset(c1_df, data %in% id1[id1$mouse==3,]$data) %>% mutate(mouse = 3)
m1.4 <- subset(c1_df, data %in% id1[id1$mouse==4,]$data) %>% mutate(mouse = 4)
m1.6 <- subset(c1_df, data %in% id1[id1$mouse==6,]$data) %>% mutate(mouse = 6)
m1.7 <- subset(c1_df, data %in% id1[id1$mouse==7,]$data) %>% mutate(mouse = 7)
m1.8 <- subset(c1_df, data %in% id1[id1$mouse==8,]$data) %>% mutate(mouse = 8)
c1df <- rbind(m1.1, m1.3, m1.4, m1.6, m1.7, m1.8)
id2 <- id %>%
  filter(cohort==2)
c2_df <- c12_df %>%
  filter(cohort==2)
m2.1 <- subset(c2_df, data %in% id1[id2$mouse==1,]$data) %>% mutate(mouse = 1)
m2.3 <- subset(c2_df, data %in% id1[id2$mouse==3,]$data) %>% mutate(mouse = 3)
m2.4 <- subset(c2_df, data %in% id1[id2$mouse==4,]$data) %>% mutate(mouse = 4)
m2.6 <- subset(c2_df, data %in% id1[id2$mouse==6,]$data) %>% mutate(mouse = 6)
m2.7 <- subset(c2_df, data %in% id1[id2$mouse==7,]$data) %>% mutate(mouse = 7)
m2.8 <- subset(c2_df, data %in% id1[id2$mouse==8,]$data) %>% mutate(mouse = 8)
c2df <- rbind(m2.1, m2.3, m2.4, m2.6, m2.7, m2.8)
c12df <- rbind(c1df, c2df)

id3 <- id %>%
  filter(cohort==3)
c3_df <- c34_df %>%
  filter(cohort==3)
m3.2 <- subset(c3_df, data %in% id3[id3$mouse==2,]$data) %>% mutate(mouse = 2)
m3.3 <- subset(c3_df, data %in% id3[id3$mouse==3,]$data) %>% mutate(mouse = 3)
m3.4 <- subset(c3_df, data %in% id3[id3$mouse==4,]$data) %>% mutate(mouse = 4)
m3.5 <- subset(c3_df, data %in% id3[id3$mouse==5,]$data) %>% mutate(mouse = 5)
m3.6 <- subset(c3_df, data %in% id3[id3$mouse==6,]$data) %>% mutate(mouse = 6)
m3.7 <- subset(c3_df, data %in% id3[id3$mouse==7,]$data) %>% mutate(mouse = 7)
c3df <- rbind(m3.2, m3.3, m3.4, m3.5, m3.6, m3.7)
id4 <- id %>%
  filter(cohort==4)
c4_df <- c34_df %>%
  filter(cohort==4)
m4.2 <- subset(c4_df, data %in% id4[id4$mouse==2,]$data) %>% mutate(mouse = 2)
m4.3 <- subset(c4_df, data %in% id4[id4$mouse==3,]$data) %>% mutate(mouse = 3)
m4.4 <- subset(c4_df, data %in% id4[id4$mouse==4,]$data) %>% mutate(mouse = 4)
m4.5 <- subset(c4_df, data %in% id4[id4$mouse==5,]$data) %>% mutate(mouse = 5)
m4.6 <- subset(c4_df, data %in% id4[id4$mouse==6,]$data) %>% mutate(mouse = 6)
m4.7 <- subset(c4_df, data %in% id4[id4$mouse==7,]$data) %>% mutate(mouse = 7)
c4df <- rbind(m4.2, m4.3, m4.4, m4.5, m4.6, m4.7)
c34df <- rbind(c3df, c4df)

id5 <- id %>%
  filter(cohort==5)
c5_df <- c56_df %>%
  filter(cohort==5)
m5.1 <- subset(c5_df, data %in% id5[id5$mouse==1,]$data) %>% mutate(mouse = 1)
m5.4 <- subset(c5_df, data %in% id5[id5$mouse==4,]$data) %>% mutate(mouse = 4)
m5.5 <- subset(c5_df, data %in% id5[id5$mouse==5,]$data) %>% mutate(mouse = 5)
m5.6 <- subset(c5_df, data %in% id5[id5$mouse==6,]$data) %>% mutate(mouse = 6)
m5.7 <- subset(c5_df, data %in% id5[id5$mouse==7,]$data) %>% mutate(mouse = 7)
m5.8 <- subset(c5_df, data %in% id5[id5$mouse==8,]$data) %>% mutate(mouse = 8)
c5df <- rbind(m5.1, m5.4, m5.5, m5.6, m5.7, m5.8)
id6 <- id %>%
  filter(cohort==6)
c6_df <- c56_df %>%
  filter(cohort==6)
m6.1 <- subset(c6_df, data %in% id6[id6$mouse==1,]$data) %>% mutate(mouse = 1)
m6.4 <- subset(c6_df, data %in% id6[id6$mouse==4,]$data) %>% mutate(mouse = 4)
m6.5 <- subset(c6_df, data %in% id6[id6$mouse==5,]$data) %>% mutate(mouse = 5)
m6.6 <- subset(c6_df, data %in% id6[id6$mouse==6,]$data) %>% mutate(mouse = 6)
m6.7 <- subset(c6_df, data %in% id6[id6$mouse==7,]$data) %>% mutate(mouse = 7)
m6.8 <- subset(c6_df, data %in% id6[id6$mouse==8,]$data) %>% mutate(mouse = 8)
c6df <- rbind(m6.1, m6.4, m6.5, m6.6, m6.7, m6.8)
c56df <- rbind(c5df, c6df)
id7 <- id %>%
  filter(cohort==7)
c7_df <- c78_df %>%
  filter(cohort==7)
m7.1 <- subset(c7_df, data %in% id7[id7$mouse==1,]$data) %>% mutate(mouse = 1)
m7.2 <- subset(c7_df, data %in% id7[id7$mouse==2,]$data) %>% mutate(mouse = 2)
m7.3 <- subset(c7_df, data %in% id7[id7$mouse==3,]$data) %>% mutate(mouse = 3)
m7.5 <- subset(c7_df, data %in% id7[id7$mouse==5,]$data) %>% mutate(mouse = 5)
m7.6 <- subset(c7_df, data %in% id7[id7$mouse==6,]$data) %>% mutate(mouse = 6)
m7.7 <- subset(c7_df, data %in% id7[id7$mouse==7,]$data) %>% mutate(mouse = 7)
c7df <- rbind(m7.1, m7.2, m7.3, m7.5, m7.6, m7.7)
id8 <- id %>%
  filter(cohort==8)
c8_df <- c78_df %>%
  filter(cohort==8)
m8.1 <- subset(c8_df, data %in% id8[id8$mouse==1,]$data) %>% mutate(mouse = 1)
m8.2 <- subset(c8_df, data %in% id8[id8$mouse==2,]$data) %>% mutate(mouse = 2)
m8.3 <- subset(c8_df, data %in% id8[id8$mouse==3,]$data) %>% mutate(mouse = 3)
m8.5 <- subset(c8_df, data %in% id8[id8$mouse==5,]$data) %>% mutate(mouse = 5)
m8.6<- subset(c8_df, data %in% id8[id8$mouse==6,]$data) %>% mutate(mouse = 6)
m8.7 <- subset(c8_df, data %in% id8[id8$mouse==7,]$data) %>% mutate(mouse = 7)
c8df <- rbind(m8.1, m8.2, m8.3, m8.5, m8.6, m8.7)
c78df <- rbind(c7df, c8df)







## creating milliseconds colum for each trial
c12df$date <- format(as.POSIXct(c12df$datetimestamp,format="%d.%m.%Y %H:%M:%S:%OS"),"%m.%d.%Y")
c12df$time <- sub("^\\S+\\s+", '', c12df$datetimestamp)
xx <- strsplit(c12df$time, split=":")
xxx <- matrix(unlist(xx), ncol = 4, byrow = TRUE)
c12df$ms<-
  (3600000 * as.numeric(xxx[,1])) +
  (60000 * as.numeric(xxx[,2])) +
  (1000 * as.numeric(xxx[,3])) +
  (1 * as.numeric(xxx[,4]))
c12df <- c12df %>%
  arrange(date)
c12_d1 <- c12df %>% filter(date == "09.27.2021")
c12_d1$ms <- as.numeric(unlist(c12_d1$ms)) 
c12_d2 <- c12df %>% filter(date == "09.28.2021")
c12_d2$ms <- (as.numeric(unlist(c12_d2$ms)) + (8.64e+7 * 1)) 
c12_d3 <- c12df %>% filter(date == "09.29.2021")
c12_d3$ms <- (as.numeric(unlist(c12_d3$ms)) + (8.64e+7 * 2)) 
c12_d4 <- c12df %>% filter(date == "09.30.2021")
c12_d4$ms <- (as.numeric(unlist(c12_d4$ms)) + (8.64e+7 * 3)) 
c12_d5 <- c12df %>% filter(date == "10.01.2021")
c12_d5$ms <- (as.numeric(unlist(c12_d5$ms)) + (8.64e+7 * 4)) 
c12_d6 <- c12df %>% filter(date == "10.02.2021")
c12_d6$ms <- (as.numeric(unlist(c12_d6$ms)) + (8.64e+7 * 5)) 
c12_d7 <- c12df %>% filter(date == "10.03.2021")
c12_d7$ms <- (as.numeric(unlist(c12_d7$ms)) + (8.64e+7 * 6)) 
c12_d8 <- c12df %>% filter(date == "10.04.2021")
c12_d8$ms <- (as.numeric(unlist(c12_d8$ms)) + (8.64e+7 * 7)) 
c12_d9 <- c12df %>% filter(date == "10.05.2021")
c12_d9$ms <- (as.numeric(unlist(c12_d9$ms)) + (8.64e+7 * 8)) 
c12_d10 <- c12df %>% filter(date == "10.06.2021")
c12_d10$ms <- (as.numeric(unlist(c12_d10$ms)) + (8.64e+7 * 9)) 
c12_d11 <- c12df %>% filter(date == "10.07.2021")
c12_d11$ms <- (as.numeric(unlist(c12_d11$ms)) + (8.64e+7 * 10)) 
c12m <- rbind(c12_d1, c12_d2, c12_d3, c12_d4, c12_d5, c12_d6, c12_d7, c12_d8, c12_d9, c12_d10, c12_d11)
c12m <- c12m %>%
  arrange(ms)
c12m$ms <- c12m$ms - 39124161
c12m$data <- as.character(c12m$data)

table(c12m$cohort)

c34df$date <- format(as.POSIXct(c34df$datetimestamp,format="%d.%m.%Y %H:%M:%S:%OS"),"%m.%d.%Y")
c34df$time <- sub("^\\S+\\s+", '', c34df$datetimestamp)
xx <- strsplit(c34df$time, split=":")
xxx <- matrix(unlist(xx), ncol = 4, byrow = TRUE)
c34df$ms<-
  (3600000 * as.numeric(xxx[,1])) +
  (60000 * as.numeric(xxx[,2])) +
  (1000 * as.numeric(xxx[,3])) +
  (1 * as.numeric(xxx[,4]))
c34df <- c34df %>%
  arrange(date)
c34_d1 <- c34df %>% filter(date == "10.18.2021")
c34_d1$ms <- as.numeric(unlist(c34_d1$ms)) 
c34_d2 <- c34df %>% filter(date == "10.19.2021")
c34_d2$ms <- (as.numeric(unlist(c34_d2$ms)) + (8.64e+7 * 1)) 
c34_d3 <- c34df %>% filter(date == "10.20.2021")
c34_d3$ms <- (as.numeric(unlist(c34_d3$ms)) + (8.64e+7 * 2)) 
c34_d4 <- c34df %>% filter(date == "10.21.2021")
c34_d4$ms <- (as.numeric(unlist(c34_d4$ms)) + (8.64e+7 * 3)) 
c34_d5 <- c34df %>% filter(date == "10.22.2021")
c34_d5$ms <- (as.numeric(unlist(c34_d5$ms)) + (8.64e+7 * 4)) 
c34_d6 <- c34df %>% filter(date == "10.23.2021")
c34_d6$ms <- (as.numeric(unlist(c34_d6$ms)) + (8.64e+7 * 5)) 
c34_d7 <- c34df %>% filter(date == "10.24.2021")
c34_d7$ms <- (as.numeric(unlist(c34_d7$ms)) + (8.64e+7 * 6)) 
c34_d8 <- c34df %>% filter(date == "10.25.2021")
c34_d8$ms <- (as.numeric(unlist(c34_d8$ms)) + (8.64e+7 * 7)) 
c34_d9 <- c34df %>% filter(date == "10.26.2021")
c34_d9$ms <- (as.numeric(unlist(c34_d9$ms)) + (8.64e+7 * 8)) 
c34_d10 <- c34df %>% filter(date == "10.27.2021")
c34_d10$ms <- (as.numeric(unlist(c34_d10$ms)) + (8.64e+7 * 9)) 
c34_d11 <- c34df %>% filter(date == "10.28.2021")
c34_d11$ms <- (as.numeric(unlist(c34_d11$ms)) + (8.64e+7 * 10)) 
c34m <- rbind(c34_d1, c34_d2, c34_d3, c34_d4, c34_d5, c34_d6, c34_d7, c34_d8, c34_d9, c34_d10, c34_d11)
c34m <- c34m %>%
  arrange(ms)
c34m$ms <- c34m$ms - 39209615
c34m$data <- as.character(c34m$data)


c56df$date <- format(as.POSIXct(c56df$datetimestamp,format="%d.%m.%Y %H:%M:%S:%OS"),"%m.%d.%Y")
c56df$time <- sub("^\\S+\\s+", '', c56df$datetimestamp)
xx <- strsplit(c56df$time, split=":")
xxx <- matrix(unlist(xx), ncol = 4, byrow = TRUE)
c56df$ms<-
  (3600000 * as.numeric(xxx[,1])) +
  (60000 * as.numeric(xxx[,2])) +
  (1000 * as.numeric(xxx[,3])) +
  (1 * as.numeric(xxx[,4]))
c56df <- c56df %>%
  arrange(date)
c56df <- c56df %>%
  filter(cohort==5 | cohort ==6)
c56_d1 <- c56df %>% filter(date == "11.08.2021")
c56_d1$ms <- as.numeric(unlist(c56_d1$ms)) 
c56_d2 <- c56df %>% filter(date == "11.09.2021")
c56_d2$ms <- (as.numeric(unlist(c56_d2$ms)) + (8.64e+7 * 1)) 
c56_d3 <- c56df %>% filter(date == "11.10.2021")
c56_d3$ms <- (as.numeric(unlist(c56_d3$ms)) + (8.64e+7 * 2)) 
c56_d4 <- c56df %>% filter(date == "11.11.2021")
c56_d4$ms <- (as.numeric(unlist(c56_d4$ms)) + (8.64e+7 * 3)) 
c56_d5 <- c56df %>% filter(date == "11.12.2021")
c56_d5$ms <- (as.numeric(unlist(c56_d5$ms)) + (8.64e+7 * 4)) 
c56_d6 <- c56df %>% filter(date == "11.13.2021")
c56_d6$ms <- (as.numeric(unlist(c56_d6$ms)) + (8.64e+7 * 5)) 
c56_d7 <- c56df %>% filter(date == "11.14.2021")
c56_d7$ms <- (as.numeric(unlist(c56_d7$ms)) + (8.64e+7 * 6)) 
c56_d8 <- c56df %>% filter(date == "11.15.2021")
c56_d8$ms <- (as.numeric(unlist(c56_d8$ms)) + (8.64e+7 * 7)) 
c56_d9 <- c56df %>% filter(date == "11.16.2021")
c56_d9$ms <- (as.numeric(unlist(c56_d9$ms)) + (8.64e+7 * 8)) 
c56_d10 <- c56df %>% filter(date == "11.17.2021")
c56_d10$ms <- (as.numeric(unlist(c56_d10$ms)) + (8.64e+7 * 9)) 
c56_d11 <- c56df %>% filter(date == "11.18.2021")
c56_d11$ms <- (as.numeric(unlist(c56_d11$ms)) + (8.64e+7 * 10))
c56m <- rbind(c56_d1, c56_d2, c56_d3, c56_d4, c56_d5, c56_d6, c56_d7, c56_d8, c56_d9, c56_d10, c56_d11)
c56m <- c56m %>%
  arrange(ms)
c56m$ms <- c56m$ms - 35017088
c56m$data <- as.character(c56m$data)


c78df$date <- format(as.POSIXct(c78df$datetimestamp,format="%d.%m.%Y %H:%M:%S:%OS"),"%m.%d.%Y")
c78df$time <- sub("^\\S+\\s+", '', c78df$datetimestamp)
xx <- strsplit(c78df$time, split=":")
xxx <- matrix(unlist(xx), ncol = 4, byrow = TRUE)
c78df$ms<-
  (3600000 * as.numeric(xxx[,1])) +
  (60000 * as.numeric(xxx[,2])) +
  (1000 * as.numeric(xxx[,3])) +
  (1 * as.numeric(xxx[,4]))
c78df <- c78df %>%
  arrange(date)
c78df <- c78df %>%
  filter(cohort==7 | cohort ==8)
c78_d1 <- c78df %>% filter(date == "11.22.2021")
c78_d1$ms <- as.numeric(unlist(c78_d1$ms)) 
c78_d2 <- c78df %>% filter(date == "11.23.2021")
c78_d2$ms <- (as.numeric(unlist(c78_d2$ms)) + (8.64e+7 * 1)) 
c78_d3 <- c78df %>% filter(date == "11.24.2021")
c78_d3$ms <- (as.numeric(unlist(c78_d3$ms)) + (8.64e+7 * 2)) 
c78_d4 <- c78df %>% filter(date == "11.25.2021")
c78_d4$ms <- (as.numeric(unlist(c78_d4$ms)) + (8.64e+7 * 3)) 
c78_d5 <- c78df %>% filter(date == "11.26.2021")
c78_d5$ms <- (as.numeric(unlist(c78_d5$ms)) + (8.64e+7 * 4)) 
c78_d6 <- c78df %>% filter(date == "11.27.2021")
c78_d6$ms <- (as.numeric(unlist(c78_d6$ms)) + (8.64e+7 * 5)) 
c78_d7 <- c78df %>% filter(date == "11.28.2021")
c78_d7$ms <- (as.numeric(unlist(c78_d7$ms)) + (8.64e+7 * 6)) 
c78_d8 <- c78df %>% filter(date == "11.29.2021")
c78_d8$ms <- (as.numeric(unlist(c78_d8$ms)) + (8.64e+7 * 7)) 
c78_d9 <- c78df %>% filter(date == "11.30.2021")
c78_d9$ms <- (as.numeric(unlist(c78_d9$ms)) + (8.64e+7 * 8)) 
c78_d10 <- c78df %>% filter(date == "12.01.2021")
c78_d10$ms <- (as.numeric(unlist(c78_d10$ms)) + (8.64e+7 * 9)) 
c78_d11 <- c78df %>% filter(date == "12.02.2021")
c78_d11$ms <- (as.numeric(unlist(c78_d11$ms)) + (8.64e+7 * 10))
c78m <- rbind(c78_d1, c78_d2, c78_d3, c78_d4, c78_d5, c78_d6, c78_d7, c78_d8, c78_d9, c78_d10, c78_d11)
c78m <- c78m %>%
  arrange(ms)
c78m$ms <- c78m$ms - 35017088
c78m$data <- as.character(c78m$data)

activity.plot <- function(act){
  
  li <- strsplit(act$datetimestamp,split=":")
  hrs <- as.numeric(unlist(lapply(li, function(x) substr(x[[1]],12,13))))
  mins <- as.numeric(unlist(lapply(li, function(x) x[2])))
  secs <- as.numeric(unlist(lapply(li, function(x) x[3])))
  ms <- as.numeric(unlist(lapply(li, function(x) x[4])))
  
  time <- strptime(act$datetimestamp, format="%d.%M.%Y %H:%M:%S:%OS")
  times <- strftime(time, format="%H:%M:%S:%OS")
  
  act$times <- times
  act$hrs <- hrs
  act$mins <- mins
  act$bin <- ifelse(mins>30, paste0(act$hrs,"b"), paste0(act$hrs, "a"))
  act$totalmins <- (act$hrs * 60) + act$mins 
  
  act_plot <-  ggplot(act, aes(x=totalmins)) + 
    geom_histogram(binwidth=1,fill='lightseagreen', color='darkgreen') +
    theme_classic()+
    xlab("Time of Day in Minutes") +
    geom_vline(xintercept=c(660, 1380), color='black', lwd=1,lty=2) + 
    facet_grid(rows = vars(mouse), cols = vars(date)) +
    ggtitle("activity plot")
  
  show(act_plot)
}

c1.act <- c12m %>%
  filter(cohort==1)
c2.act <- c12m %>%
  filter(cohort==2)
c3.act <- c34m %>%
  filter(cohort==3)
c4.act <- c34m %>%
  filter(cohort==4)
c5.act <- c56m %>%
  filter(cohort==5)
c6.act <- c56m %>%
  filter(cohort==6)
c7.act <- c78m %>%
  filter(cohort==7)
c8.act <- c78m %>%
  filter(cohort==8)

c1.act <- as.data.frame(c1.act)
c2.act <- as.data.frame(c2.act)
c3.act <- as.data.frame(c3.act)
c4.act <- as.data.frame(c4.act)
c5.act <- as.data.frame(c5.act)
c6.act <- as.data.frame(c6.act)
c7.act <- as.data.frame(c7.act)
c8.act <- as.data.frame(c8.act)


activity.plot(c1.act)
activity.plot(c2.act)
activity.plot(c3.act)
activity.plot(c4.act)
activity.plot(c5.act)
activity.plot(c6.act)
activity.plot(c8.act)


# zone function for cohorts 1, 3, 5,7
zone.1 <- function(df){
  df$row <- 1:nrow(df)
  df$deviceid <- as.numeric(df$deviceid)
  df$antennaID <- as.numeric(df$antennaID)
  
  z1 <- filter(df, deviceid==1 & antennaID==1)
  z2 <- filter(df, deviceid==1 & antennaID==2 | deviceid==2 & antennaID==1)
  z3 <- filter(df, deviceid==2 & antennaID==2 | deviceid==3 & antennaID==1 | deviceid==8 & antennaID==1)
  z4 <- filter(df, deviceid==3 & antennaID==2 | deviceid==4 & antennaID==1)
  z5 <- filter(df, deviceid==4 & antennaID==2 | deviceid==9 & antennaID==2)
  z6 <- filter(df, deviceid==9 & antennaID==1 | deviceid==8 & antennaID==2)
  
  z1$zone=1
  z2$zone=2
  z3$zone=3
  z4$zone=4
  z5$zone=5
  z6$zone=6
  
  zone_df <- rbind(z1,z2,z3,z4,z5,z6)
  zone_df <- arrange(zone_df, row)
  
  x <- zone_df$zone
  x1 <- sort(unique(c(length(x) - cumsum(rle(rev(x))$lengths) + 1, cumsum(rle(x)$lengths) )))
  x1 <- unlist(x1)
  
  zone_df <- zone_df[x1,]
}

c1.act <- zone.1(c1.act)
c3.act <- zone.1(c3.act)
c5.act <- zone.1(c5.act)
c7.act <- zone.1(c7.act)

table(c1.act$zone)
table(c3.act$zone)
table(c5.act$zone)
table(c7.act$zone)
# zone function for cohorts 2, 4, 6,8
zone.2 <- function(df){
  df$row <- 1:nrow(df)
  df$deviceid <- as.numeric(df$deviceid)
  df$antennaID <- as.numeric(df$antennaID)
  
  z1 <- filter(df, deviceid==19 & antennaID==1)
  z2 <- filter(df, deviceid==19 & antennaID==2 | deviceid==18 & antennaID==1)
  z3 <- filter(df, deviceid==18 & antennaID==2 | deviceid==17 & antennaID==1 | deviceid==21 & antennaID==2)
  z4 <- filter(df, deviceid==17 & antennaID==2 | deviceid==16 & antennaID==1)
  z5 <- filter(df, deviceid==16 & antennaID==2 | deviceid==20 & antennaID==1)
  z6 <- filter(df, deviceid==20 & antennaID==2 | deviceid==21 & antennaID==1)
  
  z1$zone=1
  z2$zone=2
  z3$zone=3
  z4$zone=4
  z5$zone=5
  z6$zone=6
  
  zone_df <- rbind(z1,z2,z3,z4,z5,z6)
  zone_df <- arrange(zone_df, row)
  
  x <- zone_df$zone
  x1 <- sort(unique(c(length(x) - cumsum(rle(rev(x))$lengths) + 1, cumsum(rle(x)$lengths) )))
  x1 <- unlist(x1)
  
  zone_df <- zone_df[x1,]
}

c2.act <- zone.2(c2.act)
c4.act <- zone.2(c4.act)
c6.act <- zone.2(c6.act)
c8.act <- zone.2(c8.act)


table(c2.act$zone)
table(c4.act$zone)
table(c6.act$zone)
table(c8.act$zone)
##need to create data frame 




act <- c1.act %>% rbind(c2.act, c3.act, c4.act,c5.act, c6.act, c7.act, c8.act)
head(act)
tail(act)

help <- act %>% select(cohort, mouse, zone, ms)
ass <- help %>% group_by(cohort,mouse,zone) %>% mutate(start = ms) %>% mutate(end1 = lead(start))
str(ass)
rank

rank$mouse <- as.numeric(rank$mouse)

asx <- ass %>% full_join(rank)
head(asx)
asx <- asx %>% select(cohort, mouse, zone, ms, start,end, glicko_rank,dom)


ca <- asx %>% split(.$cohort)
lapply(ca1, tail)

ca1 <- ca %>% 
  map(~mutate(.,end = coalesce(end1, start)))

a_df <- do.call(rbind, ca1) %>% select(cohort, mouse, zone, start, end, glicko_rank)
head(a_df)
tail(a_df)

saveRDS(asx, "RFID_stable_cohorts/data_clean/axs.rds")

asx$ztime <- asx$end -asx$start

head(asx)

x <- asx %>% group_by(cohort, glicko_rank, zone) %>% summarize(., total = sum(!is.na(ms)))

head(x)



rank <- read_csv("RFID_stable_cohorts/data_clean/socialbehavior/rank_data.csv")
head(rank)
rank$dom <- ifelse(rank$glicko_rank == 1, "Dominant", rank$glicko_rank)
rank$dom <- ifelse(rank$glicko_rank == 2, "Subdominant", rank$dom)
rank$dom <- ifelse(rank$glicko_rank == 6, "Subordinate", rank$dom)

rank$id <- as.character(rank$id)
colnames(rank)[3]<- "mouse"
rank$mouse <- as.numeric(rank$mouse)

range(act$ms)
str(rank)

a1<- act %>%  select(6:12)

act_rank <- rank %>% full_join(a1)
head(act_rank)

saveRDS(act_rank, "RFID_stable_cohorts/data_clean/activity_zone.rds")

#### zone entries isk ?????

la <- act %>% split(.$cohort)
lapply(la2, head)


la2 <- la %>% map(~group_by(., mouse)) %>% 
  map(~mutate(.,zd = ms-lag(ms))) %>% map(~group_by(.,mouse,zone)) %>% 
  map(~summarize(., total = sum(!is.na(zd))))
dfnames <- c(1:8)

la3 <- la2 %>% map(~group_by(.,mouse,zone)) %>% 
  map(~summarize(., total = sum(!is.na(zd))))
dfnames <- c(1:8)

ztime <-la2 %>% map2_df(.,dfnames, ~mutate(.x, cohort = .y))


colnames(ztime)[3] <-'time_ms'

# write.csv(act, "RFID_stable_cohorts/data_clean/zonetimes.csv", row.names=F)

rank <- read_csv("RFID_stable_cohorts/data_clean/socialbehavior/rank_data.csv")
head(rank)
rank$dom <- ifelse(rank$glicko_rank == 1, "Dominant", rank$glicko_rank)
rank$dom <- ifelse(rank$glicko_rank == 2, "Subdominant", rank$dom)
rank$dom <- ifelse(rank$glicko_rank == 6, "Subordinate", rank$dom)

rank$id <- as.character(rank$id)
colnames(rank)[3]<- "mouse"
ztime$mouse <- as.character(ztime$mouse)
zg <- ztime %>% full_join(rank)
head(zg)

rank$mouse <- as.numeric(rank$mouse)
ztot$mouse<- as.character(ztot$mouse)
zblah <- ztot %>% full_join(rank)
head(zblah)

tp <- help %>% full_join(rank)


zg$glicko_rank <- as.factor(zg$glicko_rank)
zblah <- zblah %>% filter(cohort !=2)
zblah$glicko_rank <- as.factor(zblah$glicko_rank)

zg$glicko_rank <- as.factor(zg$glicko_rank)
zblah <- zblah %>% filter(cohort !=2)
zblah$glicko_rank <- as.factor(zblah$glicko_rank)

ggplot(asx, aes(glicko_rank, total, color = glicko_rank))+
  geom_boxjitter(aes(fill = glicko_rank), outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  theme_classic()+
  theme(legend.position = "none", text = element_text(size=20))+
  ylab("# of Zone Entries")+
  xlab("Mouse Rank")

zg$glicko_rank <- as.factor(zg$glicko_rank)
zg <- zg %>% filter(cohort !=2)
zblah$glicko_rank <- as.factor(zblah$glicko_rank)

zg <- zg %>% group_by(mouse,glicko_rank) %>% mutate( time_tot = sum(time_ms))
x$glicko_rank <- as.factor(x$glicko_rank)
ggplot(x, aes(glicko_rank, total, color = glicko_rank))+
  geom_boxjitter(aes(fill = glicko_rank), outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  theme_classic()+
  theme(legend.position = "none", text = element_text(size=20))+
  ylab("")+
  xlab("Mouse Rank")

table(zg$time_ms, zg$glicko_rank)

zg1 <- zg %>% filter(zone == 1) %>% filter(dom != "3") %>% filter(dom != "4") %>% filter(dom !="5") %>% filter(cohort!=2)

library(viridis)
ggplot(zg1, aes(dom,time_ms, color =dom))+
  geom_boxjitter(aes(fill = dom), outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  theme(legend.position = "none", text = element_text(size=15))+
  ylab("# of Entries into Foodcage")+
  xlab("")+  newggtheme +ylim(2000, 10000)
# food cage is cage 1

library(lmerTest)
library(lme4)
library(emmeans)

hist(zg1$time_ms)
zg1$dom1 <- factor(zg1$dom, levels = c("Subdominant", "Subordinate", "Dominant"))
cmix <- lmer(time_ms~dom+ (1|cohort)+ (1|mouse), data =zg1)
summary(cmix)

tpt <- tp %>%  filter(cohort !=2) %>% group_by(cohort,zone, glicko_rank) %>% summarize(., total = sum(ms))

tpt$glicko_rank <-as.factor(tpt$glicko_rank)
zg$glicko_rank <- as.factor(zg$glicko_rank)
tpt <- tpt %>% filter(cohort !=2)
zblah$glicko_rank <- as.factor(zblah$glicko_rank)

ggplot(tpt, aes(glicko_rank, total, color = glicko_rank))+
  geom_boxjitter(aes(fill = glicko_rank), outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  theme_classic()+
  theme(legend.position = "none", text = element_text(size=20))+
  ylab("Total Activity (ms)")+
  xlab("Mouse Rank") 

xx <- emmeans(cmix, ~ dom)
xx
# cohort 1 
c1m1 <- c1.act %>%
  filter(mouse==1)
c1m3 <- c1.act %>%
  filter(mouse==3)
c1m4 <- c1.act %>%
  filter(mouse==4)
c1m6 <- c1.act %>%
  filter(mouse==6)
c1m7 <- c1.act %>%
  filter(mouse==7)
c1m8 <- c1.act %>%
  filter(mouse==8)

c1m1z <- c1m1 %>%
  mutate(zd=ms-lag(ms))
c1m1z %>%
  mutate(zd=ms-lag(ms)) %>%
  group_by(zone) %>%
  summarize(total=sum(!is.na(zd)))

## c1m1 spends the most amount of time in zone 3


## trying to get overlap 




