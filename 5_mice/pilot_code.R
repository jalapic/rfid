library(tidyverse)

## working with pilot data

## loading in data
getwd()
df1 <- read_csv2("5_mice/raw_data/rawdata_group20210823.csv")
df2 <- read_csv2("5_mice/raw_data/rawdata_group20210824.csv")

dff <- rbind(df1, df2)

dff$data <- as.character(dff$data)

## creating ms column

dff$date <- format(as.POSIXct(dff$datetimestamp,format="%d.%m.%Y %H:%M:%S:%OS"),"%m.%d.%Y")

dff$time <- sub("^\\S+\\s+", '', dff$datetimestamp)

xx1 <- strsplit(dff$time, split=":")

#xx
xxx1 <- matrix(unlist(xx1), ncol = 4, byrow = TRUE)
#xxx[,1]
#xxx[,2]
#xxx[,3]
#as.numeric(xxx[,4])

dff$ms<-
  (3600000 * as.numeric(xxx1[,1])) +
  (60000 * as.numeric(xxx1[,2])) +
  (1000 * as.numeric(xxx1[,3])) +
  (1 * as.numeric(xxx1[,4]))

day_1 <- dff %>% filter(date == "08.23.2021")

day_1$ms <- as.numeric(unlist(day_1$ms)) - (42341803)

day_2 <- dff %>% filter(date == "08.24.2021")

day_2$ms <- (as.numeric(unlist(day_2$ms)) + (8.64e+7 * 1)) - (42341803)

dff <- rbind(day_1, day_2)

dff$ms <- as.character(dff$ms)


## creating csv for mice id's
dfxx <- data.frame(data=c("900133000459716", "900133000459717",
                       "900133000459692","900133000459691",
                       "900133000459728", "900133000459729",
                       "900133000459714", "900133000459715",
                       "900133000459726", "900133000459730"),
                mouse = c(1,1,2,2,4,4,7,7,8,8)
                )

write.csv(dfxx,"5_mice/raw_data/mouseids.csv", row.names = F)

dfxx <- read_csv("5_mice/raw_data/mouseids.csv")

## creating `mouse` column
mouse1 <- subset(dff, data %in% dfxx[dfxx$mouse==1,]$data) %>% mutate(mouse = 1)
mouse2 <- subset(dff, data %in% dfxx[dfxx$mouse==2,]$data) %>% mutate(mouse = 2)
mouse4 <- subset(dff, data %in% dfxx[dfxx$mouse==4,]$data) %>% mutate(mouse = 4)
mouse7 <- subset(dff, data %in% dfxx[dfxx$mouse==7,]$data) %>% mutate(mouse = 7)
mouse8 <- subset(dff, data %in% dfxx[dfxx$mouse==8,]$data) %>% mutate(mouse = 8)

dff <- rbind(mouse1 %>% mutate(mouse="1"), 
            mouse2 %>% mutate(mouse="2"), 
            mouse4 %>% mutate(mouse="4"), 
            mouse7 %>% mutate(mouse="7"),
            mouse8 %>% mutate(mouse="8"))

dff$ms <- as.numeric(dff$ms)

dff <- dff %>% arrange(ms)

## activity plot
act <- dff

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

act_plot2 <- ggplot(act, aes(x=totalmins, fill=mouse)) + 
  geom_histogram(binwidth=1) +
  theme_classic()+
  xlab("Time of Day in Minutes") +
  geom_vline(xintercept=c(660, 1380), color='black', lwd=1,lty=2) + 
  facet_grid(cols = vars(date)) +
  ggtitle("activity plot")

show(act_plot2)


## zones
find_zone <- function(df){
  df$row <- 1:nrow(df)
  df$deviceid <- as.numeric(df$deviceid)
  df$antennaID <- as.numeric(df$antennaID)
  
  z1 <- filter(df, deviceid==1 & antennaID==1)
  z2 <- filter(df, deviceid==1 & antennaID==2 | deviceid==3 & antennaID==1)
  z3 <- filter(df, deviceid==3 & antennaID==2 | deviceid==9 & antennaID==1 | deviceid==19 & antennaID==2)
  z4 <- filter(df, deviceid==9 & antennaID==2 | deviceid==17 & antennaID==1)
  z5 <- filter(df, deviceid==17 & antennaID==2 | deviceid==18 & antennaID==1)
  z6 <- filter(df, deviceid==18 & antennaID==2 | deviceid==19 & antennaID==1)
  
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


m1_z1 <- find_zone(mouse1)
m2_z1 <- find_zone(mouse2)
m4_z1 <- find_zone(mouse4)
m7_z1 <- find_zone(mouse7)
m8_z1 <- find_zone(mouse8)

mzone <- rbind(m1_z1, m2_z1, m4_z1, m7_z1, m8_z1)

mzone <- mzone %>%
  arrange(ms)

## zone transition plot
m1_z2 <- m1_z1 %>%
  filter(ms < 3600000)
m1_t1 <- data.frame(
  start = m1_z2$ms,
  id = m1_z2$mouse,
  zone = m1_z2$zone
  )
m1_t1$end <- lead(m1_t1$start)
m1_t1$end[2633]<-130393530
m1_t1 <- rbind(m1_t1[1,],
               m1_t1[m1_t1$zone!=lag(m1_t1$zone),])[-2,]
m1_t1$end <- lead(m1_t1$start)

m2_z2 <- m2_z1 %>%
  filter(ms < 3600000)
m2_t1 <- data.frame(
  start = m2_z2$ms,
  id = m2_z2$mouse,
  zone = m2_z2$zone
  )
m2_t1$end <- lead(m2_t1$start)
m2_t1$end[1653]<-130388629
m2_t1 <- rbind(m2_t1[1,],
               m2_t1[m2_t1$zone!=lag(m2_t1$zone),])[-2,]
m2_t1$end <- lead(m2_t1$start)

m4_z2 <- m4_z1 %>%
  filter(ms < 3600000)
m4_t1 <- data.frame(
  start = m4_z2$ms,
  id = m4_z2$mouse,
  zone = m4_z2$zone
  )
m4_t1$end <- lead(m4_t1$start)
m4_t1$end[1762]<-130358179
m4_t1 <- rbind(m4_t1[1,],
               m4_t1[m4_t1$zone!=lag(m4_t1$zone),])[-2,]
m4_t1$end <- lead(m4_t1$start)

m7_z2 <- m7_z1 %>%
  filter(ms < 3600000)
m7_t1 <- data.frame(
  start = m7_z2$ms,
  id = m7_z2$mouse,
  zone = m7_z2$zone
  )
m7_t1$end <- lead(m7_t1$start)
m7_t1$end[12041]<-130389211
m7_t1 <- rbind(m7_t1[1,],
               m7_t1[m7_t1$zone!=lag(m7_t1$zone),])[-2,]
m7_t1$end <- lead(m7_t1$start)

m8_z2 <- m8_z1 %>%
  filter(ms < 3600000)
m8_t1 <- data.frame(
  start = m8_z2$ms,
  id = m8_z2$mouse,
  zone = m8_z2$zone
  )
m8_t1$end <- lead(m8_t1$start)
m8_t1$end[5693]<-130356408
m8_t1 <- rbind(m8_t1[1,],
               m8_t1[m8_t1$zone!=lag(m8_t1$zone),])[-2,]
m8_t1$end <- lead(m8_t1$start)

m_t1 <- rbind(m1_t1,
              m2_t1,
              m4_t1,
              m7_t1,
              m8_t1)
m_t1 <- as.data.frame(m_t1)

m_t1$end[5692] <- 130356364

m_t1 <- m_t1 %>%
  group_by(id) %>%
  mutate(trans=row_number())

ggplot(m_t1, aes(x=start, y=trans, color=factor(id))) +
         geom_line()


## tube error function
tube_errors <- function(df, pair1=c(1,2), pair2=c(3,4), pair3=c(5,6), pair4=c(7,8),
                        pair5=c(9,10), pair6=c(11,12), pair7=c(13,14), pair8=c(15,16)){
  
  ids <- c(pair1,pair2,pair3,pair4,pair5,pair6,pair7,pair8)
  codes <- c("1", "3","9", "17", "18","19")
  
  x <- codes[match(df$deviceid, ids)]
  
  row.inds <-
    c(intersect(which(x == "1"), which(lag(x) == "9")),
      intersect(which(x == "9"), which(lag(x) == "1")),
      intersect(which(x == "1"),which(lag(x) == "17")),
      intersect(which(x == "17"), which(lag(x) == "1")),
      intersect(which(x == "1"), which(lag(x) == "19")),
      intersect(which(x == "19"), which(lag(x) == "1")),
      intersect(which(x == "1"),which(lag(x) == "18")),
      intersect(which(x == "18"), which(lag(x) == "1")),
      intersect(which(x == "3"), which(lag(x) == "17")),
      intersect(which(x == "17"), which(lag(x) == "3")),
      intersect(which(x == "3"),which(lag(x) == "18")),
      intersect(which(x == "18"), which(lag(x) == "3")),
      intersect(which(x == "9"), which(lag(x) == "18")),
      intersect(which(x == "18"), which(lag(x) == "9")),
      intersect(which(x == "17"),which(lag(x) == "19")),
      intersect(which(x == "19"), which(lag(x) == "17"))
    )
  
  df$error <- FALSE
  df[row.inds,"error"]<-TRUE
  
  
  return(df)
}

m1_e <- as.data.frame(tube_errors(m1_z1, pair1 = c(1,9), pair2 = c(1,17), pair3 = c(1,18), 
                                  pair4 = c(1,19), pair5 = c(3,17), pair6 = c(3,18), pair7 = c(9,18),
                                  pair8 = c(17,19))) %>% filter(error==TRUE)
m2_e <- as.data.frame(tube_errors(m2_z1, pair1 = c(1,9), pair2 = c(1,17), pair3 = c(1,18), 
                                  pair4 = c(1,19), pair5 = c(3,17), pair6 = c(3,18), pair7 = c(9,18),
                                  pair8 = c(17,19))) %>% filter(error==TRUE)
m4_e <- as.data.frame(tube_errors(m4_z1, pair1 = c(1,9), pair2 = c(1,17), pair3 = c(1,18), 
                                  pair4 = c(1,19), pair5 = c(3,17), pair6 = c(3,18), pair7 = c(9,18),
                                  pair8 = c(17,19))) %>% filter(error==TRUE)
m7_e <- as.data.frame(tube_errors(m7_z1, pair1 = c(1,9), pair2 = c(1,17), pair3 = c(1,18), 
                                  pair4 = c(1,19), pair5 = c(3,17), pair6 = c(3,18), pair7 = c(9,18),
                                  pair8 = c(17,19))) %>% filter(error==TRUE)
m8_e <- as.data.frame(tube_errors(m8_z1, pair1 = c(1,9), pair2 = c(1,17), pair3 = c(1,18), 
                                  pair4 = c(1,19), pair5 = c(3,17), pair6 = c(3,18), pair7 = c(9,18),
                                  pair8 = c(17,19))) %>% filter(error==TRUE)

## error plot
e_plot <- rbind(m1_e,
                m2_e, 
                m4_e, 
                m7_e,
                m8_e)

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
  facet_grid(rows = vars(mouse), cols = vars(date))
show(e_plot)
