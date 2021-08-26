library(tidyverse)

## working with pilot data

## loading in data
getwd()
df1 <- read_csv2("5_mice/raw_data/rawdata_group20210823.csv")
df2 <- read_csv2("5_mice/raw_data/rawdata_group20210824.csv")

df <- rbind(df1, df2)

df$data <- as.character(df$data)

## creating ms column

df$date <- format(as.POSIXct(df$datetimestamp,format="%d.%m.%Y %H:%M:%S:%OS"),"%m.%d.%Y")

df$time <- sub("^\\S+\\s+", '', df$datetimestamp)

xx <- strsplit(df$time, split=":")

#xx
xxx <- matrix(unlist(xx), ncol = 4, byrow = TRUE)
#xxx[,1]
#xxx[,2]
#xxx[,3]
#as.numeric(xxx[,4])

df$ms<-
  (3600000 * as.numeric(xxx[,1])) +
  (60000 * as.numeric(xxx[,2])) +
  (1000 * as.numeric(xxx[,3])) +
  (1 * as.numeric(xxx[,4]))

day_1 <- df %>% filter(date == "08.23.2021")

day_1$ms <- as.numeric(unlist(day_1$ms)) - 42341803

day_2 <- df %>% filter(date == "08.24.2021")

day_2$ms <- (as.numeric(unlist(day_2$ms)) + (8.64e+7 * 1)) - 40089792

df <- rbind(day_1, day_2)

df$ms <- as.character(df$ms)

## creating csv for mice id's
dfx <- data.frame(data=c("900133000459716", "900133000459717",
                       "900133000459692","900133000459691",
                       "900133000459728", "900133000459729",
                       "900133000459714", "900133000459715",
                       "900133000459726", "900133000459730"),
                mouse = c(1,1,2,2,4,4,7,7,8,8)
                )
getwd()

write.csv(dfx,"5_mice/raw_data/mouseids.csv", row.names = F)

dfx <- read_csv("5_mice/raw_data/mouseids.csv")

## creating `mouse` column
mouse1 <- subset(df, data %in% dfx[dfx$mouse==1,]$data) %>% mutate(mouse = 1)
mouse2 <- subset(df, data %in% dfx[dfx$mouse==2,]$data) %>% mutate(mouse = 2)
mouse4 <- subset(df, data %in% dfx[dfx$mouse==4,]$data) %>% mutate(mouse = 4)
mouse7 <- subset(df, data %in% dfx[dfx$mouse==7,]$data) %>% mutate(mouse = 7)
mouse8 <- subset(df, data %in% dfx[dfx$mouse==8,]$data) %>% mutate(mouse = 8)

df <- rbind(mouse1 %>% mutate(mouse="1"), 
            mouse2 %>% mutate(mouse="2"), 
            mouse4 %>% mutate(mouse="4"), 
            mouse7 %>% mutate(mouse="7"),
            mouse8 %>% mutate(mouse="8"))

## activity plot
act <- df

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

## tube error function
