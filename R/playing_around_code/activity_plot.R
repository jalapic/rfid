getwd()
library(tidyverse)
df<-read_csv2("raw_data/Tracking/rawdata20210607.csv")
df$data <- as.character(df$data)
head(df)

df

df1 <- df[df$data=='900133000459723',] # why does this keep loads of rows with NA NA NA NA NA ?
df1 <- df1[!is.na(df1$deviceid),]

li <- strsplit(df1$datetimestamp,split=":")
hrs <- as.numeric(unlist(lapply(li, function(x) substr(x[[1]],12,13))))
mins <- as.numeric(unlist(lapply(li, function(x) x[2])))
secs <- as.numeric(unlist(lapply(li, function(x) x[3])))
ms <- as.numeric(unlist(lapply(li, function(x) x[4])))

time <- strptime(df1$datetimestamp, format="%d.%M.%Y %H:%M:%S:%OS")
times <- strftime(time, format="%H:%M:%S:%OS")

# make activity log in 30 minute bins...
df1$times <- times
df1$hrs <- hrs
df1$mins <- mins
df1$bin <- ifelse(mins>30, paste0(df1$hrs,"b"), paste0(df1$hrs, "a"))
df1$totalmins <- (df1$hrs * 60) + df1$mins 

11*60

ggplot(df1, aes(x=totalmins)) + 
  geom_histogram(binwidth=20,fill='lightseagreen', color='darkgreen') +
  theme_classic()+
  xlab("Time of Day in Minutes") +
 geom_vline(xintercept=660, color='black', lwd=1,lty=2) 

ggplot(df1, aes(x=totalmins)) + 
  geom_histogram(binwidth=1,fill='lightseagreen', color='darkgreen') +
  theme_classic()+
  xlab("Time of Day in Minutes") +
  geom_vline(xintercept=660, color='black', lwd=1,lty=2)  
