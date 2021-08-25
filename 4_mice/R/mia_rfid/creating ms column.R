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
```


editing ms column to account for number days past
```{r, warning=FALSE, message=FALSE, results='hide'}
day_1 <- df %>% filter(date == "06.04.2021")

day_1$ms <- as.numeric(unlist(day_1$ms)) - 40089792

day_2 <- df %>% filter(date == "06.05.2021")

day_2$ms <- (as.numeric(unlist(day_2$ms)) + (8.64e+7 * 1)) - 40089792

day_3 <- df %>% filter(date == "06.06.2021")

day_3$ms <- (as.numeric(unlist(day_3$ms)) + (8.64e+7 * 2)) - 40089792

day_4 <- df %>% filter(date == "06.07.2021")

day_4$ms <- (as.numeric(unlist(day_4$ms)) + (8.64e+7 * 3)) - 40089792

day_5 <- df %>% filter(date == "06.08.2021")

day_5$ms <- (as.numeric(unlist(day_5$ms)) + (8.64e+7 * 4)) - 40089792

day_6 <- df %>% filter(date == "06.09.2021")

day_6$ms <- (as.numeric(unlist(day_6$ms)) + (8.64e+7 * 5)) - 40089792

day_7 <- df %>% filter(date == "06.10.2021")

day_7$ms <- (as.numeric(unlist(day_7$ms)) + (8.64e+7 * 6)) - 40089792

df <- rbind(day_1, day_2, day_3, day_4, day_5, day_6, day_7)

df$ms <- as.character(df$ms)