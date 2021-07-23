## activity plot

zz <- rbind(mouse1 %>% mutate(mouse="m1"), 
            mouse2 %>% mutate(mouse="m2"), 
            mouse3 %>% mutate(mouse="m3"), 
            mouse4 %>% mutate(mouse="m4"))

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

act_plot <-  ggplot(zz, aes(x=totalmins)) + 
  geom_histogram(binwidth=1,fill='lightseagreen', color='darkgreen') +
  theme_classic()+
  xlab("Time of Day in Minutes") +
  geom_vline(xintercept=c(660, 1380), color='black', lwd=1,lty=2) + 
  facet_grid(rows = vars(mouse), cols = vars(date))

show(act_plot)
