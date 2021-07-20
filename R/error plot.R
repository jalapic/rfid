## error plot

error_plot <- rbind(mouse1_error, 
                    mouse2_error, 
                    mouse3_error, 
                    mouse4_error)

li <- strsplit(error_plot$datetimestamp,split=":")
hrs <- as.numeric(unlist(lapply(li, function(x) substr(x[[1]],12,13))))
mins <- as.numeric(unlist(lapply(li, function(x) x[2])))
secs <- as.numeric(unlist(lapply(li, function(x) x[3])))
ms <- as.numeric(unlist(lapply(li, function(x) x[4])))

time <- strptime(error_plot$datetimestamp, format="%d.%M.%Y %H:%M:%S:%OS")
times <- strftime(time, format="%H:%M:%S:%OS")

error_plot$times <- times
error_plot$hrs <- hrs
error_plot$mins <- mins
error_plot$bin <- ifelse(mins>30, paste0(error_plot$hrs,"b"), paste0(error_plot$hrs, "a"))
error_plot$totalmins <- (error_plot$hrs * 60) + error_plot$mins 

err_plot <- ggplot(error_plot, aes(x=totalmins)) + 
  geom_histogram(binwidth=1,fill='lightseagreen', color='darkgreen') +
  theme_classic()+
  xlab("Time of Day in Minutes") +
  geom_vline(xintercept=c(660, 1380), color='black', lwd=1,lty=2) + 
  facet_grid(rows = vars(mouse), cols = vars(date))

show(err_plot)