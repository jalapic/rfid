### Checking cohort 3, batch B

### CLEAN Functions
library(tidyverse)
library(lubridate)

## read in transition types csv
ddf <- read_csv("RFID_stable_cohorts/data_carpentry/transitions.csv")




#### Bring in Data

# from df_code1.R
coh3 <- batchB_clean %>% filter(cohort==3)

seconds <- as.numeric(as.POSIXct(coh3$datetimestamp, format="%d.%m.%Y %H:%M:%S:%OS"))
millisecs <- as.numeric(paste(seconds, substr(coh3$datetimestamp,21,24), sep="."))*1000
coh3$ms <- millisecs-min(millisecs)



plot(coh3$ms)
           

table(coh3$mouse) 
l3 <- split(coh3, coh3$mouse)
l3 <- lapply(l3, make_df)
l3 <- lapply(l3, add_type)
unlist(lapply(l3, function(x) sum(is.na(x$type)))) #
lapply(l3, function(x) table(x$type))  # 



# # from df_code1.R
# coh4 <- batchB_clean %>% filter(cohort==4)
# coh4$ms <- coh4$cantimestamp
# 
# table(coh4$mouse) 
# l4 <- split(coh4, coh4$mouse)
# l4 <- lapply(l4, make_df)
# l4 <- lapply(l4, add_type)
# unlist(lapply(l4, function(x) sum(is.na(x$type)))) # some missing
# lapply(l4, function(x) table(x$type))  # why so many errors?
# 

## Notice that "tt" we have are in the range of 3000-6000 per animal - what proportion of these are follows?
## Need to figure out how to leverage both back_tag and leg_tag.




### Follows

## collapse to one time if within eg 5 ms of each other

# for one transition type
  # vecs <- map(l3, ~ .x %>% filter(trans == "2-1:2-2") %>% pull(ms))
  # vecs0 <- which(unlist(lapply(vecs, length))==0)
  # vecs1 <- drop_els(vecs, vecs0)
  # new_pairs(vecs1, window=1)
  # 
  # 
  # vecs <- map(l3, ~ .x %>% filter(trans == "9-1:9-2") %>% pull(ms))
  # vecs0 <- which(unlist(lapply(vecs, length))==0)
  # vecs1 <- drop_els(vecs, vecs0)
  # new_pairs(vecs1, window=500)
  # new_pairs(vecs1, window=10000)

  
out <- map_dfr(tubetrans, ~ get_pairs_df(l3, tt = .x, win = 500), .id = "tubetrans")
out  
rows <- which(out[,2]>=out[,3])
out[rows,]


compete::org_matrix(compete::get_wl_matrix(out[c(5,4)]),method='ds')
compete::isi13(compete::org_matrix(compete::get_wl_matrix(out[c(5,4)])))
compete::get_di_matrix(compete::isi13(compete::org_matrix(compete::get_wl_matrix(out[c(5,4)])))$best_matrix)
compete::devries(compete::get_wl_matrix(out[c(5,4)]))




###

# from df_code1.R

# cohort 3
coh3 <- batchB_clean %>% filter(cohort==3)

# putting into continuous milliseconds
seconds <- as.numeric(as.POSIXct(coh3$datetimestamp, format="%d.%m.%Y %H:%M:%S:%OS"))
millisecs <- as.numeric(paste(seconds, substr(coh3$datetimestamp,21,24), sep="."))*1000
coh3$ms <- millisecs-min(millisecs)

# get vectors of tube transitions per mouse
l3 <- split(coh3, coh3$mouse)
l3 <- lapply(l3, make_df)
l3 <- lapply(l3, add_type)
unlist(lapply(l3, function(x) sum(is.na(x$type)))) #
lapply(l3, function(x) table(x$type))  # 

# get pairs of tube transitions within a window
out <- map_dfr(tubetrans, ~ get_pairs_df(l3, tt = .x, win = 500), .id = "tubetrans")
out  

# hierarchy dynamics
compete::org_matrix(compete::get_wl_matrix(out[c(5,4)]),method='ds')
compete::isi13(compete::org_matrix(compete::get_wl_matrix(out[c(5,4)])))
compete::get_di_matrix(compete::isi13(compete::org_matrix(compete::get_wl_matrix(out[c(5,4)])))$best_matrix)
compete::devries(compete::get_wl_matrix(out[c(5,4)]))




# cohort 4
coh4 <- batchB_clean %>% filter(cohort==4)

# putting into continuous milliseconds
seconds <- as.numeric(as.POSIXct(coh4$datetimestamp, format="%d.%m.%Y %H:%M:%S:%OS"))
millisecs <- as.numeric(paste(seconds, substr(coh4$datetimestamp,21,24), sep="."))*1000
coh4$ms <- millisecs-min(millisecs)

# get vectors of tube transitions per mouse
l4 <- split(coh4, coh4$mouse)
l4 <- lapply(l4, make_df)
l4 <- lapply(l4, add_type)
unlist(lapply(l4, function(x) sum(is.na(x$type)))) #
lapply(l4, function(x) table(x$type))  # 

# get pairs of tube transitions within a window
out <- map_dfr(tubetrans2, ~ get_pairs_df(l4, tt = .x, win = 500), .id = "tubetrans")
out  

rows <- which(out[,2]>=out[,3])
out[rows,]


# hierarchy dynamics
compete::org_matrix(compete::get_wl_matrix(out[c(5,4)]),method='ds')
compete::isi13(compete::org_matrix(compete::get_wl_matrix(out[c(5,4)])))
compete::get_di_matrix(compete::isi13(compete::org_matrix(compete::get_wl_matrix(out[c(5,4)])))$best_matrix)
compete::devries(compete::get_wl_matrix(out[c(5,4)]))

compete::ttri_test(compete::get_wl_matrix(out[c(5,4)]))



# cohort 5
coh5 <- batchC_clean %>% filter(cohort==5)

# putting into continuous milliseconds
seconds <- as.numeric(as.POSIXct(coh5$datetimestamp, format="%d.%m.%Y %H:%M:%S:%OS"))
millisecs <- as.numeric(paste(seconds, substr(coh5$datetimestamp,21,24), sep="."))*1000
coh5$ms <- millisecs-min(millisecs)

# get vectors of tube transitions per mouse
l5 <- split(coh5, coh5$mouse)
l5 <- lapply(l5, make_df)
l5 <- lapply(l5, add_type)
unlist(lapply(l5, function(x) sum(is.na(x$type)))) #
lapply(l5, function(x) table(x$type))  # 

# get pairs of tube transitions within a window
out <- map_dfr(tubetrans, ~ get_pairs_df(l5, tt = .x, win = 500), .id = "tubetrans")
out  

# hierarchy dynamics
compete::org_matrix(compete::get_wl_matrix(out[c(5,4)]),method='ds')
compete::isi13(compete::org_matrix(compete::get_wl_matrix(out[c(5,4)])))
compete::get_di_matrix(compete::isi13(compete::org_matrix(compete::get_wl_matrix(out[c(5,4)])))$best_matrix)
compete::devries(compete::get_wl_matrix(out[c(5,4)]))



# cohort 6
coh6 <- batchC_clean %>% filter(cohort==6)

# putting into continuous milliseconds
seconds <- as.numeric(as.POSIXct(coh6$datetimestamp, format="%d.%m.%Y %H:%M:%S:%OS"))
millisecs <- as.numeric(paste(seconds, substr(coh6$datetimestamp,21,24), sep="."))*1000
coh6$ms <- millisecs-min(millisecs)

# get vectors of tube transitions per mouse
l6 <- split(coh6, coh6$mouse)
l6 <- lapply(l6, make_df)
l6 <- lapply(l6, add_type)
unlist(lapply(l6, function(x) sum(is.na(x$type)))) #
lapply(l6, function(x) table(x$type))  # 

# get pairs of tube transitions within a window
out <- map_dfr(tubetrans2, ~ get_pairs_df(l6, tt = .x, win = 500), .id = "tubetrans")
out  

# hierarchy dynamics
compete::org_matrix(compete::get_wl_matrix(out[c(5,4)]),method='ds')
compete::isi13(compete::org_matrix(compete::get_wl_matrix(out[c(5,4)])))
compete::get_di_matrix(compete::isi13(compete::org_matrix(compete::get_wl_matrix(out[c(5,4)])))$best_matrix)
compete::devries(compete::get_wl_matrix(out[c(5,4)]))

compete::ttri_test(compete::get_wl_matrix(out[c(5,4)]))



# cohort 7
coh7 <- batchD_clean %>% filter(cohort==7)

# putting into continuous milliseconds
seconds <- as.numeric(as.POSIXct(coh7$datetimestamp, format="%d.%m.%Y %H:%M:%S:%OS"))
millisecs <- as.numeric(paste(seconds, substr(coh7$datetimestamp,21,24), sep="."))*1000
coh7$ms <- millisecs-min(millisecs)

# get vectors of tube transitions per mouse
l7 <- split(coh7, coh7$mouse)
l7 <- lapply(l7, make_df)
l7 <- lapply(l7, add_type)
unlist(lapply(l7, function(x) sum(is.na(x$type)))) #
lapply(l7, function(x) table(x$type))  # 

# get pairs of tube transitions within a window
out <- map_dfr(tubetrans, ~ get_pairs_df(l7, tt = .x, win = 500), .id = "tubetrans")
out  

# hierarchy dynamics
compete::org_matrix(compete::get_wl_matrix(out[c(5,4)]),method='ds')
compete::isi13(compete::org_matrix(compete::get_wl_matrix(out[c(5,4)])))
compete::get_di_matrix(compete::isi13(compete::org_matrix(compete::get_wl_matrix(out[c(5,4)])))$best_matrix)
compete::devries(compete::get_wl_matrix(out[c(5,4)]))



# cohort 8
coh8 <- batchD_clean %>% filter(cohort==8)

# putting into continuous milliseconds
seconds <- as.numeric(as.POSIXct(coh8$datetimestamp, format="%d.%m.%Y %H:%M:%S:%OS"))
millisecs <- as.numeric(paste(seconds, substr(coh8$datetimestamp,21,24), sep="."))*1000
coh8$ms <- millisecs-min(millisecs)

# get vectors of tube transitions per mouse
l8 <- split(coh8, coh8$mouse)
l8 <- lapply(l8, make_df)
l8 <- lapply(l8, add_type)
unlist(lapply(l8, function(x) sum(is.na(x$type)))) #
lapply(l8, function(x) table(x$type))  # 

# get pairs of tube transitions within a window
out <- map_dfr(tubetrans2, ~ get_pairs_df(l8, tt = .x, win = 500), .id = "tubetrans")
out  

# hierarchy dynamics
compete::org_matrix(compete::get_wl_matrix(out[c(5,4)]),method='ds')
compete::isi13(compete::org_matrix(compete::get_wl_matrix(out[c(5,4)])))
compete::get_di_matrix(compete::isi13(compete::org_matrix(compete::get_wl_matrix(out[c(5,4)])))$best_matrix)
compete::devries(compete::get_wl_matrix(out[c(5,4)]))

compete::ttri_test(compete::get_wl_matrix(out[c(5,4)]))

compete::ds(compete::get_wl_matrix(out[c(5,4)]))


<<<<<<< HEAD



# cohort 9
coh9 <- batchE_clean %>% filter(cohort==9)

# putting into continuous milliseconds
seconds <- as.numeric(as.POSIXct(coh9$datetimestamp, format="%d.%m.%Y %H:%M:%S:%OS"))
millisecs <- as.numeric(paste(seconds, substr(coh9$datetimestamp,21,24), sep="."))*1000
coh9$ms <- millisecs-min(millisecs)

# get vectors of tube transitions per mouse
l9 <- split(coh9, coh9$mouse)
l9 <- lapply(l9, make_df)
l9 <- lapply(l9, add_type)
unlist(lapply(l9, function(x) sum(is.na(x$type)))) #
lapply(l9, function(x) table(x$type))  # 

# get pairs of tube transitions within a window
out <- map_dfr(tubetrans, ~ get_pairs_df(l9, tt = .x, win = 500), .id = "tubetrans")
out  

# hierarchy dynamics
compete::org_matrix(compete::get_wl_matrix(out[c(5,4)]),method='ds')
compete::isi13(compete::org_matrix(compete::get_wl_matrix(out[c(5,4)])))
compete::get_di_matrix(compete::isi13(compete::org_matrix(compete::get_wl_matrix(out[c(5,4)])))$best_matrix)
compete::devries(compete::get_wl_matrix(out[c(5,4)]))

compete::ttri_test(compete::get_wl_matrix(out[c(5,4)]))

compete::ds(compete::get_wl_matrix(out[c(5,4)]))





# cohort 10
coh10 <- batchE_clean %>% filter(cohort==10)

# putting into continuous milliseconds
seconds <- as.numeric(as.POSIXct(coh10$datetimestamp, format="%d.%m.%Y %H:%M:%S:%OS"))
millisecs <- as.numeric(paste(seconds, substr(coh10$datetimestamp,21,24), sep="."))*1000
coh10$ms <- millisecs-min(millisecs)

# get vectors of tube transitions per mouse
l10 <- split(coh10, coh10$mouse)
l10 <- lapply(l10, make_df)
l10 <- lapply(l10, add_type)
unlist(lapply(l10, function(x) sum(is.na(x$type)))) #
lapply(l10, function(x) table(x$type))  # 

# get pairs of tube transitions within a window
out <- map_dfr(tubetrans2, ~ get_pairs_df(l10, tt = .x, win = 500), .id = "tubetrans")
out  

# hierarchy dynamics
compete::org_matrix(compete::get_wl_matrix(out[c(5,4)]),method='ds')
compete::isi13(compete::org_matrix(compete::get_wl_matrix(out[c(5,4)])))
compete::get_di_matrix(compete::isi13(compete::org_matrix(compete::get_wl_matrix(out[c(5,4)])))$best_matrix)
compete::devries(compete::get_wl_matrix(out[c(5,4)]))

compete::ttri_test(compete::get_wl_matrix(out[c(5,4)]))

compete::ds(compete::get_wl_matrix(out[c(5,4)]))
=======
wlmat <- compete::org_matrix(compete::get_wl_matrix(out[c(5,4)]),method='ds')

rowSums(wlmat)/sum(wlmat)
compete::ds(wlmat, norm=T)
>>>>>>> 8b16cf43f9bada16e6b115a6d877b0587fb73506
