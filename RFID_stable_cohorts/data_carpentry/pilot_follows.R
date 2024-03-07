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
  vecs <- map(l3, ~ .x %>% filter(trans == "2-1:2-2") %>% pull(ms))
  vecs0 <- which(unlist(lapply(vecs, length))==0)
  vecs1 <- drop_els(vecs, vecs0)
  new_pairs(vecs1, window=1)
 
  
  vecs <- map(l3, ~ .x %>% filter(trans == "9-1:9-2") %>% pull(ms))
  vecs0 <- which(unlist(lapply(vecs, length))==0)
  vecs1 <- drop_els(vecs, vecs0)
  new_pairs(vecs1, window=500)
  new_pairs(vecs1, window=10000)

  
out <- map_dfr(tubetrans, ~ get_pairs_df(l3, tt = .x, win = 500), .id = "tubetrans")
out  
  

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



## histogram of diftimes would be good.
# it's just random noise
# result1 <- NULL  
# 
# for(i in 1:length(tubetrans)){
#   result1[[i]] <-  get_pairs_tubes(l3, tubetrans[[i]], win=1000000)
# } 
# 
# ddt <- data.table::rbindlist(map2(result1, tubetrans, ~ mutate(.x, tubetype = .y)))
# ddt$atime <- pmin(ddt$number_from_element1, ddt$number_from_element2)
# ddt$btime <- pmax(ddt$number_from_element1, ddt$number_from_element2)
# ddt$diftime <- ddt$btime - ddt$atime
# ddt$winner <- ifelse(ddt$number_from_element1==ddt$btime, ddt$element1, ddt$element2)
# ddt$loser <- ifelse(ddt$number_from_element1==ddt$atime, ddt$element1, ddt$element2)
# ddt$winner <- names(l3)[ddt$winner]
# ddt$loser <- names(l3)[ddt$loser]
# ddt
# 
# #histogram of difference times
# 
# ggplot(ddt, aes(x=diftime)) + geom_histogram(binwidth = 1000, color='black',fill='white')
# 
# 
# 
# 
# 
# 
# 
