### Checking cohort 3, batch B

### CLEAN Functions
library(tidyverse)
library(lubridate)

## read in transition types csv
ddf <- read_csv("RFID_stable_cohorts/data_carpentry/transitions.csv")




#### Bring in Data

# from df_code1.R
coh3 <- batchB_clean %>% filter(cohort==3)
coh3$ms <- coh3$cantimestamp

table(coh3$mouse) 
l3 <- split(coh3, coh3$mouse)
l3 <- lapply(l3, make_df)
l3 <- lapply(l3, add_type)
unlist(lapply(l3, function(x) sum(is.na(x$type)))) # some missing
lapply(l3, function(x) table(x$type))  # why so many errors?



# from df_code1.R
coh4 <- batchB_clean %>% filter(cohort==4)
coh4$ms <- coh4$cantimestamp

table(coh4$mouse) 
l4 <- split(coh4, coh4$mouse)
l4 <- lapply(l4, make_df)
l4 <- lapply(l4, add_type)
unlist(lapply(l4, function(x) sum(is.na(x$type)))) # some missing
lapply(l4, function(x) table(x$type))  # why so many errors?


## Notice that "tt" we have are in the range of 3000-6000 per animal - what proportion of these are follows?
## Need to figure out how to leverage both back_tag and leg_tag.




### Follows

# for one transition type
  vecs <- map(l3, ~ .x %>% filter(trans == "2-1:2-2") %>% pull(ms))
  vecs0 <- which(unlist(lapply(vecs, length))==0)
  vecs1 <- drop_els(vecs, vecs0)
  new_pairs(vecs1, window=1000)
 
  
  vecs <- map(l3, ~ .x %>% filter(trans == "9-1:9-2") %>% pull(ms))
  vecs0 <- which(unlist(lapply(vecs, length))==0)
  vecs1 <- drop_els(vecs, vecs0)
  new_pairs(vecs1, window=500)
  new_pairs(vecs1, window=10000)

# not sure why this doesn't work:
# out <- map_dfr(tubetrans, ~ get_pairs_df(l3, tt = .x, win = 500), .id = "tubetrans")
# putting in loop instead

get_pairs_tubes <- function(l, tubetype, win) { 
  vecs <- map(l, ~ .x %>% filter(trans == tubetype) %>% pull(ms))
  vecs0 <- which(unlist(lapply(vecs, length))==0)
  vecs1 <- drop_els(vecs, vecs0)
  dd <- new_pairs(vecs1, window=win)
  return(dd)
}

result <- NULL  

for(i in 1:length(tubetrans)){
result[[i]] <-  get_pairs_tubes(l3, tubetrans[[i]], win=1000)
} 


dt <- data.table::rbindlist(map2(result, tubetrans, ~ mutate(.x, tubetype = .y)))
dt$atime <- pmin(dt$number_from_element1, dt$number_from_element2)
dt$btime <- pmax(dt$number_from_element1, dt$number_from_element2)
dt$diftime <- dt$btime - dt$atime
dt$winner <- ifelse(dt$number_from_element1==dt$btime, dt$element1, dt$element2)
dt$loser <- ifelse(dt$number_from_element1==dt$atime, dt$element1, dt$element2)
dt$winner <- names(l3)[dt$winner]
dt$loser <- names(l3)[dt$loser]
dt

compete::org_matrix(compete::get_wl_matrix(dt[,9:10]),method='ds')
compete::isi13(compete::org_matrix(compete::get_wl_matrix(dt[,9:10])))
compete::get_di_matrix(compete::isi13(compete::org_matrix(compete::get_wl_matrix(dt[,9:10])))$best_matrix)
compete::devries(compete::get_wl_matrix(dt[,9:10]))






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
