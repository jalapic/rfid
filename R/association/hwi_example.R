### Primer on Association Indices

library(assocInd)
library(tidyverse)


# Half-Weight Association Index
?HWI

#' x	 Number of times individuals a and b were observed together
#' Ya	 Number of times individual a was observed without b
#' Yb	 Number of times individual b was observed without a
#' Yab Number of times individuals a and b were observed at the same time but not associating
#' 

# example sort of dataset

df <- data.frame(
  period = c(1,1,2,3,4,5,5,6,6,7,7,8,9,10,10,11),
  id = c("A","B","B","A","A","A","B","A","B","A","B","A","A","A","B","B")
)

df

# here we have 11 time periods.
# imagine there is only one zone
# we have denoted if in each time period we see "A" or "B" or neither.
# here we don't have a Yab as never were they both present but not associating
# Let's calculate x, Ya, Yb

get_x <- function(df){
split(df, df$period) %>%
  map(~ .$id) %>%
  map(function(x) c("A","B") %in% x) %>%
  map(sum) %>%
  keep(~ .x == 2) %>%
  length()
}

get_x(df)


get_Ya <- function(df){
  split(df, df$period) %>%
    map(~ .$id) %>%
    map(function(x) c("A","B") %in% x) %>%
    map(as.numeric) %>%
    keep(function(x) x[1] > x[2]) %>%
    length()
}

get_Yb <- function(df){
  split(df, df$period) %>%
    map(~ .$id) %>%
    map(function(x) c("A","B") %in% x) %>%
    map(as.numeric) %>%
    keep(function(x) x[2] > x[1]) %>%
    length()
}


get_HWI <- function(df){
assocInd::HWI(x = get_x(df), Ya = get_Ya(df),
              Yb = get_Yb(df), Yab = 0)
}

df

get_HWI(df)

# [1] 0.6250000 0.1711633   HWI association index + std.err.





### Redoing the above, but this time adding in "zones".

ddf <- data.frame(
  period = c(1,1,2,2,3,4,4,5,5,6,6,7,7,8,8,9,10,10),
  id = c("A","B","A","B","B","A","B","A","B",
         "A","B","A","B","A","B","B","A","B"),
  zone = c("A","A","A","B","A","C","A","C","C","D",
           "B","B","C","C","C","A","A","A")
)

ddf







## THINGS TO THINK ABOUT

# Is Converting Continuous time data to 'bins' for this association appropriate?
# What if an animal is in more than 1 zone during same 'period' ?
# how does changing width/duration of bins change HWI ?

# could plot HWI over time
# but how do significance tests ?


# construct association matrix of time spent together
# construct association matrix of periods associating

