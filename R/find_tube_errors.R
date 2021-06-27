
set.seed(1)
df <- data.frame(
  row_number = 1:40, 
  code = sample(LETTERS[1:4],40,T)
)
df

# not allowed:
# A->C
# B->D

# A->C are on rows 6, 21
# B->D are not on any rows.


x <- df$code

# look at what these do
x
lead(x)
lag(x)

which(x == "A")
which(lead(x) == "C")

# intersect gets the common values in each vector:
intersect(which(x == "A"),which(lead(x) == "C"))

# intersect when B leads to D:
intersect(which(x == "B"),which(lead(x) == "D"))  # returns nothing.

# we can combine this to set notifications in a new column:

row.inds <-
  c(intersect(which(x == "A"),which(lead(x) == "C")),
    intersect(which(x == "B"),which(lead(x) == "D"))
  )

row.inds

df$error <- FALSE
df[row.inds,"error"]<-TRUE
df

# need to make sure this code will work if no errors are found...

set.seed(4)

df1 <- data.frame(
  row_number = 1:10, 
  code = sample(LETTERS[1:4],10,T)
)
df1

x <- df1$code
x #notice there are no A->C, or B->D

row.inds <- 
  c(intersect(which(x == "A"),which(lead(x) == "C")),
    intersect(which(x == "B"),which(lead(x) == "D"))
  )

row.inds #integer(0)  - so here, no issues.

# this code seems to work ok, and doesn't do anything odd 
df1$error <- FALSE
df1[row.inds,"error"]<-TRUE
df1   


### Now, make this a bit more generalizable for our case
# i.e. when we have a dataset with numbered tubes.

# the easiest thing is probably to:
# 1. convert the numbered tubes to letters
# 2. make a function to automatically do the above




# let's pretend the pairs of bad tube crossings are 3&15, 10&21.
# this requires you to provide the two pairs of numbers as vectors
# it requires the column with the tube numbers to be called tube

library(tidyverse)

set.seed(1)
df <- data.frame(
  row_number = 1:40, 
  tube = sample(c(3,10,15,21),40,T)
)
df

tube_errors <- function(df, pair1=c(1,2), pair2=c(3,4)){
  
  ids <- c(pair1,pair2)
  codes <- c("A","C", "B", "D")
  
  x <- codes[match(df$tube, ids)]
  
  row.inds <-
    c(intersect(which(x == "A"),which(lead(x) == "C" | lag(x) == "C")),
      intersect(which(x == "B"),which(lead(x) == "D" | lag(x) == "C"))
       )
  
  df$error <- FALSE
  df[row.inds,"error"]<-TRUE
  
  return(df)
  }

df

tube_errors(df, pair1 = c(3,15), pair2 = c(21, 10))

#  seems to work.