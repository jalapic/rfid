### Getting every n numbers

x <- c(1,3,5,12,16,32,1512,12,1232,23,33,111)
x #original vector

x1 <- x[-c(1:2)]
x1 #starting with 3rd integer

x[seq(1, length(x), 3)] #gets 1st, 4th, 7th, 10th etc.

x1[seq(1, length(x1), 3)] #gets 3rd, 6th, 9th, 12th...


x1[seq(1, length(x1), 3)] - x[seq(1, length(x), 3)] 

interval3 <- function(x){
  x1 <- x[-c(1:2)]
 out <-  x1[seq(1, length(x1), 3)] - x[seq(1, length(x), 3)] 
  return(out)
  
}
interval3(x)
