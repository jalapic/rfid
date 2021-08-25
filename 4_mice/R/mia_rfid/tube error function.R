## tube error function

tube_errors <- function(df, pair1=c(1,2), pair2=c(3,4)){
  
  ids <- c(pair1,pair2)
  codes <- c("3","19", "17", "9")
  
  x <- codes[match(df$deviceid, ids)]
  
  row.inds <-
    c(intersect(which(x == "3"), which(lag(x) == "19")),
      intersect(which(x == "19"), which(lag(x) == "3")),
      intersect(which(x == "17"),which(lag(x) == "9")),
      intersect(which(x == "9"), which(lag(x) == "17"))
    )
  
  df$error <- FALSE
  df[row.inds,"error"]<-TRUE
  
  
  return(df)
}

mouse1_error <- as.data.frame(tube_errors(mouse1, pair1 = c(3,19), pair2 = c(17,9))) %>% filter(error==TRUE)
mouse2_error <- as.data.frame(tube_errors(mouse2, pair1 = c(3,19), pair2 = c(17,9))) %>% filter(error==TRUE)
mouse3_error <- as.data.frame(tube_errors(mouse3, pair1 = c(3,19), pair2 = c(17,9))) %>% filter(error==TRUE)
mouse4_error <- as.data.frame(tube_errors(mouse4, pair1 = c(3,19), pair2 = c(17,9))) %>% filter(error==TRUE)