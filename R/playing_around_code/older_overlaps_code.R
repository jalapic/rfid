## older overlaps code that we don't use anymore

f1 <- function(x) {
  
  x <- setDT(x)
  Asp <- split(x, by = "mouse")
  
  u <- na.omit(foverlaps(Asp[[1]], setkey(Asp[[2]], start, end)))
  
  if(nrow(u)==0){
    outdf<-data.frame(zone=NA,t1=NA,t2=NA)
  } else {
    
    r <- c()
    
    for (k in 1:nrow(u)) {
      
      p <- cbind(
        t1=pmax(u[k,start],u[k,i.start]), 
        t2=pmin(u[k,end],u[k,i.end])
      )
      
      r[[k]] <- p
      
    }
    
    
    outdf<-data.frame(
      cbind(
        zone = u[, zone],
        do.call('rbind', r)
      )
    )
    
    
  }
  return(outdf)
}


f1(m1_m2_plotz[[1]]) # correct
f1(m1_m2_plotz[[2]]) # returns NA as no overlaps
f1(m1_m2_plotz[[3]]) # correct
f1(m1_m2_plotz[[4]]) # correct


### To do in one step:

f2 <- function(df){
  na.omit(rbindlist(Map(f1, split(setDT(df), by = "zone"))))
}

# good up until here:
f2(m1_m2_plotz)


## other (better) method
