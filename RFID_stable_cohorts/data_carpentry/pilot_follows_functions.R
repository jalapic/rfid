

# function to get back tags in time order
make_df  <- function(df){
  df %>% 
    arrange(ms) %>% 
    filter(tag=="back_tag") %>%
    mutate(id = paste(deviceid,antennaID,sep="-")) %>%
    mutate(idlag = lag(id)) %>%
    mutate(trans = paste(idlag, id, sep=":"))
}

#function to add in transition type
add_type <- function(dfdf){
  dfdf$type <- ddf$type[match(dfdf$trans, ddf$Var1)]
  return(dfdf)
}




#need a step to discard empty elements in this list.
drop_els <- function(lst, ind) {
  if (is.null(ind) || length(ind) == 0) {
    return(lst)
  }
  # Filter out elements at specified indices
  rem <- setdiff(seq_along(lst), ind)
  return(lst[rem])
}




# Types of tube transitions
tubetrans <- c("9-1:9-2", "9-2:9-1",
               "1-1:1-2", "1-2:1-1",
               "2-1:2-2", "2-2:2-1",
               "3-1:3-2", "3-2:3-1",
               "4-1:4-2", "4-2:4-1",
               "8-1:8-2", "8-2:8-1"
)

tubetrans

tubetrans2 <- c("19-1:19-2", "19-2:19-1",
                "16-1:16-2", "16-2:16-1",
                "21-1:21-2", "21-2:21-1",
                "20-1:20-2", "20-2:20-1",
                "17-1:17-2", "17-2:17-1",
                "18-1:18-2", "18-2:18-1"
)


## Find Pairs in vectors of times
find_pairs <- function(vecs, window_size) {
  pairs <- list()
  n <- length(vecs)
  
  # Iterate over all combinations of pairs of vectors
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      vec1 <- vecs[[i]]
      vec2 <- vecs[[j]]
      
      # Iterate over all combinations of values from different vectors
      for (k in 1:length(vec1)) {
        for (l in 1:length(vec2)) {
          if (abs(vec1[k] - vec2[l]) <= window_size) {
            pairs[[paste0("pair_", length(pairs) + 1)]] <- list(
              value1 = vec1[k],
              value2 = vec2[l],
              vector1 = paste0("vec", i),
              vector2 = paste0("vec", j)
            )
          }
        }
      }
    }
  }
  
  return(pairs)
}

### Put all into one function
find_pairs1 <- function(x, w){
  pairs <- find_pairs(x,w)
  pairs_df <- do.call(rbind, pairs)
  pairs_df <- as.data.frame.matrix(pairs_df)
  pairs_df <- as.data.frame(lapply(pairs_df, function(x) unlist(x)))
  
  out <- data.frame(
    value1 = pmin(pairs_df$value1, pairs_df$value2), 
    value2 = pmax(pairs_df$value1, pairs_df$value2),
    vector1 = ifelse(pairs_df$value1 <= pairs_df$value2, pairs_df$vector1, pairs_df$vector2),
    vector2 = ifelse(pairs_df$value1 <= pairs_df$value2, pairs_df$vector2, pairs_df$vector1))
  
  return(out)
}

### Put above into one function (more generalizeable):
get_pairs_df <- function(ll,tt,win=500){
  vecs <- map(ll, ~ .x %>% filter(trans == tt) %>% pull(ms))
  vecs0 <- which(unlist(lapply(vecs, length))==0)
  vecs1 <- drop_els(vecs, vecs0)
  outdf <- find_pairs1(vecs1, w=win)# find pairs
  matchdf <- data.frame(vector = paste0('vec',1:length(vecs1)),
                        mouseid = names(vecs1)
  )
  outdf$vector1<- matchdf$mouseid[match(outdf$vector1, matchdf$vector)]
  outdf$vector2<- matchdf$mouseid[match(outdf$vector2, matchdf$vector)]
  return(outdf)
}



