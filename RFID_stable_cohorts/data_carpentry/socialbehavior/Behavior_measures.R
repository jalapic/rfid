#Saving all behavior measures


#libraries 
library(compete)
library(PlayerRatings)
library(tidyverse)

# Functions

get_matrix <- function(df){
  df <-df[df$score==1,]
  mat <-compete::org_matrix(compete::get_wl_matrix(df[,c('winner','loser')]),
                            method = "ds")
  return(mat)
}

# Calculate despotism
despotism <- function(x) {
  rev(sort(round(100*(rowSums(x)/sum(x)),2)))
}


# Calculate p-value from steepness test
getStp.pval <- function(x){
  a <- steepness::steeptest(x,rep=1000)
  return( sum(a$Stpsim>=a$Stp)/1000 )
}


# Make dataframe out of David's Scores
get_dsdf <- function(d){
  d.df <- data.frame(rank=1:length(d), ds = rev(d[order(d)]))
  return(d.df)
}


#glicko
get_glickos <- function(aa, cval=3){
  PlayerRatings::glicko(aa %>% 
                          mutate(event=row_number()) %>% 
                          select(event,winner,loser,score),
                        history=T,plot=F,cval=cval)
}






## Read in Data
df <- read_csv("RFID_stable_cohorts/data_clean/socialbehavior/WL_data.csv")
head(df)

df.groupx <- df %>% split(.$cohort)

# 1. Make Win-Loss Sociomatrices
df.groupxx <- df.groupx %>% 
  map(~ select(., winner,loser,score)) %>% 
  map(~ as.data.frame(.))

m.wl <- df.groupxx %>% 
  map(get_matrix)


# 2. Make Binary Sociomatrices
m.di <- m.wl %>% 
  map(get_di_matrix)


# 3. Calculate modifed h'
m.dv <- lapply(m.wl,devries)
m.dv.p <- lapply(m.dv, function(x) x$`h-modified`) %>% unlist
m.dv.pval <- lapply(m.dv, function(x) x$`p-value`) %>% unlist


# 4. Calculate directional consistency
m.dc <- lapply(m.wl,dc_test)
m.dc.p <- lapply(m.dc, function(x) x$DC) %>% unlist
m.dc.pval <- lapply(m.dc, function(x) x$`DC.pvalue`) %>% unlist


# 5. Steepness
m.st.p <- lapply(m.wl, steepness::getStp) %>% unlist
m.st.pval <- lapply(m.wl, getStp.pval) %>% unlist


# 6. Triangle transitivity
m.tt <- lapply(m.di,ttri_test)
m.tt.p <- lapply(m.tt, function(x) x$ttri) %>% unlist
m.tt.pval <- lapply(m.tt, function(x) x$pval) %>% unlist



# 7. Despotism
m.d <- lapply(m.wl,despotism)
m.d.val <- lapply(m.d, function(x) x[[1]]) %>% unlist
m.d.val


# 8. Gini-coefficients 
gcw <- lapply(m.wl, function(x) ineq::Gini(rowSums(x))) %>% unlist() #GC Wins
gcl <- lapply(m.wl, function(x) ineq::Gini(colSums(x))) %>% unlist() #GC Losses




### Save all results data to results folder

## Hierarchy Results
data.frame(
  'cohort' = c(1:8),
  'hvalues' = m.dv.p,
  'dc' = m.dc.p,
  'steepness' = m.st.p,
  'ttri' = m.tt.p,
  'despotism' = m.d.val/100,
  'gini.win' = gcw,
  'gini.lose'= gcl,
  'hvalue.pval' = m.dv.pval,
  'dc.pval' = m.dc.pval,
  'steep.pval' = m.st.pval,
  'ttri.pval' = m.tt.pval
) -> resultsdf

#round
resultsdf[,2:8]  <- round(resultsdf[,2:8],2)
 
write.csv(resultsdf, "RFID_stable_cohorts/data_clean/socialbehavior/hierarchystats.csv", row.names = F)


# DS ranking 

dss <- lapply(m.wl, function(x) compete::ds(x,norm = F)) # each cohort's David's Scores
dss.dfs <- lapply(dss, get_dsdf)  %>%  map(~mutate(., mouse = rownames(.))) # put into dataframe
dss.df.all <- do.call('rbind', Map(cbind, dss.dfs, cohort = c(1:8))) 

# Plot
dss.df.all %>% filter(cohort != "6") %>% 
  group_by(rank) %>% 
  summarise(median = median(ds),
            lqr = quantile(ds,.25),
            uqr = quantile(ds,.75)
  ) -> ds.long.summary


  ggplot() + 
    geom_line(data=dss.df.all, aes(x=rank, y=ds, group=cohort), alpha=.3, color="gray57") +
    theme_classic() +
    xlab("Mouse Rank") +
    ylab("David's Scores") +
    geom_hline(yintercept=0, lty=2, color="red", alpha=.5) +
    geom_errorbar(data=ds.long.summary, 
                  aes(x=rank, ymin=lqr, ymax=uqr), width=0.0, size=1, color="firebrick") +
    geom_point(data=ds.long.summary, 
               aes(x=rank, y=median), size=3, shape=21, fill="white")+
    scale_x_continuous(breaks=1:6)


## get ds_rank df
  
  dsrank<-dss.df.all %>% 
    mutate(ds_rank=rank) %>% 
    select(-rank) 
  
  
  #glicko 
  df.glickos <-  lapply(df.groupxx, get_glickos, cval=3)

  
  glickorank <-lapply(df.glickos,function(x) x$ratings %>%
                       mutate(id=Player,
                              glicko_rank=row.names(.)) %>% 
                       select(id,glicko_rank)) %>% 
    map2_df (.,names(.),~mutate(.x,cohort=.y)) 
  
colnames(glickorank)[1] <- "mouse"

  dom_ranks <- dsrank %>% full_join(glickorank)
head(dom_ranks)  


write.csv(dom_ranks, "RFID_stable_cohorts/data_clean/socialbehavior/rank_data.csv", row.names = F)
