## creating mouse column

df <- df[!is.na(df$data),]

dfx<-data.frame(data=c("900133000459724", "900133000459725",
                       "900133000459723","900133000459727",
                       "900133000459722", "900133000459721",
                       "900133000459719", "900133000459693"),
                mouse = c(4,4,3,3,2,2,1,1)
)
getwd()
write.csv(dfx,"raw_data/mouseids.csv", row.names = F)

dfx <- read_csv("raw_data/mouseids.csv")


mouse1 <- subset(df, data %in% dfx[dfx$mouse==1,]$data) %>% mutate(mouse = 1)
mouse2 <- subset(df, data %in% dfx[dfx$mouse==2,]$data) %>% mutate(mouse = 2)
mouse3 <- subset(df, data %in% dfx[dfx$mouse==3,]$data) %>% mutate(mouse = 3)
mouse4 <- subset(df, data %in% dfx[dfx$mouse==4,]$data) %>% mutate(mouse = 4)
