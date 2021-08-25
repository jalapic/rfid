# https://stackoverflow.com/questions/68134690/extracting-rows-matching-conditions-based-on-previous-rows-r 

time = 1:22
device =  c(rep(9,8), rep(3,6),rep(17,8))
antenna = c(2,1,2,2,1,1,2,1,2,2,2,2,2,2,1,2,1,2,1,1,1,1)
ID = c("A","A",NA,"B",NA,"B",NA,NA,"B",NA,"B",NA,"C",NA,"A","B",NA,NA,"B",NA,"B",NA)

df <- data.frame(time, device,antenna,ID)

df

#' for each ID, device, antenna combination
#' want to extract that row plus the very next row with
#'  same device/antenna combination but with NA - should it exist

# desired output

#A: 
df[c(1,3,2,5,15,17),]

#B:
df[c(4,7,6,8,9,10,11,12,16,18,19,20,21),]

#C:
df[c(13,14),]


## Idea:
subset(df, df$device==9 & df$antenna==2)
subset(df, df$device==17 & df$antenna==1)
subset(df, df$device==3 & df$antenna==1)
