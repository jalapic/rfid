library(tidyverse)

## data 
temp <- list.files(path="RFID_stable_cohorts/data_raw/tracking_behavior/",pattern="*.csv")
xfiles <- lapply(temp, function(x) read_csv2(paste0("RFID_stable_cohorts/data_raw/tracking_behavior/",x)) )                 
lapply(xfiles, head)
lapply(xfiles, colnames)

data <- do.call(rbind, xfiles)
data$data <- as.character(data$data)
data$date <- format(as.POSIXct(data$datetimestamp,format="%d.%m.%Y %H:%M:%S:%OS"),"%m.%d.%Y")
data$date <- as.Date(data$date, "%m.%d.%Y")

#grouping batch by date because Tyler did reuse some tag ids in repeat runs
data$batch <- ifelse(data$date >= "2021-09-27" & data$date < "2021-10-18", "A", NA) #09/27-10/07 BATCH A
data$batch <- ifelse(data$date >= "2021-10-18" & data$date < "2021-11-08", "B", data$batch) #10/18-10/28 BATCH B
data$batch <- ifelse(data$date >= "2021-11-08" & data$date < "2021-11-22", "C", data$batch) #11/08-11/18 BATCH C
data$batch <- ifelse(data$date >= "2021-11-22" & data$date < "2022-01-17", "D", data$batch) #11/22-12/02 BATCH D
data$batch <- ifelse(data$date >= "2022-01-17" & data$date < "2022-01-28", "E", data$batch) #01/17-01/27 BATCH d

table(data$batch)

# get id data and join with tracking 
id <- read_csv("RFID_stable_cohorts/data_raw/id_data.csv")
id$back_tag <- as.character(id$back_tag)
id$leg_tag <- as.character(id$leg_tag)

id_data <- id %>% select(mouse, cohort, back_tag, leg_tag)




######BATCH A PRE-PROCESSING
id <- id_data %>%
  pivot_longer(cols =3:4) %>% filter(cohort %in% c(1,2)) %>% 
  select(mouse, tag= name,cohort, data = value)

id %>% as.data.frame() %>% .$data %>% unique()

data1 <- data %>%
  filter(batch == "A")

batchA <- data1 %>% full_join(id)

ggplot(batchA, aes(x = deviceid, group = cohort, fill = cohort)) + geom_histogram() + facet_grid(cohort ~ .)
#looks like a low but present number of antenna read values for cohort 2... 

table(batchA$cohort, batchA$deviceid)
#there's 1186 reads from antennas 16-21 (cage set up 2) in cohort 1 of batch A (compared to 378,905 total reads)
#there's 1023 reads from antennas 1-9 (cage set up 1) in cohort 2 of batch A (compared to 298,505 total reads)

#QUESTION: SHOULD I JUST REMOVE THESE READS?

#checking number of NA values
sum(is.na(batchA$tag)) # Only 1 na value, remove!
sum(is.na(batchA$data))
sum(is.na(batchA$antennaID))
sum(is.na(batchA$deviceid)) 

batchA %>%
  group_by(cohort, mouse) %>%
  summarize(sum(is.na(tag))) #so no NA values for either cohort but there are 1,821,857 NA rows, guess I'll just remove them? Ask Tyler maybe?

batchA_clean <- batchA %>% filter(!is.na(tag)) 
sum(is.na(batchA_clean$tag))

ggplot(batchA_clean, aes(x = deviceid, group = cohort, fill = as.character(cohort))) + geom_histogram() + facet_grid(cohort ~ .) + labs(fill = "Cohort")


#how well is leg and back tag being read? 
table(batchA_clean$tag) #this looks pretty good! pretty equal on leg and back tags




######BATCH B PRE-PROCESSING
id <- id_data %>%
  pivot_longer(cols =3:4) %>% filter(cohort %in% c(3,4)) %>% 
  select(mouse, tag= name,cohort, data = value)


data2 <- data %>%
  filter(batch == "B")
batchB <-  data2 %>% full_join(id)

ggplot(batchB, aes(x = deviceid, group = cohort, fill = cohort)) + geom_histogram() + facet_grid(cohort ~ .)
#no opposite cage setups appear in this cohort! woohoo!

table(batchB$cohort, batchB$deviceid)
#no opposite cage setup reads in either of the cohorts

#checking number of NA values
sum(is.na(batchB$tag)) # Only 1 na value, remove!
sum(is.na(batchB$data))
sum(is.na(batchB$antennaID))
sum(is.na(batchB$deviceid)) 

batchB %>%
  group_by(cohort, mouse) %>%
  summarize(sum(is.na(tag))) 

batchB_clean <- batchB %>% filter(!is.na(tag)) 
sum(is.na(batchB_clean$tag))

Bplot <- ggplot(batchB_clean, aes(x = deviceid, group = cohort, fill = as.character(cohort))) + geom_histogram() + facet_grid(cohort ~ .)+ labs(fill = "Cohort")

#how well is leg and back tag being read? 
table(batchB_clean$tag) #for this group there are about 70,000 more back tag reads than leg tag reads! but still 246,773 leg tag reads



######BATCH C PRE-PROCESSING
id <- id_data %>%
  pivot_longer(cols =3:4) %>% filter(cohort %in% c(5,6)) %>% 
  select(mouse, tag= name,cohort, data = value)


data3 <- data %>%
  filter(batch == "C")
batchC <- data3 %>% full_join(id)

ggplot(batchC, aes(x = deviceid, group = cohort, fill = cohort)) + geom_histogram() + facet_grid(cohort ~ .)
#no opposite cage setups appear in this cohort! woohoo!

table(batchC$cohort, batchC$deviceid)
#just ONE opposite cage read in cohort 6 (device id 1)
#I definitely think I should remove this value since it's only ONE read BUT STILL ASK

#checking number of NA values
sum(is.na(batchC$tag)) # Only one NA value, remove!
sum(is.na(batchC$data))
sum(is.na(batchC$antennaID))
sum(is.na(batchC$deviceid)) 

batchC %>%
  group_by(cohort, mouse) %>%
  summarize(sum(is.na(tag))) 

batchC_clean <- batchC %>% filter(!is.na(tag)) 
sum(is.na(batchC_clean$tag))

Cplot <- ggplot(batchC_clean, aes(x = deviceid, group = cohort, fill = as.character(cohort))) + geom_histogram() + facet_grid(cohort ~ .)+ labs(fill = "Cohort")

#how well is leg and back tag being read? 
table(batchC_clean$tag) #for this group there again about 120,000 less leg tag reads (but still well over 300,000)





######BATCH D PRE-PROCESSING
id <- id_data %>%
  pivot_longer(cols =3:4) %>% filter(cohort %in% c(7,8)) %>% 
  select(mouse, tag= name,cohort, data = value)


data4 <- data %>%
  filter(batch == "D")
batchD <- id %>% full_join(data4)

ggplot(batchD, aes(x = deviceid, group = cohort, fill = cohort)) + geom_histogram() + facet_grid(cohort ~ .)
#no opposite cage setups appear in this cohort! woohoo!

table(batchD$cohort, batchD$deviceid)
#just ONE opposite cage read in cohort 8 (device id 9)
#I definitely think I should remove this value since it's only ONE read BUT STILL ASK


#checking number of NA values
sum(is.na(batchD$tag)) # Only one NA value, remove!
sum(is.na(batchD$data))
sum(is.na(batchD$antennaID))
sum(is.na(batchD$deviceid)) 

batchD %>%
  group_by(cohort, mouse) %>%
  summarize(sum(is.na(tag))) #so no NA values for either cohort but there are 2,529,227 NA rows, guess I'll just remove them? Ask Tyler maybe?

batchD_clean <- batchD %>% filter(!is.na(tag)) 
sum(is.na(batchD_clean$tag)) #this is the lowest number of data points for any group... does this contain the failed cohort because of aggresion?

Dplot <- ggplot(batchD_clean, aes(x = deviceid, group = cohort, fill = as.character(cohort))) + geom_histogram() + facet_grid(cohort ~ .)+ labs(fill = "Cohort")

#how well is leg and back tag being read? 
table(batchD_clean$tag) #about 80,000 more back tag reads than leg tag reads





######BATCH E PRE-PROCESSING
id <- id_data %>%
  pivot_longer(cols =3:4) %>% filter(cohort %in% c(9,10)) %>% 
  select(mouse, tag= name,cohort, data = value)


data5 <- data %>%
  filter(batch == "E")
batchE <- id %>% full_join(data5)

ggplot(batchE, aes(x = deviceid, group = cohort, fill = cohort)) + geom_histogram() + facet_grid(cohort ~ .)
#no opposite cage setups appear in this cohort! woohoo!

table(batchE$cohort, batchE$deviceid)
#no opposite cage setup reads! 
#celebration


#checking number of NA values
sum(is.na(batchE$tag)) 
sum(is.na(batchE$data))
sum(is.na(batchE$antennaID))
sum(is.na(batchE$deviceid)) 

batchE %>%
  group_by(cohort, mouse) %>%
  summarize(sum(is.na(tag))) 

batchE_clean <- batchE %>% filter(!is.na(tag)) 
sum(is.na(batchE_clean$tag)) #also quite a lot less than other cohorts, though more than batchE...

Eplot <- ggplot(batchE_clean, aes(x = deviceid, group = cohort, fill = as.character(cohort))) + geom_histogram() + facet_grid(cohort ~ .)+ labs(fill = "Cohort")

#how well is leg and back tag being read? 
table(batchE_clean$tag) #the back_tag and leg_tag reads are only 50,000 off for this dataset, which is actually quite good for this data


library(ggpubr)
ggarrange(Bplot, Cplot, Dplot, Eplot, nrow = 4)






#Now formatting in ms

batchA_clean$date <- format(as.POSIXct(batchA_clean$datetimestamp,format="%d.%m.%Y %H:%M:%S:%OS"),"%m.%d.%Y")
batchA_clean$time <- sub("^\\S+\\s+", '', batchA_clean$datetimestamp)
xx <- strsplit(batchA_clean$time, split=":")
xxx <- matrix(unlist(xx), ncol = 4, byrow = TRUE)
batchA_clean$ms<-
  (3600000 * as.numeric(xxx[,1])) +
  (60000 * as.numeric(xxx[,2])) +
  (1000 * as.numeric(xxx[,3])) +
  (1 * as.numeric(xxx[,4]))

batchB_clean$date <- format(as.POSIXct(batchB_clean$datetimestamp,format="%d.%m.%Y %H:%M:%S:%OS"),"%m.%d.%Y")
batchB_clean$time <- sub("^\\S+\\s+", '', batchB_clean$datetimestamp)
xx <- strsplit(batchB_clean$time, split=":")
xxx <- matrix(unlist(xx), ncol = 4, byrow = TRUE)
batchB_clean$ms<-
  (3600000 * as.numeric(xxx[,1])) +
  (60000 * as.numeric(xxx[,2])) +
  (1000 * as.numeric(xxx[,3])) +
  (1 * as.numeric(xxx[,4]))

batchC_clean$date <- format(as.POSIXct(batchC_clean$datetimestamp,format="%d.%m.%Y %H:%M:%S:%OS"),"%m.%d.%Y")
batchC_clean$time <- sub("^\\S+\\s+", '', batchC_clean$datetimestamp)
xx <- strsplit(batchC_clean$time, split=":")
xxx <- matrix(unlist(xx), ncol = 4, byrow = TRUE)
batchC_clean$ms<-
  (3600000 * as.numeric(xxx[,1])) +
  (60000 * as.numeric(xxx[,2])) +
  (1000 * as.numeric(xxx[,3])) +
  (1 * as.numeric(xxx[,4]))

batchD_clean$date <- format(as.POSIXct(batchD_clean$datetimestamp,format="%d.%m.%Y %H:%M:%S:%OS"),"%m.%d.%Y")
batchD_clean$time <- sub("^\\S+\\s+", '', batchD_clean$datetimestamp)
xx <- strsplit(batchD_clean$time, split=":")
xxx <- matrix(unlist(xx), ncol = 4, byrow = TRUE)
batchD_clean$ms<-
  (3600000 * as.numeric(xxx[,1])) +
  (60000 * as.numeric(xxx[,2])) +
  (1000 * as.numeric(xxx[,3])) +
  (1 * as.numeric(xxx[,4]))

batchE_clean$date <- format(as.POSIXct(batchE_clean$datetimestamp,format="%d.%m.%Y %H:%M:%S:%OS"),"%m.%d.%Y")
batchE_clean$time <- sub("^\\S+\\s+", '', batchE_clean$datetimestamp)
xx <- strsplit(batchE_clean$time, split=":")
xxx <- matrix(unlist(xx), ncol = 4, byrow = TRUE)
batchE_clean$ms<-
  (3600000 * as.numeric(xxx[,1])) +
  (60000 * as.numeric(xxx[,2])) +
  (1000 * as.numeric(xxx[,3])) +
  (1 * as.numeric(xxx[,4]))


allcohorts <- rbind(batchA_clean, batchB_clean, batchC_clean, batchD_clean, batchE_clean)

table(allcohorts$batch, allcohorts$tag)


#now plotting what cohorts have which cage setup
ggplot(allcohorts, aes(x = deviceid, group = cohort, fill = cohort)) + geom_histogram() + facet_grid(cohort ~ .)



#adding zones to the batches

zone.cagesetup1.2 <- function(df){
  df$row <- 1:nrow(df)
  df$deviceid <- as.numeric(df$deviceid)
  df$antennaID <- as.numeric(df$antennaID)
  
  cage1 <- filter(df, deviceid==1 & antennaID==1)
  t1 <- filter(df, deviceid==1 & antennaID==1 | deviceid==1 & antennaID==2)
  cage2 <- filter(df, deviceid==1 & antennaID==2 | deviceid==2 & antennaID==1)
  t2 <- filter(df, deviceid==2 & antennaID==1 | deviceid==2 & antennaID==2)
  cage3 <- filter(df, deviceid==2 & antennaID==2 | deviceid==3 & antennaID==1 | deviceid==8 & antennaID==1)
  t3 <- filter(df, deviceid==3 & antennaID==1 | deviceid==3 & antennaID==2)
  cage4 <- filter(df, deviceid==3 & antennaID==2 | deviceid==4 & antennaID==1)
  t4 <- filter(df, deviceid==4 & antennaID==1 | deviceid==4 & antennaID==2)
  cage5 <- filter(df, deviceid==4 & antennaID==2 | deviceid==9 & antennaID==2)
  t9 <- filter(df, deviceid==9 & antennaID==2 | deviceid==9 & antennaID==1)
  cage6 <- filter(df, deviceid==9 & antennaID==1 | deviceid==8 & antennaID==2)
  t8 <- filter(df, deviceid==8 & antennaID==1 | deviceid==8 & antennaID==2)
  
  cage1$zone = "1"
  t1$zone = "t1"
  cage2$zone = "2"
  t2$zone = "t2"
  cage3$zone = "3"
  t3$zone = "t3"
  cage4$zone = "4"
  t4$zone = "t4"
  cage5$zone = "5"
  t9$zone = "t8"
  cage6$zone = "6"
  t8$zone = "t9"
  
  zone_df <- rbind(cage1, t1, cage2, t2, cage3, t3, cage4, t4, cage5, t9, cage6, t8)
  zone_df <- arrange(zone_df, row)
  
  x <- zone_df$zone
  x1 <- sort(unique(c(length(x) - cumsum(rle(rev(x))$lengths) + 1, cumsum(rle(x)$lengths) )))
  x1 <- unlist(x1)
  
  zone_df <- zone_df[x1,]
}


#trying this out with cohort 9 (since that cohort is in cage setup 1)
c9 <- allcohorts %>% filter(cohort == 9)
c9.act2 <- zone.cagesetup1.2(c9)

c9.act2.sumplot <- c9.act2 %>%
  group_by(mouse,zone) %>%
  summarize(counts <- n())
colnames(c9.act2.sumplot) <- c("mouse", "zone", "count")
c9.act2.sumplot$mouse <- as.character(c9.act2.sumplot$mouse)
c9.act2.sumplot$zone <- as.character(c9.act2.sumplot$zone)
ggplot(c9.act2.sumplot, aes(mouse, zone)) +
  geom_tile(aes(fill = count)) +
  geom_text(aes(label = count)) +
  scale_fill_gradient(low = "white", high = "red")
#just the cages 
c9.act2.sumplot %>%
  filter(zone == "1" | zone == "2" | zone == "3" | zone == "4" | zone == "5" | zone == "6") %>%
  ggplot(aes(mouse, zone)) +
  geom_tile(aes(fill = count)) +
  geom_text(aes(label = count)) +
  scale_fill_gradient(low = "white", high = "red") + ylab("Zone (cage)")
#just the tubes 
c9.act2.sumplot %>%
  filter(zone == "t1" | zone == "t2" | zone == "t3" | zone == "t4" | zone == "t8" | zone == "t9") %>%
  ggplot(aes(mouse, zone)) +
  geom_tile(aes(fill = count)) +
  geom_text(aes(label = count)) +
  scale_fill_gradient(low = "white", high = "red") + ylab("Zone (tubes)")


#based on the table I'm thinking the dom might be #8, let's look at James's "following" function

