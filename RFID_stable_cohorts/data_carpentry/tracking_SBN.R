move<- readRDS("RFID_stable_cohorts/data_clean/cagechangebyzone_day.RDS")
head(move)

mx <- move %>% group_by(cohort, mouse,day) %>% 
  mutate(total2 = sum(total)) %>% 
  dplyr::select(1:4,7) %>% unique(.)

head(mx)
mxx <-mx %>%  filter(day != 11) %>% na.omit(.) %>% filter(glicko_rank %in% c(1,6))
mxx$dom <- ifelse(mxx$glicko_rank == 1, "DOM", mxx$glicko_rank)
mxx$dom <- ifelse(mxx$glicko_rank == 2, "SUBDOM", mxx$dom)
mxx$dom <- ifelse(mxx$glicko_rank == 6, "SUB", mxx$dom)
mxx$dom <- factor(mxx$dom, levels = c("DOM", "SUBDOM", "SUB"))
mxx$glicko_rank <- ifelse(mxx$glicko_rank == 1, "DOM", "SUB")
mxx1 <-mx %>%  filter(day != 11) %>% na.omit(.)

ggplot(mxx, aes(as.factor(day),total2, fill =glicko_rank))+
  geom_boxplot()+
  theme_bw()

x <- ggplot(mxx, aes(as.factor(day), total2, fill = glicko_rank))+
  geom_boxjitter(aes(fill = glicko_rank),outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  scale_color_manual(values = viridis::viridis(2)) +
  scale_fill_manual(values = viridis::viridis(2)) +
  ylim(2000,10000)+
  theme_bw()+
  theme(
        axis.text.x = element_text(size= 15) ,
        axis.text.y = element_text(size= 15),
        text = element_text(size= 20),
        legend.position = "top")+
  ylab("Total Cage Transitions") +
  xlab("Day")+
labs(fill = "Status")
x

ggsave("RFID_stable_cohorts/imgs/domsub_byday.png", x,height = 5, width = 10, dpi = 300)


mxx <-mx %>%  filter(day != 11) %>% na.omit(.) %>%ungroup(.) 

jp2 <- mxx %>% group_by(glicko_rank) %>% arrange(day) %>%  mutate(acum = cumsum(total2)) %>% 
  group_by(day,glicko_rank) %>%  mutate(avg =mean(acum))

jp2$day <- as.factor(jp2$day)
jp2 <- na.omit(jp2)
jp2 <- jp2 %>% filter(day !=11)
head(jp2)


x <- jp2 %>% group_by(glicko_rank,day) %>% summarise(avg = mean(acum)) 
x2 <- jp2 %>% filter(glicko_rank %in% c(1,6))
x <- x %>% filter(glicko_rank %in% c(1,6))
x$glicko_rank <- ifelse(x$glicko_rank == 1, "DOM", "SUB")
x2$glicko_rank <- ifelse(x2$glicko_rank == 1, "DOM", "SUB")

x2 %>% group_by(glicko_rank,day) %>% 
  summarise(median = median(acum),
            lqr = quantile(acum,.25),
            uqr = quantile(acum,.75)
  ) -> DF



x <- ggplot(DF, aes(x=day,y=median,color=glicko_rank)) +
  ggplot2::geom_ribbon(aes( ymin = DF$lqr, ymax = DF$uqr,fill = glicko_rank),alpha =.25)+
  geom_line(aes(color =glicko_rank)) +
  theme_bw()+
  ylab("Cumulative Cage Transitions") +
  xlab("Day") +
  theme(legend.position = "top",
        axis.text.x = element_text(size= 15) ,
   axis.text.y = element_text(size= 15),
   strip.text = element_text(size =20),
   text = element_text(size= 20))+
  labs(color = "Status")+
  scale_x_continuous(breaks = c(1:10))+
  scale_y_continuous(labels = scales::comma)+
  scale_color_manual(values = c("#0071A2", "#FF6600"))+
  scale_fill_manual(values = c("#0071A2", "#FF6600"))

x
ggsave("RFID_stable_cohorts/imgs/domsub_cum2.png", x,height = 5, width = 5, dpi = 300)

DF$glicko_rank <- as.factor(DF$glicko_rank)
DF$day <- as.integer(DF$day)
ggplot()+geom_line(data = DF, aes(x=day,y=median,color = glicko_rank)) +
  geom_ribbon(data = DF, aes(x = day, ymin = lqr, ymax = uqr,fill = glicko_rank),  alpha = 0.5) 
  




m
geom_line(data = DF, aes(x=day,y=median, color = glicko_rank)) 

ggsave("RFID_stable_cohorts/imgs/domsub_cumulative.png", x,height = 5, width = 5, dpi = 300)



y <- move %>% filter(zone == 1) %>% group_by(cohort, mouse,day) %>% 
  mutate(food = sum(total)) %>% 
  dplyr::select(1:4,7) %>% unique(.)

head(y)

ggplot(y, aes(as.factor(day),food, fill =glicko_rank))+
  geom_boxplot()+
  theme_bw()


yx <-y %>%  filter(day != 11) %>% na.omit(.) %>% filter(glicko_rank %in% c(1,6))
yx$glicko_rank <- ifelse(yx$glicko_rank == 1, "DOM", "SUB")

y <- ggplot(yx, aes(as.factor(day),food, fill = glicko_rank))+
  geom_boxjitter(aes(fill = glicko_rank),outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  scale_color_manual(values = c("#0071A2", "#FF6600"))+
  scale_fill_manual(values = c("#0071A2", "#FF6600"))+
  theme_bw()+
  theme(
    axis.text.x = element_text(size= 15) ,
    axis.text.y = element_text(size= 15),
    text = element_text(size= 20),
    legend.position = "top")+
  ylab("Food Cage Entries") +
  xlab("Day")+
  labs(fill = "Status")

y
ggsave("RFID_stable_cohorts/imgs/domsub_food2.png", y,height = 5, width = 10, dpi = 300)



b <- read_csv('RFID_stable_cohorts/data_clean/all_blood_Post.csv')
bx <- b %>% full_join(rank) %>% pivot_longer(c(2:11,19:25))  %>%dplyr::select(c(3,4,8,9,10))
means <- bx %>% group_by(dom, name) %>% summarise(mean = mean(value,na.rm =T)) %>% as.data.frame(.)
bxx <- bx  %>% full_join(means)

bxx$val <- ifelse(is.na(bxx$value), bxx$mean, bxx$value )


bxx <- bxx %>% dplyr::group_by(cohort, mouse, dom, name,val) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  pivot_wider(names_from = name, values_from = val) 

bxx$cm <- paste(bxx$cohort, bxx$mouse, sep = "-")
# bxx <- bxx %>% filter(cm != "6-4")
mo <- bxx %>% dplyr::select(5,9,10,11,15,17,19,22) %>% column_to_rownames('cm')
ma <- bxx %>% dplyr::select(6,8,12,13,14,16,18,19,20,21,22) %>% column_to_rownames('cm')


or <- mo %>% dplyr::select(Ghrelin,ACTH,LH,GH,FSH) %>% cbind(rank)
an <- ma %>% dplyr::select(c(1:5,8,9))

or2 <- or %>% cbind(rank) %>% pivot_longer(1:5)
an2 <- ma %>% cbind(rank) %>% pivot_longer(1:10) 
unique(an2$name)

ggplot(or, aes(Ghrelin,FSH,color =dom))+
  geom_point()


or2$dom <- ifelse(or2$dom == "Dominant", "DOM", "SUB")

y <- ggplot(or2, aes(dom,value, fill = dom))+
  geom_boxjitter(aes(fill = dom),outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.02, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  facet_wrap(factor(name, levels = c("Ghrelin", "GH", "ACTH","LH", "FSH"))~.,scales = "free", ncol =5)+
  scale_color_manual(values = c("#0071A2", "#FF6600"))+
  scale_fill_manual(values = c("#0071A2", "#FF6600"))+
  theme_bw()+
  theme(
    axis.text.x = element_text(size= 15) ,
    axis.text.y = element_text(size= 15),
    text = element_text(size= 20),
    legend.position = "top")+
  ylab("Concentration (pg/ml)") +
  xlab("")+
  labs(fill = "Status")+
  expand_limits(limits = c(100,NA))

y


ggsave("RFID_stable_cohorts/imgs/or_blood_SBN.png", y,height = 3.2, width = 12, dpi = 300)


an2$name <- ifelse(an2$name == "MCP1_CCL2", "MCP1", an2$name)
an2$dom <- ifelse(an2$dom == "Dominant", "DOM", "SUB")

y2 <- ggplot(an2, aes(dom,value, fill = dom))+
  # geom_line(aes(group = cohort), size = .75, alpha = .75) +
  geom_boxjitter(aes(fill = dom),outlier.color = NA, jitter.shape = 21,
                 alpha = 0.5,
                 jitter.height = 0.01, jitter.width = 0.030, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  facet_wrap(factor(name, levels = c("Amylin", "Cpeptide2","Insulin", "TSH", "Leptin", "PYY", "Secretin","MCP1","IL6","TNFa"))~.,scales = "free", ncol =5)+
  scale_color_manual(values = c("#0071A2", "#FF6600"))+
  scale_fill_manual(values = c("#0071A2", "#FF6600"))+
  theme_bw()+
  theme(
    axis.text.x = element_text(size= 15) ,
    axis.text.y = element_text(size= 15),
    text = element_text(size= 20),
    legend.position = "none")+
  ylab("Concentration (pg/ml)") +
  xlab("")+
  labs(fill = "Status")+
  expand_limits(limits = c(100,NA))

y2


ggsave("RFID_stable_cohorts/imgs/anor_blood_SBN.png", y2,height = 5, width = 12, dpi = 300)

ggplot(an2, aes(x=dom , y = value)) +
  geom_line(aes(group = cohort, color=dom), size = .75, alpha = .75) +
  geom_point(aes(color=dom), alpha = .75, size = 1)+
  scale_color_manual(values = c("#238A8DFF", "#FDE725FF"), name = "Rank")+
  labs(x = "",
       y=  "Concentration (pg/ml)") +
  facet_wrap(~name, scales = "free_y")+
  theme(axis.text.x = element_text(vjust = 1, size = 15),
        axis.text.y = element_text(hjust = 0.5, size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.title = element_text(size = 20),
        axis.text= element_text(color="#3C3C3C", size=15),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 15)
  )

    