library(tidyverse)


source('RFID_stable_cohorts/data_carpentry/physiology_data/metabolic_results.R')
head(mdf)

colnames(mdf)
m.long <-mdf %>% pivot_longer(cols = 2:11, names_to="analyte") 
m.long$ID <- substr(m.long$ID, 5,9)
m.long$ID <- gsub("_", "", m.long$ID)

head(m.long)


ggplot(m.long, aes(x=time , y = value)) +
  geom_line(aes(group = ID, color=dom), size = .75, alpha = .75) +
  geom_point(aes(color=dom), alpha = .75, size = 1)+
  scale_color_manual(values = c("#238A8DFF", "#FDE725FF"), name = "Rank")+
  labs(x = "",
       y=  "Concentration (pg/ml)") +
  facet_wrap(~analyte, scales = "free_y")+
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

m.long$time <- factor(m.long$time, levels = c("Pre", "Post"))


a.list <- m.long %>% split(.$analyte)
lapply(a.list,head)


df <- a.list[[1]]



prepost <- function (df){
  # source('RFID_stable_cohorts/data_carpentry/figures')
  # library(ggplot2)

p1 <- ggplot(df, aes(x=dom , y = value, color =dom, fill = dom)) +
  geom_boxjitter(outlier.color = NA, jitter.shape = NA,
                 alpha = 0.4,
                 jitter.height = 0, jitter.width = 0, errorbar.draw = TRUE,
                 position = position_dodge(0.5))+
  
  geom_line(aes(group = cohort),position = position_dodge( 0.0), size = .6, color = "black") +
  geom_point(aes(group = ID),position = position_dodge(  0.0), size = 3, alpha =0.4) +
  facet_wrap(~time)+
  xlab("")+
  ylab("Concentration (pg/ml)")+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  theme(axis.text.x = element_text(vjust = 1, size = 15),
        axis.text.y = element_text(hjust = 0.5, size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.title = element_text(size = 15),
        axis.text= element_text(color="#3C3C3C", size=15),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 15),
        legend.position = "none")

return(p1)
}


lapply(a.list, prepost)

labels = c("Amylin", "C-Peptide2", "Ghrelin", "IL6", "Insulin", "Leptin","MCP1/CCL2", "PYY", "Secretin", "TNF-alpha")

plots <- lapply(a.list, prepost)

met2 <- egg::ggarrange(plots=plots, labels = labels, nrow = 2,  ncol = 5, 
               label.args = list(gp = grid::gpar(font = 1, cex =1.5)))


p1 <- plots[[1]]+ggtitle("Amylin")+ theme(plot.title = element_text(hjust = 0.5)) 
p2 <- plots[[2]]+ggtitle("C-Peptide2")+  theme(plot.title = element_text(hjust = 0.5)) 
p3 <- plots[[3]]+ggtitle("Ghrelin")+  theme(plot.title = element_text(hjust = 0.5)) 
p4 <- plots[[4]]+ggtitle("IL6")+  theme(plot.title = element_text(hjust = 0.5)) 
p5 <- plots[[5]]+ggtitle("Insulin")+  theme(plot.title = element_text(hjust = 0.5)) 
p6 <- plots[[6]]+ggtitle("Leptin")+  theme(plot.title = element_text(hjust = 0.5)) 
p7 <- plots[[7]]+ggtitle("MCP1/CCL2")+  theme(plot.title = element_text(hjust = 0.5)) 
p8 <- plots[[8]]+ggtitle("PYY")+  theme(plot.title = element_text(hjust = 0.5)) 
p9 <- plots[[9]]+ggtitle("Secretin")+  theme(plot.title = element_text(hjust = 0.5)) 
p10 <- plots[[10]]+ggtitle("TNF-alpha")+ theme(plot.title = element_text(hjust = 0.5)) 

met <- gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,ncol=5)


ggsave("RFID_stable_cohorts/imgs/metabolic_prepost.png", met, height = 10, width= 30, dpi = 300)

#############
library(tidyverse)
library(MASS)
library(car)

#source
#rank data
rank <- read_csv("RFID_stable_cohorts/data_clean/socialbehavior/rank_data.csv")
head(rank)

rank$dom <- ifelse(rank$glicko_rank == 1, "Dominant", rank$glicko_rank)
rank$dom <- ifelse(rank$glicko_rank == 2, "Subdominant", rank$dom)
rank$dom <- ifelse(rank$glicko_rank == 6, "Subordinate", rank$dom)

#metabolic hormone data 
m <- read_csv("RFID_stable_cohorts/data_raw/physiological/endocrine_copy.csv")
head(m)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
# get cohort and mouse ids
m$time <- ifelse(grepl("pre",m$ID), "Pre", "Post")
m$cohort  <-  as.numeric(gsub(".*?([0-9]+).*", "\\1", m$ID))
m$mouse <- as.numeric(substrRight(m$ID,1))



head(m)

m$BDNF <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m$BDNF))
m$GH <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m$GH))
m$Prolactin <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m$Prolactin))
m$TSH <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m$TSH))
m$LH <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]", "", m$LH))


df <- m %>% left_join(rank)
head(df)

ed <- df %>% dplyr::select(1:11,15) %>% filter(time  == 'Post')

ed.long <-  ed %>% pivot_longer(cols = 2:7, names_to="analyte") 
ed.long$ID <- substr(ed.long$ID, 5,9)
ed.long$ID <- gsub("_", "", ed.long$ID)



raw <- ggplot(ed.long, aes(x=dom , y = value, color =dom, fill = dom)) +
  geom_boxjitter(outlier.color = NA, jitter.shape = NA,
                 alpha = 0.4,
                 jitter.height = 0, jitter.width = 0, errorbar.draw = TRUE,
                 position = position_dodge(0.5))+
  
  geom_line(aes(group = cohort),position = position_dodge( 0.0), size = .6, color = "black") +
  geom_point(aes(group = ID),position = position_dodge(  0.0), size = 3, alpha =0.4) +
  facet_wrap(~analyte, scales = "free_y")+
  xlab("")+
  ylab("Concentration (pg/ml)")+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  theme(axis.text.x = element_text(vjust = 1, size = 15),
        axis.text.y = element_text(hjust = 0.5, size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.title = element_text(size = 15),
        axis.text= element_text(color="#3C3C3C", size=15),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 15),
        legend.position = "none")

ggsave("RFID_stable_cohorts/imgs/raw_endocrine.png", raw, height = 10, width = 12)



#filtering out C10M2, C4_M2, C10_M5, more than 2 out of range. 
#GH: sample C7_M6 and BDNF sample C7_M2  10X+ higher than all other samples 
# C10M2 - SUB
# C4_M2 - SUB
# C10_M5 - DOM
# C7_M6 - SUB
# C7_M2 - DOM

m1 <- m  %>% filter(ID != "post_C10M2")%>%
  filter(ID != "post_C10M5") %>%
  filter(ID != "post_C4M2")
# 
  m1 <- m1 %>% filter(BDNF <20) %>% filter(GH <15000)


df <- m1 %>% left_join(rank)
head(df)

ed <- df %>% dplyr::select(1:11,15) %>% filter(time  == 'Post')

ed.long <-  ed %>% pivot_longer(cols = 2:7, names_to="analyte") 
ed.long$ID <- substr(ed.long$ID, 5,9)
ed.long$ID <- gsub("_", "", ed.long$ID)


filtered <- ggplot(ed.long, aes(x=dom , y = value, color =dom, fill = dom)) +
  geom_boxjitter(outlier.color = NA, jitter.shape = 21,
                 alpha = 0.4,
                 jitter.height = 0, jitter.width = 0.03, errorbar.draw = TRUE,
                 position = position_dodge(0.85))+
  
  # geom_line(aes(group = cohort),position = position_dodge( 0.0), size = .6, color = "black") +
  # geom_point(aes(group = ID),position = position_dodge( 0.03), size = 3, alpha =0.4) +
  facet_wrap(~analyte, scales = "free_y")+
  xlab("")+
  ylab("Concentration (pg/ml)")+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  theme(axis.text.x = element_text(vjust = 1, size = 15),
        axis.text.y = element_text(hjust = 0.5, size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.title = element_text(size = 15),
        axis.text= element_text(color="#3C3C3C", size=15),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 15),
        legend.position = "none")
filtered
ggsave("RFID_stable_cohorts/imgs/_filtered_major_endocrine_withoutlines.png", filtered, height = 10, width = 12)

