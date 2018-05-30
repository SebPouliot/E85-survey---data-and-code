# Clear memory
rm(list = ls())

#########################
###   Load packages   ###
#########################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr)
pacman::p_load(xlsx)
pacman::p_load(readxl)
pacman::p_load(stringr)
pacman::p_load(tidyverse, ggplot2, actuar, viridis)
pacman::p_load(extrafont) #The package extrafont imports fonts that can be used with ggplot2 
#font_import() #To run once under windows to import fonts
loadfonts(device = "win") #Load the fonts

############################
###   Load the dataset   ###
############################

dta <- readRDS("Data/SP-off-RP data.rds")

##################################
### Normalize the price of E85 ###
##################################

adj <- (0.74*2/3 + 0.26*1)/(0.1*2/3 + 0.9*1)

dta <- dta %>%
  mutate(E85P = E85P/adj)

################################
### Table 1: Summary of data ###
################################

dta <- tbl_df(dta) %>%
  mutate(E85 = as.numeric(E85),
         CHOICE = as.character(CHOICE))

#Per station
dta %>% group_by(STNID) %>% 
  summarize(N = sum(ONES), 
            E85p = mean(E85P) %>% round(2), 
            E10p = mean(G1P) %>% round(2), 
            premium = mean(E85P-G1P) %>% round(2), 
            ratio = mean((E85P/G1P)) %>% round(2), 
            ratio_min = min((E85P/G1P)) %>% round(2),
            ratio_max = max((E85P/G1P)) %>% round(2), 
            share = 100*mean(E85) %>% round(3), 
            drive_0 = 100*(mean(as.numeric(QD==0 & E85 == 1))/mean(E85 == 1)) %>% round(3), 
            dist_1 = 100*(mean(as.numeric(DIST ==0 & E85 == 1))/mean(E85 == 1)) %>% round(3))


#Per location
dta %>% mutate(STNST = ifelse(str_detect(STNID, "DM"), "DM", ifelse(str_detect(STNID, "CS"), "CS", ifelse(str_detect(STNID, "LR"), "LR", ifelse(str_detect(STNID, "LA"), "LA", ifelse(str_detect(STNID, "TS"), "TS", "SAC")))))) %>% 
  group_by(STNST) %>% 
  summarize(N = sum(ONES), 
            E85p = mean(E85P) %>% round(2), 
            E10p = mean(G1P) %>% round(2), 
            premium = mean(E85P-G1P) %>% round(2), 
            ratio = mean((E85P/G1P)) %>% round(2), 
            ratio_min = min((E85P/G1P)) %>% round(2),
            ratio_max = max((E85P/G1P)) %>% round(2), 
            share = 100*mean(E85 == 1) %>% round(3), 
            drive_0 = 100*(mean(as.numeric(QD==0 & E85 == 1))/mean(E85 == 1)) %>% round(3), 
            dist_1 = 100*(mean(as.numeric(DIST ==0 & E85 == 1))/mean(E85 == 1)) %>% round(3))


#Per retailer
dta %>% group_by(STNRET) %>% 
  summarize(N = sum(ONES), 
            E85p = mean(E85P) %>% round(2), 
            E10p = mean(G1P) %>% round(2), 
            premium = mean(E85P-G1P) %>% round(2), 
            ratio = mean((E85P/G1P)) %>% round(2), 
            ratio_min = min((E85P/G1P)) %>% round(2),
            ratio_max = max((E85P/G1P)) %>% round(2), 
            share = 100*mean(E85 == 1) %>% round(3), 
            drive_0 = 100*(mean(as.numeric(QD==0 & E85 == 1))/mean(E85 == 1)) %>% round(3), 
            dist_1 = 100*(mean(as.numeric(DIST ==0 & E85 == 1))/mean(E85 == 1)) %>% round(3))

#Total
dta %>% 
  summarize(N = sum(ONES), 
            E85p = mean(E85P) %>% round(2), 
            E10p = mean(G1P) %>% round(2), 
            premium = mean(E85P-G1P) %>% round(2), 
            ratio = mean((E85P/G1P)) %>% round(2), 
            ratio_min = min((E85P/G1P)) %>% round(2),
            ratio_max = max((E85P/G1P)) %>% round(2), 
            share = 100*mean(E85 == 1) %>% round(3), 
            drive_0 = 100*(mean(as.numeric(QD==0 & E85 == 1))/mean(E85 == 1)) %>% round(3), 
            dist_1 = 100*(mean(as.numeric(DIST ==0 & E85 == 1))/mean(E85 == 1)) %>% round(3))

dta %>% 
  summarize(N_E85 = sum(E85 == 1),
            N_E10 = 881- sum(E85 == 1),
            N_no = sum(as.numeric(QD==0)),
            N_0 = sum(as.numeric(DIST==0)),
            N_no = sum(as.numeric(QD==0 & CHOICE == "E85")),
            N_0 = sum(as.numeric(DIST==0 & CHOICE == "E85")))
            
     

###############################################################################
### Table 2: Responses to questions to flex motorists who refueled with E10 ###
###############################################################################

dta %>% dplyr::filter(E85 == 0) %>%
  summarize(N_QA = sum(QA==1, na.rm = TRUE),
            QA = mean(QA==1, na.rm = TRUE))
            
dta %>% dplyr::filter(E85 == 0 & QA ==1) %>%
  summarize(N_QB = sum(QB==1, na.rm = TRUE),
            N_QC = sum(QC==1, na.rm = TRUE),
            QB = mean(QB==1, na.rm = TRUE),
            QC = mean(QC==1, na.rm = TRUE))

dta %>% dplyr::filter(E85 == 0 & QA ==1) %>%
  summarize(N_QB = sum(QB==0, na.rm = TRUE),
            N_QC = sum(QC==0, na.rm = TRUE),
            QB = mean(QB==0, na.rm = TRUE),
            QC = mean(QC==0, na.rm = TRUE))


###############################################################################
### Table 3: Responses to fuel opinion questions by region and fuel choice ###
###############################################################################

### E10 motorists ###

#Question about the environment
env <- dta %>% dplyr::filter(E85 == 0) %>%
  group_by(STNST) %>%  
  summarize(Q = "Which fuel is better for the environment?",
            N = sum(Ones),
            Eth = (100*sum(as.numeric(Q5==1))/N) %>% round(),
            Gas = (100*sum(as.numeric(Q5==2))/N) %>% round(),
            ND = (100*sum(as.numeric(Q5==3))/N) %>% round(),
            DK = (100*sum(as.numeric(Q5==4))/N) %>% round()) 

#Question about the engine
eng <- dta %>% dplyr::filter(E85 == 0) %>%
  group_by(STNST) %>%  
  summarize(Q = "Which fuel is better for your engine?",
            N = sum(Ones),
            Eth = (100*sum(as.numeric(Q6==1))/N) %>% round(),
            Gas = (100*sum(as.numeric(Q6==2))/N) %>% round(),
            ND = (100*sum(as.numeric(Q6==3))/N) %>% round(),
            DK = (100*sum(as.numeric(Q6==4))/N) %>% round())


#Question about the economy
econ <- dta %>% dplyr::filter(E85 == 0) %>%
  group_by(STNST) %>%  
  summarize(Q = "Which fuel is better for the economy?",
            N = sum(Ones),
            Eth = (100*sum(as.numeric(Q7==1))/N) %>% round(),
            Gas = (100*sum(as.numeric(Q7==2))/N) %>% round(),
            ND = (100*sum(as.numeric(Q7==3))/N) %>% round(),
            DK = (100*sum(as.numeric(Q7==4))/N) %>% round())

#Question about national security
sec <- dta %>% dplyr::filter(E85 == 0) %>%
  group_by(STNST) %>%  
  summarize(Q = "Which fuel is better for national security?",
            N = sum(Ones),
            Eth = (100*sum(as.numeric(Q8==1))/N) %>% round(),
            Gas = (100*sum(as.numeric(Q8==2))/N) %>% round(),
            ND = (100*sum(as.numeric(Q8==3))/N) %>% round(),
            DK = (100*sum(as.numeric(Q8==4))/N) %>% round())

#Question about fuel efficiency
eff <- dta %>% dplyr::filter(E85 == 0) %>%
  group_by(STNST) %>%  
  summarize(Q = "Which fuel yields more miles per gallon?",
            N = sum(Ones),
            Eth = (100*sum(as.numeric(Q9==1))/N) %>% round(),
            Gas = (100*sum(as.numeric(Q9==2))/N) %>% round(),
            ND = (100*sum(as.numeric(Q9==3))/N) %>% round(),
            DK = (100*sum(as.numeric(Q9==4))/N) %>% round())

rbind(env, eng, econ, sec, eff) %>% data.frame


# Totals for E10

#Question about the environment
env <- dta %>% dplyr::filter(E85 == 0) %>%
  summarize(Q = "Which fuel is better for the environment?",
            N = sum(Ones),
            Eth = (100*sum(as.numeric(Q5==1))/N) %>% round(),
            Gas = (100*sum(as.numeric(Q5==2))/N) %>% round(),
            ND = (100*sum(as.numeric(Q5==3))/N) %>% round(),
            DK = (100*sum(as.numeric(Q5==4))/N) %>% round()) 

#Question about the engine
eng <- dta %>% dplyr::filter(E85 == 0) %>%
  summarize(Q = "Which fuel is better for your engine?",
            N = sum(Ones),
            Eth = (100*sum(as.numeric(Q6==1))/N) %>% round(),
            Gas = (100*sum(as.numeric(Q6==2))/N) %>% round(),
            ND = (100*sum(as.numeric(Q6==3))/N) %>% round(),
            DK = (100*sum(as.numeric(Q6==4))/N) %>% round())


#Question about the economy
econ <- dta %>% dplyr::filter(E85 == 0) %>%
  summarize(Q = "Which fuel is better for the economy?",
            N = sum(Ones),
            Eth = (100*sum(as.numeric(Q7==1))/N) %>% round(),
            Gas = (100*sum(as.numeric(Q7==2))/N) %>% round(),
            ND = (100*sum(as.numeric(Q7==3))/N) %>% round(),
            DK = (100*sum(as.numeric(Q7==4))/N) %>% round())

#Question about national security
sec <- dta %>% dplyr::filter(E85 == 0) %>%
  summarize(Q = "Which fuel is better for national security?",
            N = sum(Ones),
            Eth = (100*sum(as.numeric(Q8==1))/N) %>% round(),
            Gas = (100*sum(as.numeric(Q8==2))/N) %>% round(),
            ND = (100*sum(as.numeric(Q8==3))/N) %>% round(),
            DK = (100*sum(as.numeric(Q8==4))/N) %>% round())

#Question about fuel efficiency
eff <- dta %>% dplyr::filter(E85 == 0) %>%
  summarize(Q = "Which fuel yields more miles per gallon?",
            N = sum(Ones),
            Eth = (100*sum(as.numeric(Q9==1))/N) %>% round(),
            Gas = (100*sum(as.numeric(Q9==2))/N) %>% round(),
            ND = (100*sum(as.numeric(Q9==3))/N) %>% round(),
            DK = (100*sum(as.numeric(Q9==4))/N) %>% round())

rbind(env, eng, econ, sec, eff) %>% data.frame


### E85 motorists ###

#Question about the environment
env <- dta %>% dplyr::filter(E85 == 1) %>%
  group_by(STNST) %>%  
  summarize(Q = "Which fuel is better for the environment?",
            N = sum(Ones),
            Eth = (100*sum(as.numeric(Q5==1))/N) %>% round(),
            Gas = (100*sum(as.numeric(Q5==2))/N) %>% round(),
            ND = (100*sum(as.numeric(Q5==3))/N) %>% round(),
            DK = (100*sum(as.numeric(Q5==4))/N) %>% round()) 

#Question about the engine
eng <- dta %>% dplyr::filter(E85 == 1) %>%
  group_by(STNST) %>%  
  summarize(Q = "Which fuel is better for your engine?",
            N = sum(Ones),
            Eth = (100*sum(as.numeric(Q6==1))/N) %>% round(),
            Gas = (100*sum(as.numeric(Q6==2))/N) %>% round(),
            ND = (100*sum(as.numeric(Q6==3))/N) %>% round(),
            DK = (100*sum(as.numeric(Q6==4))/N) %>% round())


#Question about the economy
econ <- dta %>% dplyr::filter(E85 == 1) %>%
  group_by(STNST) %>%  
  summarize(Q = "Which fuel is better for the economy?",
            N = sum(Ones),
            Eth = (100*sum(as.numeric(Q7==1))/N) %>% round(),
            Gas = (100*sum(as.numeric(Q7==2))/N) %>% round(),
            ND = (100*sum(as.numeric(Q7==3))/N) %>% round(),
            DK = (100*sum(as.numeric(Q7==4))/N) %>% round())

#Question about national security
sec <- dta %>% dplyr::filter(E85 == 1) %>%
  group_by(STNST) %>%  
  summarize(Q = "Which fuel is better for national security?",
            N = sum(Ones),
            Eth = (100*sum(as.numeric(Q8==1))/N) %>% round(),
            Gas = (100*sum(as.numeric(Q8==2))/N) %>% round(),
            ND = (100*sum(as.numeric(Q8==3))/N) %>% round(),
            DK = (100*sum(as.numeric(Q8==4))/N) %>% round())

#Question about fuel efficiency
eff <- dta %>% dplyr::filter(E85 == 1) %>%
  group_by(STNST) %>%  
  summarize(Q = "Which fuel yields more miles per gallon?",
            N = sum(Ones),
            Eth = (100*sum(as.numeric(Q9==1))/N) %>% round(),
            Gas = (100*sum(as.numeric(Q9==2))/N) %>% round(),
            ND = (100*sum(as.numeric(Q9==3))/N) %>% round(),
            DK = (100*sum(as.numeric(Q9==4))/N) %>% round())

rbind(env, eng, econ, sec, eff) %>% data.frame

# Totals for E10

#Question about the environment
env <- dta %>% dplyr::filter(E85 == 1) %>%
  summarize(Q = "Which fuel is better for the environment?",
            N = sum(Ones),
            Eth = (100*sum(as.numeric(Q5==1))/N) %>% round(),
            Gas = (100*sum(as.numeric(Q5==2))/N) %>% round(),
            ND = (100*sum(as.numeric(Q5==3))/N) %>% round(),
            DK = (100*sum(as.numeric(Q5==4))/N) %>% round()) 

#Question about the engine
eng <- dta %>% dplyr::filter(E85 == 1) %>%
  summarize(Q = "Which fuel is better for your engine?",
            N = sum(Ones),
            Eth = (100*sum(as.numeric(Q6==1))/N) %>% round(),
            Gas = (100*sum(as.numeric(Q6==2))/N) %>% round(),
            ND = (100*sum(as.numeric(Q6==3))/N) %>% round(),
            DK = (100*sum(as.numeric(Q6==4))/N) %>% round())


#Question about the economy
econ <- dta %>% dplyr::filter(E85 == 1) %>%
  summarize(Q = "Which fuel is better for the economy?",
            N = sum(Ones),
            Eth = (100*sum(as.numeric(Q7==1))/N) %>% round(),
            Gas = (100*sum(as.numeric(Q7==2))/N) %>% round(),
            ND = (100*sum(as.numeric(Q7==3))/N) %>% round(),
            DK = (100*sum(as.numeric(Q7==4))/N) %>% round())

#Question about national security
sec <- dta %>% dplyr::filter(E85 == 1) %>%
  summarize(Q = "Which fuel is better for national security?",
            N = sum(Ones),
            Eth = (100*sum(as.numeric(Q8==1))/N) %>% round(),
            Gas = (100*sum(as.numeric(Q8==2))/N) %>% round(),
            ND = (100*sum(as.numeric(Q8==3))/N) %>% round(),
            DK = (100*sum(as.numeric(Q8==4))/N) %>% round())

#Question about fuel efficiency
eff <- dta %>% dplyr::filter(E85 == 1) %>%
  summarize(Q = "Which fuel yields more miles per gallon?",
            N = sum(Ones),
            Eth = (100*sum(as.numeric(Q9==1))/N) %>% round(),
            Gas = (100*sum(as.numeric(Q9==2))/N) %>% round(),
            ND = (100*sum(as.numeric(Q9==3))/N) %>% round(),
            DK = (100*sum(as.numeric(Q9==4))/N) %>% round())

rbind(env, eng, econ, sec, eff) %>% data.frame


###################
### Amount paid ###
###################

#Setup for the figures
mytheme <-  theme(text=element_text(size=10, family = "Times New Roman", colour = 'black'),
                  axis.line.y = element_blank(), 
                  panel.border = element_blank(), 
                  panel.background =  element_blank(),
                  plot.margin=unit(c(0.5,0.5,0.5,0.5), "lines"), 
                  legend.title=element_blank(), 
                  legend.text = element_text(size = 8, colour = 'black', family = "Times New Roman"), 
                  legend.background=element_blank(),
                  legend.key=element_blank(),
                  legend.position= "none", 
                  legend.direction="vertical")



### Check expenditures near round numbers based on recorded expenditures ###

ggplot(data = dta, aes(x = EXP)) + 
  geom_histogram(bins = 150, color = "black", fill = "white") + 
  scale_x_continuous(breaks=seq(0,90,by=5), labels=seq(0,90,by=5)) +
  mytheme


dta  <- dta %>% mutate(exp_round = case_when(EXP > 4.95 & EXP < 10.05  ~ 1,
                                     EXP > 9.95 & EXP < 10.05 ~ 1,
                                     EXP > 14.95 & EXP < 15.05 ~ 1,
                                     EXP > 19.95 & EXP < 20.05 ~ 1,
                                     EXP > 24.95 & EXP < 25.05 ~ 1,
                                     EXP > 29.95 & EXP < 30.05 ~ 1,
                                     EXP > 34.95 & EXP < 35.05 ~ 1,
                                     EXP > 39.95 & EXP < 40.05 ~ 1,
                                     EXP > 44.95 & EXP < 45.05 ~ 1,
                                     EXP > 49.95 & EXP < 50.05 ~ 1,
                                     EXP > 54.95 & EXP < 55.05 ~ 1,
                                     EXP > 59.95 & EXP < 60.05 ~ 1,
                                     is.na(EXP) ~ as.numeric("NA"),
                                     TRUE ~ 0))

summary(dta$exp_round)



#Calculate expenditure from volumes and prices
dta <- dta %>% mutate(exp_calc = case_when(!is.na(EXP) ~ EXP,
                                           CHOICE == "E85" ~ VOL*E85P,
                                           CHOICE == "Gas1" ~ VOL*G1P,
                                           CHOICE == "Gas2" ~ VOL*G2P,
                                           CHOICE == "Gas3" ~ VOL*G3P,
                                           CHOICE == "Gas3" ~ VOL*G3P,
                                           CHOICE == "E30" ~ VOL*G2P,
                                           CHOICE == "Gas1CSH" ~ EXP,
                                           CHOICE == "Gas2CW" ~ EXP,
                                           CHOICE == "Prem" ~ EXP,
                                           TRUE ~ as.numeric("NA")),
                      exp_calc = round(exp_calc,1),
                      choice = case_when(CHOICE == "E85" ~ "E85",
                                         TRUE ~ "E10"))

# Compare recorded and calculated. Use calculated because fewer missing observations
summary(dta[,c("EXP", "exp_calc")])


#################################
### Expenditures - figure D.1 ###
#################################

exp_plot <- ggplot(data = dta, aes(x = exp_calc, fill = choice)) + 
  geom_histogram(bins = 150, color = "black") + 
  facet_grid(choice ~.) +
  scale_x_continuous(breaks=seq(0,100,by=5), labels=seq(0,100,by=5)) +#  ggtitle("Fuel expenditure") +
  ylab("Count") +
  xlab("Expenditure ($)") +
  mytheme

ggsave(exp_plot, filename = "Figures/Figure D1.png",  width = 6, height = 4, units = "in", dpi = 600) 

#####################################
### Volume purchased - figure D.2 ###
#####################################

vol_plot <- ggplot(data = dta, aes(x = VOL, fill = choice)) + 
  geom_histogram(bins = 150, color = "black") + 
  facet_grid(choice ~.) +
  scale_x_continuous(breaks=seq(0,100,by=5), labels=seq(0,100,by=5)) +  #ggtitle("Volume purchased") +
  ylab("Count") +
  xlab("Volume (gallon)") +
  mytheme

ggsave(vol_plot, filename = "Figures/Figure D2.png",  width = 6, height = 4, units = "in", dpi = 600) 

######################################################################################
### Same histograms but only for buyers of E85  - not shown in paper and not saved ###
######################################################################################

dta <- dta %>% mutate(distance = case_when(DIST==0 ~ "Dist = 0",
                                    TRUE ~ "Dist > 0"))

exp_plot <- ggplot(data = dta %>% dplyr::filter(choice == "E85"), aes(x = exp_calc, fill = distance)) + 
  geom_histogram(bins = 150, color = "black") + 
  facet_grid(distance ~.) +
  scale_x_continuous(breaks=seq(0,100,by=5), labels=seq(0,100,by=5)) +
  ggtitle("Fuel expenditure - E85 motorists") +
  ylab("Count") +
  xlab("Expenditure ($)") +
  mytheme

exp_plot


vol_plot <-  ggplot(data = dta %>% dplyr::filter(choice == "E85"), aes(x = VOL, fill = distance)) + 
  geom_histogram(bins = 150, color = "black") + 
  facet_grid(distance ~.) +
  scale_x_continuous(breaks=seq(0,100,by=5), labels=seq(0,100,by=5)) +
  ggtitle("Volume purchased - E85 motorists") +
  ylab("Count") +
  xlab("Volume (gallon)") +
  mytheme

vol_plot



