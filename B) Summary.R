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

############################
###   Load the dataset   ###
############################

dta <- readRDS("Data/SP-off-RP data.rds")

#############################################
### Table B.1: Summary of characteristics ###
#############################################

#Make
dta %>% 
  group_by(VMAKE) %>%
  summarise(n = n()) %>%
  mutate(freq = 100*n/sum(n)) %>%
  arrange(-freq) %>%
  head(6)

#Make (2 = Truck; SUV = 3; Car = 1; Van = 4)
dta %>% 
  group_by(VTYPE) %>%
  summarise(n = n()) %>%
  mutate(freq = 100*n/sum(n)) %>%
  arrange(-freq) %>%
  head(6)

#Gender (1 = Female; 0 = Male)
dta %>% 
  group_by(FEMALE) %>%
  summarise(n = n()) %>%
  mutate(freq = 100*n/sum(n)) %>%
  arrange(-freq) %>%
  head(6)

#Badge (1 = yes; 0 = no)
dta %>% 
  group_by(BADGE) %>%
  summarise(n = n()) %>%
  mutate(freq = 100*n/sum(n)) %>%
  arrange(-freq) %>%
  head(6)

#Yellow cap (1 = yes; 0 = no)
dta %>% 
  group_by(YELCAP) %>%
  summarise(n = n()) %>%
  mutate(freq = 100*n/sum(n)) %>%
  arrange(-freq) %>%
  head(6)

#Ownership
dta %>% 
  dplyr::select(GOV, COMP, ONPV) %>%
  summarise(GOV = sum(GOV), COMP = sum(COMP), OTHER = sum(ONPV)) %>%
  mutate(Personal = 100 - 100*(GOV + COMP + OTHER)/nrow(dta), GOV = 100*GOV/nrow(dta), COMP = 100*COMP/nrow(dta), OTHER = 100*OTHER/nrow(dta)) 

#Age
summary(dta$AGE)

#Miles per year
summary(dta$MPY)


################################################
### Table B.2: Motorists who fueled with E10 ###
################################################
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

################################################
### Table B.3: Motorists who fueled with E10 ###
################################################

#Come for E85
dta %>% dplyr::filter(E85 == 1) %>%
  group_by(QD) %>%
  summarise(n = n()) %>%
  mutate(freq = 100*n/sum(n)) %>%
  arrange(-freq) %>%
  head(6)

#How far did you drive?
dta %>%
  mutate(dist = case_when(DIST == 0 ~ "Not at all",
                          DIST > 0 & DIST <= 1 ~ "(0,1]",
                          DIST > 1 & DIST <= 3 ~ "(1,3]",
                          DIST > 3 & DIST <= 5 ~ "(3,5]",
                          DIST > 5 & DIST <= 10 ~ "(5,10]",
                          TRUE ~ "More than 10")) %>%
  dplyr::filter(E85 == 1 & QD == 1) %>%
  group_by(dist) %>%
  summarise(n = n()) %>%
  mutate(freq = 100*n/sum(n)) %>%
  arrange(-freq) %>%
  head(6)

