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
p_load(numDeriv)
p_load(data.table)

############################
###   Load the dataset   ###
############################

dta <- readRDS("Data/SP-off-RP data.rds")

##################################
### Normalize the price of E85 ###
##################################

adj <- (0.74*2/3 + 0.26*1)/(0.1*2/3 + 0.9*1)

dta <- dta %>%
  mutate(E85P = E85P/adj, 
         PREM = E85P - G1P,
         RATIO = E85P/G1P,
         LN_RATIO = log(RATIO),
         HYP_P85 = HYP_P85/adj,
         HPREM = HYP_P85 - HYP_P10,
         HRATIO = HYP_P85/HYP_P10,
         LN_HRATIO = log(HRATIO))

# Source solver function with mu, s, and SE's (delta method).
# This file also has summary functions to summarize model results and make tables.
source("Code/func_logit_solver_deltamethod.R")

# Source SP-off-RP code
source("Code/Func_SP_WMLE_logit.R")

####################################
###   Choose estimation sample   ###
####################################

dta <- dta %>% dplyr::filter(QD==0) # 479 obs
#dta <- dta %>% dplyr::filter(DIST==0) # 670 obs

##################################################################
###   Set up models with dependent and independent variables   ###
##################################################################

# Set seed so we get the same random draws and results each time
set.seed(12345)

# Choose number of conditional draws for simulated integration for SP-off-RP models
N_draw <- 1000

# Define dependent variables
rp_dep <- c("E85")
sp_dep <- c("SP_E85")

# RP-only premium model complete list of independent variables for Models 1A, 3A.
rp_indep_prem <- c("ONES", "PREM", "GOV", "COMP", "ONPV", "VTYPE_TRUCK", "VTYPE_SUV", "VTYPE_VAN", "BADGE", "FEMALE", "AGE", "TMPY", "ENV_ETH", "ENV_GAS", "ENV_ND", "ENG_ETH", "ENG_GAS", "ENG_ND", "ECON_ETH", "ECON_GAS", "ECON_ND", "NS_ETH", "NS_GAS", "NS_ND", "MPG_ETH", "MPG_GAS", "MPG_ND",  "STNST_AR", "STNST_CO",  "STNST_OK", "STNST_CA")

# SP-off-RP premium model list of independent variables for Models 1B, 1C, 3B, 3C.
sp_indep_prem <- c("ONES", "HPREM", "GOV", "COMP", "ONPV", "VTYPE_TRUCK", "VTYPE_SUV", "VTYPE_VAN", "BADGE", "FEMALE", "AGE", "TMPY", "ENV_ETH", "ENV_GAS", "ENV_ND", "ENG_ETH", "ENG_GAS", "ENG_ND", "ECON_ETH", "ECON_GAS", "ECON_ND", "NS_ETH", "NS_GAS", "NS_ND","MPG_ETH", "MPG_GAS", "MPG_ND", "STNST_AR", "STNST_CO", "STNST_OK", "STNST_CA")

# Define survey weights for energy content
dta$SW3 <- 1
# Based on Ethanol/Gas energy = 2/3, E10 ethanol content =0.10, and E85 ethanol content =0.74 (implies wts=0.77931)
dta$SW3[dta$E85==1] <- 0.78
svy_wts <- c("SW3")

# Remove extra columns, save QD and DIST to make subsets, and save RATIO to make graphs
dta <- dta[,unique(c(rp_dep, sp_dep, rp_indep_prem, sp_indep_prem, svy_wts, "QD", "DIST", "RATIO"))]
rownames(dta) <- NULL

# Formula for premium
formula_prem <- as.formula(paste(rp_dep, paste(rp_indep_prem, collapse = "+"), sep = "~ -1 + "))

#######################
### A) RP data only ###
#######################
modA <- logit_RP(formula = formula_prem, data = dta, var = rp_indep_prem, weights = NULL)

# Marginal effects
mA_mfx <- mfx_func(modA$coef, modA$vcov, var = rp_indep_prem, data = dta)   %>% data.frame %>% rename(mfx11 = mfx, semfx11 = se_mfx)

# McFadden's pseudo R-squared
mA_R2 <- modA$MF_R2

# Willingness to pay distribution parameters
mA_res <- logit_param_prem(coef_est = modA$coef, vcov_est = modA$vcov, data = dta)

# Percent correct predictions
mA_fit_E85 <- sum(round(modA$fitted)[dta[,rp_dep]==1])/sum(dta[,rp_dep]==1)
mA_fit_E10 <- sum(1-round(modA$fitted)[dta[,rp_dep]==0])/sum(dta[,rp_dep]==0)
mA_fit_total <- (sum(round(modA$fitted)[dta[,rp_dep]==1]) + sum(1-round(modA$fitted)[dta[,rp_dep]==0]))/nrow(dta)

#Number of observations
mA_nobs <- as.numeric(nrow(dta))

###########################
### B) SP-off-RP model  ###
###########################
modB <- sp_wmlelogit(rp.dep = rp_dep, rp.indep = rp_indep_prem, sp.dep = sp_dep, sp.indep = sp_indep_prem, svy.wts = "ONES", data = dta, N_draw = N_draw, par.start = modA$coef)

# Marginal effects
mB_mfx <- modB$mfx %>% rename(mfxB = `MFX`, semfxB = `MFX SE`)
rownames(mB_mfx) <- rownames(modB$coef)[1:length(rownames(modB$coef))-1]

# McFadden's pseudo R-squared
mB_R2 <- modB$R2

# Willingness to pay distribution parameters
mB_res <- logit_param_prem(coef_est = setNames(as.numeric(modB$coef[1:(nrow(modB$coef)-1),1]), rp_indep_prem), vcov_est = modB$vcov[1:(nrow(modB$coef)-1), 1:(nrow(modB$coef)-1)], data = dta)

# Percent correct RP predictions
dta$PPB <- as.numeric(plogis(as.matrix(dta[,rp_indep_prem])%*%modB$coef$Est[1:length(modB$coef$Est)-1]))
mB_fit_E85 <- sum(round(dta$PPB)[dta[,rp_dep]==1]) / sum(dta[,rp_dep]==1)
mB_fit_E10 <- sum(1-round(dta$PPB)[dta[,rp_dep]==0]) / sum(dta[,rp_dep]==0)
mB_fit_total <- (sum(round(dta$PPB)[dta[,rp_dep]==1]) + sum(1-round(dta$PPB)[dta[,rp_dep]==0]))/nrow(dta)

# Percent correct SP predictions
dta$PPBsp <- as.numeric(plogis(as.matrix(dta[,sp_indep_prem])%*%modB$coef$Est[1:length(modB$coef$Est)-1]))
mB_fit_sp_E85 <- sum(round(dta$PPBsp)[dta[,sp_dep]==1]) / sum(dta[,sp_dep]==1)
mB_fit_sp_E10 <- sum(1-round(dta$PPBsp)[dta[,sp_dep]==0]) / sum(dta[,sp_dep]==0)
mB_fit_sp_total <- (sum(round(dta$PPBsp)[dta[,sp_dep]==1]) + sum(1-round(dta$PPBsp)[dta[,sp_dep]==0]))/nrow(dta)

#Number of observations
mB_nobs <- as.numeric(nrow(dta))

#Garbage collection to free-up memory
gc()

#######################################################
### C) SP-off-RP model - endogenous stratification  ###
#######################################################
modC <- sp_wmlelogit(rp.dep = rp_dep, rp.indep = rp_indep_prem, sp.dep = sp_dep, sp.indep = sp_indep_prem, svy.wts = "SW3", data = dta, N_draw = N_draw, par.start = modA$coef)

# Marginal effects
mC_mfx <- modC$mfx %>% rename(mfxC = `MFX`, semfxC = `MFX SE`)
rownames(mC_mfx) <- rownames(modC$coef)[1:length(rownames(modC$coef))-1]

# McFadden's pseudo R-squared
mC_R2 <- modC$R2

# Willingness to pay distribution parameters
mC_res <- logit_param_prem(coef_est = setNames(as.numeric(modC$coef[1:(nrow(modC$coef)-1),1]), rp_indep_prem), vcov_est = modC$vcov[1:(nrow(modC$coef)-1), 1:(nrow(modC$coef)-1)], data = dta)

# Percent correct RP predictions
dta$PPC <- as.numeric(plogis(as.matrix(dta[,rp_indep_prem])%*%modC$coef$Est[1:length(modC$coef$Est)-1]))
mC_fit_E85 <- sum(round(dta$PPC)[dta[,rp_dep]==1]) / sum(dta[,rp_dep]==1)
mC_fit_E10 <- sum(1-round(dta$PPC)[dta[,rp_dep]==0]) / sum(dta[,rp_dep]==0)
mC_fit_total <- (sum(round(dta$PPC)[dta[,rp_dep]==1]) + sum(1-round(dta$PPC)[dta[,rp_dep]==0]))/nrow(dta)

# Percent correct SP predictions
dta$PPCsp <- as.numeric(plogis(as.matrix(dta[,sp_indep_prem])%*%modC$coef$Est[1:length(modC$coef$Est)-1]))
mC_fit_sp_E85 <- sum(round(dta$PPCsp)[dta[,sp_dep]==1]) / sum(dta[,sp_dep]==1)
mC_fit_sp_E10 <- sum(1-round(dta$PPCsp)[dta[,sp_dep]==0]) / sum(dta[,sp_dep]==0)
mC_fit_sp_total <- (sum(round(dta$PPCsp)[dta[,sp_dep]==1]) + sum(1-round(dta$PPCsp)[dta[,sp_dep]==0]))/nrow(dta)

#Number of observations
mC_nobs <- as.numeric(nrow(dta))

#Garbage collection to free-up memory
gc()

################################################
### Summary table of distribution parameters ###
################################################

r1 <- cbind(sum_func_prem_new("A"), sum_func_prem_new("B"), sum_func_prem_new("C")) %>% round(3)

if(nrow(dta) == 479){write.xlsx(r1, file="Results/Table C3 - 479 obs.xlsx")}
if(nrow(dta) > 479){write.xlsx(r1, file="Results/Table C3 - 670 obs.xlsx")}

###################################################################
### Table comparing marginal effects and SE's of premium models ###
###################################################################

prem_mfx <- cbind(mA_mfx[-1,], mB_mfx[-1,1:2], mC_mfx[-1,1:2])
colnames(prem_mfx) <- c("mA_mfx", "mA_mfx_se", "mB_mfx", "mB_mfx_se", "mC_mfx", "mC_mfx_se")

if(nrow(dta) == 479){write.xlsx(prem_mfx, file= "Results/Table C1.xlsx")}
if(nrow(dta) > 479){write.xlsx(prem_mfx, file= "Results/Table C2.xlsx")}

########################
###   Save Results   ###
########################

if(nrow(dta) == 479){save.image("Results/SP-off-RP with 479 obs - premium.RData")}
if(nrow(dta) > 479){save.image("Results/SP-off-RP with 670 obs - premium.RData")}


