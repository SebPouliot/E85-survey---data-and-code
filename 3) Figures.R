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

#Graph parameters
source("Code/Graph parameters.R")

############################
###   Load the dataset   ###
############################

dta <- readRDS("Data/SP-off-RP data.rds")

# We can do figures using either of the two samples mentioned in the paper. The figures in the paper are made with the 670 observations sample.

#dta <- dta %>% dplyr::filter(QD ==0) #Did not drive out of their way (479 obs)
dta <- dta %>% dplyr::filter(DIST ==0) #Zero distance (670 obs)

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
  
#####################
###### Graphs #######
#####################


################################################################
### Share E85 in function of price diff - not shown in paper ###
################################################################

df <- dta %>% mutate(State = ifelse(STNST_IA == 1, "Iowa", ifelse(STNST_CO == 1, "Colorado", ifelse(STNST_CA == 1, "California", ifelse(STNST_AR == 1, "Arkansas", "Oklahoma")))), share = E85) %>% 
  dplyr::select(PREM, share, State) %>%  
  group_by(PREM, State) %>%
  summarize(share = mean(share), total = n())

survey_diff <- ggplot(data = df, aes(y = PREM, x = share, size = total, color=State)) +
  geom_point(aes(color=State), alpha=0.75) + 
  scale_color_viridis(discrete = TRUE) + 
  scale_size(guide='none') + 
  stat_smooth(aes(weight = total), color = 1, method=lm, se=FALSE, size=1) +
  ylab("E85 premium ($/gal)") + 
  xlab("Share of motorists who chose E85") + 
  theme_bw() + 
  mytheme + 
  theme(legend.position=c(0.85, 0.80), legend.direction = "horizontal") + guides(col = guide_legend(ncol = 2)) + theme(axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'))

survey_diff

######################################################
### Figure 2: Share E85 in function of price ratio ###
######################################################

dfr <- dta %>% 
  mutate(State = ifelse(STNST_IA == 1, "Iowa", ifelse(STNST_CO == 1, "Colorado", ifelse(STNST_CA == 1, "California", ifelse(STNST_AR == 1, "Arkansas", "Oklahoma")))), share = E85) %>% 
  dplyr::select(RATIO, share, State) %>%  
  group_by(RATIO, State) %>%
  summarize(share = mean(share), total = n())

survey_ratio <- ggplot(data = dfr, aes(y = RATIO, x = share, size = total, color=State)) + 
  geom_hline(yintercept = 1, color="gray", size=1) + 
  geom_point(aes(color=State), alpha=0.75) + 
  scale_color_viridis(discrete = TRUE) + 
  scale_size(guide='none') + 
  stat_smooth(aes(weight = total), color = 1, method=lm, se=FALSE, size=1) + 
  scale_y_continuous(breaks=seq(0.9,1.3, by=0.1), labels=seq(0.9,1.3, by=0.1)) + 
  ylab("Price ratio (E85/E10)") + 
  xlab("Share E85") + 
  theme_bw() + 
  mytheme + 
  theme(legend.position=c(0.85, 0.80), legend.direction = "horizontal") + 
  guides(col = guide_legend(ncol = 2)) + 
  theme(axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'))


ggsave(survey_ratio, filename = "Figures/Figure 2.png",  width = 6, height = 4, units = "in", dpi = 600) 

#################################
###### Stated preferences #######
#################################

#### Price difference - not shown in paper
df2 <- dta %>% mutate(State = ifelse(STNST_IA == 1, "Iowa", ifelse(STNST_CO == 1, "Colorado", ifelse(STNST_CA == 1, "California", ifelse(STNST_AR == 1, "Arkansas", "Oklahoma")))), share = SP_E85, RP_choice = ifelse(E85==1, "E85", "E10")) %>% 
  dplyr::select(HPREM, RP_choice, share, State) %>%  
  group_by(HPREM, RP_choice, State) %>%
  summarize(share = mean(share), total = n())

survey_diff_Y <- ggplot(data = df2 %>% dplyr::filter(State != "California"), aes(y = HPREM, x = share, size = total, color = RP_choice)) + 
  geom_point(alpha=0.75) + 
  geom_smooth(aes(weight = total), method=lm, se=FALSE, size=1)  + 
  ylab("Hypothetical E85 premium ($/gal)") +
  xlab("Share of motorists who chose E85") + 
  theme_bw() + 
  mytheme + 
  theme(legend.position = c(0.75, 0.05), legend.title = element_text(size = 10, colour = 'black', family = "Times New Roman"), legend.direction = "horizontal", strip.background = element_blank()) + 
  guides(col = guide_legend(ncol = 2, title = "RP choice:")) + 
  scale_color_manual(values = viridis(10)[c(1,8)]) + 
  scale_size(guide = 'none') + 
  theme(axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'))

survey_diff_Y


survey_diff_Z <- ggplot(data = df2 %>% dplyr::filter(State == "California"), aes(y = HPREM, x = share, size = total, color = RP_choice)) + 
  geom_point(alpha=0.75) + geom_smooth(aes(weight = total), method=lm, se=FALSE, size=1)  + 
  ylab("Hypothetical E85 premium ($/gal)") + 
  xlab("Share of motorists who chose E85") + 
  theme_bw() + 
  mytheme + 
  theme(legend.position = c(0.75, 0.05), legend.title = element_text(size = 10, colour = 'black', family = "Times New Roman"), legend.direction = "horizontal", strip.background = element_blank()) + 
  guides(col = guide_legend(ncol = 2, title = "RP choice:")) + 
  scale_color_manual(values = viridis(10)[c(1,8)]) + 
  scale_size(guide = 'none') + 
  theme(axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'))

survey_diff_Z

#################################################
### Figure 3: Price ratio - stated preference ###
#################################################

dfr2 <- dta %>% mutate(State = ifelse(STNST_IA == 1, "Iowa", ifelse(STNST_CO == 1, "Colorado", ifelse(STNST_CA == 1, "California", ifelse(STNST_AR == 1, "Arkansas", "Oklahoma")))), share = SP_E85, RP_choice = ifelse(E85==1, "E85", "E10")) %>% 
  dplyr::select(HRATIO, RP_choice, share, State) %>%  
  group_by(HRATIO, RP_choice, State) %>%
  summarize(share = mean(share), total = n())

survey_ratio_Y <- ggplot(data = dfr2 %>% dplyr::filter(State != "California"), aes(y = HRATIO, x = share, size = total, color = RP_choice)) + 
  geom_hline(yintercept = 1, color="gray", size=1) +
  geom_point(alpha=0.75) + geom_smooth(aes(weight = total), method=lm, se=FALSE, size=1)  + 
  ylab("Hypothetical price ratio (E85/E10)") + 
  xlab("Share of motorists who chose E85") + 
  theme_bw() + 
  mytheme + 
  theme(legend.position = c(0.75, 0.85), legend.title = element_text(size = 10, colour = 'black', family = "Times New Roman"), legend.direction = "horizontal", strip.background = element_blank()) + 
  guides(col = guide_legend(ncol = 2, title = "RP choice:")) + 
  scale_color_manual(values = viridis(10)[c(1,8)]) + 
  scale_size(guide = 'none') + 
  theme(axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'))

ggsave(survey_ratio_Y, filename = "Figures/Figure 3a.png",  width = 6, height = 4, units = "in", dpi = 600) 

survey_ratio_Z <- ggplot(data = dfr2 %>% dplyr::filter(State == "California"), aes(y = HRATIO, x = share, size = total, color = RP_choice)) + 
  geom_hline(yintercept = 1, color="gray", size=1) + 
  geom_point(alpha=0.75) + 
  geom_smooth(aes(weight = total), method=lm, se=FALSE, size=1)  + 
  ylab("Hypothetical price ratio (E85/E10)") + 
  xlab("Share of motorists who chose E85") + 
  theme_bw() + 
  mytheme + 
  theme(legend.position = c(0.75, 0.85), legend.title = element_text(size = 10, colour = 'black', family = "Times New Roman"), legend.direction = "horizontal", strip.background = element_blank()) + 
  guides(col = guide_legend(ncol = 2, title = "RP choice:")) + 
  scale_color_manual(values = viridis(10)[c(1,8)]) + 
  scale_size(guide = 'none') + 
  theme(axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'))

ggsave(survey_ratio_Z, filename = "Figures/Figure 3b.png",  width = 6, height = 4, units = "in", dpi = 600) 

