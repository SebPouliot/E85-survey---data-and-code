
# Clear memory
rm(list = ls())

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(numDeriv)
pacman::p_load(tidyverse)
pacman::p_load(stringr)


########################################
### Premium  - with 670 observations ###
########################################

load("Results/SP-off-RP with 670 obs - premium.RData")

rm(list = objects()[!(objects() %in% c("dta", "modA", "modB", "rp_indep_prem"))])

mod_RP_prem <- modA
mod_SPRP_prem <- modB

rm(modA, modB)

#Number of points for the graph
n_points <- 100

dta_graph <- data.frame(array(NA,c(n_points, 9))) %>% tbl_df()
colnames(dta_graph) <- c("prem", "prob_RP_Y", "se_RP_Y", "prob_RP_Z", "se_RP_Z", "prob_SPRP_Y", "se_SPRP_Y", "prob_SPRP_Z", "se_SPRP_Z")
dta_graph <- dta_graph %>% mutate(prem = seq(-1.5, 1.5, length.out = nrow(dta_graph)))

source("Code/Demand graph func.R")

for (i in 1:nrow(dta_graph)){
  probs_RP <-  prob_e85_prem(coef_est = mod_RP_prem$coef, vcov_est = mod_RP_prem$vcov, data = dta, prem = dta_graph$prem[i])
  dta_graph$prob_RP_Y[i] <- probs_RP[1,1]
  dta_graph$se_RP_Y[i] <- probs_RP[2,1]
  dta_graph$prob_RP_Z[i] <- probs_RP[1,2]
  dta_graph$se_RP_Z[i] <- probs_RP[2,2]
  probs_SPRP <-  prob_e85_prem(coef_est = setNames(as.numeric(mod_SPRP_prem$coef[rp_indep_prem,1]), rp_indep_prem), vcov_est = mod_SPRP_prem$vcov[-nrow(mod_SPRP_prem$vcov),-ncol(mod_SPRP_prem$vcov)], data = dta, prem = dta_graph$prem[i])
  dta_graph$prob_SPRP_Y[i] <- probs_SPRP[1,1]
  dta_graph$se_SPRP_Y[i] <- probs_SPRP[2,1]
  dta_graph$prob_SPRP_Z[i] <- probs_SPRP[1,2]
  dta_graph$se_SPRP_Z[i] <- probs_SPRP[2,2]
}

quant <- qnorm(0.975, mean = 0, sd = 1)

dta_graph <- dta_graph %>% 
  mutate(max_RP_Y = prob_RP_Y + quant * se_RP_Y, 
         max_RP_Y = ifelse(max_RP_Y >1, 1, max_RP_Y), 
         min_RP_Y = prob_RP_Y - quant * se_RP_Y, 
         min_RP_Y = ifelse(min_RP_Y <0, 0, min_RP_Y), 
         max_RP_Z = prob_RP_Z + quant * se_RP_Z, 
         max_RP_Z = ifelse(max_RP_Z >1, 1, max_RP_Z), 
         min_RP_Z = prob_RP_Z - quant * se_RP_Z, 
         min_RP_Z = ifelse(min_RP_Z <0, 0, min_RP_Z), 
         max_SPRP_Y = prob_SPRP_Y + quant * se_SPRP_Y, 
         max_SPRP_Y = ifelse(max_SPRP_Y >1, 1, max_SPRP_Y), 
         min_SPRP_Y = prob_SPRP_Y - quant * se_SPRP_Y, 
         min_SPRP_Y = ifelse(min_SPRP_Y <0, 0, min_SPRP_Y), 
         max_SPRP_Z = prob_SPRP_Z + quant * se_SPRP_Z, 
         max_SPRP_Z = ifelse(max_SPRP_Z >1, 1, max_SPRP_Z), 
         min_SPRP_Z = prob_SPRP_Z - quant * se_SPRP_Z, 
         min_SPRP_Z = ifelse(min_SPRP_Z <0, 0, min_SPRP_Z))

dta_mean <- dta_graph %>% 
    dplyr::select(prem, prob_RP_Y, prob_SPRP_Y, prob_RP_Z, prob_SPRP_Z) %>% 
    gather(key, value, -prem) %>% 
    mutate(retailer = ifelse(str_detect(key, "_Y"), "Retailer Y", "Retailer Z"), 
           key = ifelse(str_detect(key, "_RP"), "A) RP data", "B) SP-off-RP"))
  
dta_min <- dta_graph %>% 
    dplyr::select(prem, min_RP_Y, min_SPRP_Y, min_RP_Z, min_SPRP_Z) %>% 
    gather(key, min, -prem) %>% 
    mutate(retailer = ifelse(str_detect(key, "_Y"), "Retailer Y", "Retailer Z"), 
           key = ifelse(str_detect(key, "_RP"), "A) RP data", "B) SP-off-RP"), 
           min = ifelse(min<0, 0, min))
  
dta_max <- dta_graph %>% 
    dplyr::select(prem, max_RP_Y, max_SPRP_Y, max_RP_Z, max_SPRP_Z) %>% 
    gather(key, max, -prem) %>% 
    mutate(retailer = ifelse(str_detect(key, "_Y"), "Retailer Y", "Retailer Z"), 
           key = ifelse(str_detect(key, "_RP"), "A) RP data", "B) SP-off-RP"), 
           max = ifelse(max>1, 1, max))
  
dta_graph_670 <- left_join(dta_mean, dta_min) %>% left_join(dta_max)

dta_graph_670 <- dta_graph_670 %>% mutate(obs = "670 obs.")

######################################
### Ratio  - with 479 observations ###
######################################

load("Results/SP-off-RP with 479 obs - premium.RData")

rm(list = objects()[!(objects() %in% c("dta", "modA", "modB", "rp_indep_prem", "dta_graph_670"))])

mod_RP_prem <- modA
mod_SPRP_prem <- modB

rm(modA, modB)

#Number of points for the graph
n_points <- 100

dta_graph <- data.frame(array(NA,c(n_points, 9))) %>% tbl_df()
colnames(dta_graph) <- c("prem", "prob_RP_Y", "se_RP_Y", "prob_RP_Z", "se_RP_Z", "prob_SPRP_Y", "se_SPRP_Y", "prob_SPRP_Z", "se_SPRP_Z")
dta_graph <- dta_graph %>% mutate(prem = seq(-1.5, 1.5, length.out = nrow(dta_graph)))

source("Code/Demand graph func.R")

for (i in 1:nrow(dta_graph)){
  probs_RP <-  prob_e85_prem(coef_est = mod_RP_prem$coef, vcov_est = mod_RP_prem$vcov, data = dta, prem = dta_graph$prem[i])
  dta_graph$prob_RP_Y[i] <- probs_RP[1,1]
  dta_graph$se_RP_Y[i] <- probs_RP[2,1]
  dta_graph$prob_RP_Z[i] <- probs_RP[1,2]
  dta_graph$se_RP_Z[i] <- probs_RP[2,2]
  probs_SPRP <-  prob_e85_prem(coef_est = setNames(as.numeric(mod_SPRP_prem$coef[rp_indep_prem,1]), rp_indep_prem), vcov_est = mod_SPRP_prem$vcov[-nrow(mod_SPRP_prem$vcov),-ncol(mod_SPRP_prem$vcov)], data = dta, prem = dta_graph$prem[i])
  dta_graph$prob_SPRP_Y[i] <- probs_SPRP[1,1]
  dta_graph$se_SPRP_Y[i] <- probs_SPRP[2,1]
  dta_graph$prob_SPRP_Z[i] <- probs_SPRP[1,2]
  dta_graph$se_SPRP_Z[i] <- probs_SPRP[2,2]
}

quant <- qnorm(0.975, mean = 0, sd = 1)

dta_graph <- dta_graph %>% 
  mutate(max_RP_Y = prob_RP_Y + quant * se_RP_Y, 
         max_RP_Y = ifelse(max_RP_Y >1, 1, max_RP_Y), 
         min_RP_Y = prob_RP_Y - quant * se_RP_Y, 
         min_RP_Y = ifelse(min_RP_Y <0, 0, min_RP_Y), 
         max_RP_Z = prob_RP_Z + quant * se_RP_Z, 
         max_RP_Z = ifelse(max_RP_Z >1, 1, max_RP_Z), 
         min_RP_Z = prob_RP_Z - quant * se_RP_Z, 
         min_RP_Z = ifelse(min_RP_Z <0, 0, min_RP_Z), 
         max_SPRP_Y = prob_SPRP_Y + quant * se_SPRP_Y, 
         max_SPRP_Y = ifelse(max_SPRP_Y >1, 1, max_SPRP_Y), 
         min_SPRP_Y = prob_SPRP_Y - quant * se_SPRP_Y, 
         min_SPRP_Y = ifelse(min_SPRP_Y <0, 0, min_SPRP_Y), 
         max_SPRP_Z = prob_SPRP_Z + quant * se_SPRP_Z, 
         max_SPRP_Z = ifelse(max_SPRP_Z >1, 1, max_SPRP_Z), 
         min_SPRP_Z = prob_SPRP_Z - quant * se_SPRP_Z, 
         min_SPRP_Z = ifelse(min_SPRP_Z <0, 0, min_SPRP_Z))

dta_mean <- dta_graph %>% 
  dplyr::select(prem, prob_RP_Y, prob_SPRP_Y, prob_RP_Z, prob_SPRP_Z) %>% 
  gather(key, value, -prem) %>% 
  mutate(retailer = ifelse(str_detect(key, "_Y"), "Retailer Y", "Retailer Z"), 
         key = ifelse(str_detect(key, "_RP"), "A) RP data", "B) SP-off-RP"))

dta_min <- dta_graph %>% 
  dplyr::select(prem, min_RP_Y, min_SPRP_Y, min_RP_Z, min_SPRP_Z) %>% 
  gather(key, min, -prem) %>% 
  mutate(retailer = ifelse(str_detect(key, "_Y"), "Retailer Y", "Retailer Z"), 
         key = ifelse(str_detect(key, "_RP"), "A) RP data", "B) SP-off-RP"), 
         min = ifelse(min<0, 0, min))

dta_max <- dta_graph %>% 
  dplyr::select(prem, max_RP_Y, max_SPRP_Y, max_RP_Z, max_SPRP_Z) %>% 
  gather(key, max, -prem) %>% 
  mutate(retailer = ifelse(str_detect(key, "_Y"), "Retailer Y", "Retailer Z"),                                 key = ifelse(str_detect(key, "_RP"), "A) RP data", "B) SP-off-RP"), 
         max = ifelse(max>1, 1, max))

dta_graph_479 <- left_join(dta_mean, dta_min) %>% left_join(dta_max)

dta_graph_479 <- dta_graph_479 %>% mutate(obs = "479 obs.")

######################################
### Bind data and make facet graph ###
######################################

dta_graph <- rbind(dta_graph_479, dta_graph_670)

source("Code/Graph parameters.R")

#Graph with only retailer Y
plot <- ggplot(dta_graph, aes(y = value, x = prem, color = key, fill = key, ymin = min, ymax = max)) + 
  geom_ribbon(alpha = 0.5, linetype = 0, color = "black", size = 0.25) + 
  geom_line(size = 1.5) + 
  coord_flip() +
  facet_grid(obs ~ retailer) +
  xlab("E85 premium ($/gal)") + 
  ylab("Share of motorists who purchase E85") + 
  scale_color_manual(values = viridis(10)[c(3,7)]) + 
  scale_fill_manual(values = viridis(10)[c(3,8)]) + 
  scale_x_continuous(expand = c(0,0)) + #scale_y_continuous(breaks = seq(0, 1, by = 0.25), expand = c(0,0)) + 
  theme_bw() + 
  mytheme + 
  theme(legend.position = c(0.5, 0.25), legend.direction = "horizontal", plot.margin=unit(c(0.5,1,0.5,0.5), "lines"), axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black')) 

plot

ggsave(plot, filename = "Figures/Figure C1.png",  width = 6, height = 6, units = "in", dpi = 600)









