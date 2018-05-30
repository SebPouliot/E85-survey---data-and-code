
#########################
###   Load packages   ###
#########################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, actuar, viridis)
pacman::p_load(extrafont) #The package extrafont imports fonts that can be used with ggplot2 
#font_import() #To run once under windows to import fonts
loadfonts(device = "win") #Load the fonts

###################
### Simulations ###
###################

#Motorists have log-logistic preferences

#Parameters of the utility function are:
b0 <- 0
b1 <- 0.1
b2 <- 0.1

#Generate data for simulations
set.seed(123457)

N_pop <- 10000000

#Data
dta0 <- data.frame(array(NA, c(N_pop, 2))) %>% 
  tbl_df %>%
  dplyr::rename(pe = X1, pg = X2)

dta0 <- dta0 %>%
  mutate(pe = 2.3*rlnorm(N_pop, meanlog = log(1), sdlog = 0.1),
         pg = 2.3*rlnorm(N_pop, meanlog = log(0.9), sdlog = 0.1),
         c = runif(N_pop, -5000, 5000),
         c = ifelse(c > 0, c, 0),
         ratio = log((pe + c)/pg),
         x1 = rlnorm(N_pop, meanlog = 1, sdlog = 0.5),
         epsilon = rllogis(N_pop, scale = 0.8, shape = 4),
         V0 = (b0 + b1*ratio + b2*x1)/epsilon,
         y = as.numeric(V0 > 1))


#Share of motorists who choose E85
mean(dta0$y)
mean(dta0$y[dta0$c==0])
summary(dta0$V0>1)

#Data where motorists who fueled with E10 and c > 0 are removed
dta1 <- dta0 %>% 
  dplyr::filter(!(y ==0 & c > 0)) %>% 
  mutate(one  = 1, fuel = ifelse(y==1, "E85", "E10"), fuel = ifelse(c > 0, "E85 dist", fuel))

#Motorists who do not drive out for E85
dta_random <- dta1 %>% 
  dplyr::filter(c==0)  %>% 
  mutate(fuel = "Motorists who access the fuel station at zero cost") 

dta_added <- dta1 %>% 
  dplyr::filter(c>0) %>% 
  mutate(fuel = "E85 motorists who incur a cost to access the fuel station") 

dta_plot <- rbind(dta_random, dta_added)

mean(dta_plot$y)
mean(dta_plot$y[dta_plot$c==0])


#Setup for the figures
mytheme <-  theme(plot.title = element_blank(),
                  text=element_text(size=10, family = "Times New Roman", colour = 'black'),
                  axis.text.y = element_blank(), 
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(), 
                  axis.ticks.y = element_blank(),
                  axis.title.y = element_blank(), 
                  axis.title.x = element_blank(),
                  axis.line.x = element_blank(),
                  axis.line.y = element_blank(), 
                  panel.border = element_blank(), 
                  panel.background =  element_blank(),
                  plot.margin=unit(c(0.5,0.5,0.5,0.5), "lines"), 
                  legend.title=element_blank(), 
                  legend.text = element_text(size = 8, colour = 'black', family = "Times New Roman"), 
                  legend.background=element_blank(),
                  legend.key=element_blank(),
                  legend.position=c(0.70, 0.75), 
                  legend.direction="vertical")


plot_distribution <- ggplot(data = dta_plot, aes(x = V0, ..count.., fill = fuel)) + 
  geom_histogram(bins = 250, alpha = 0.5, position  = "identity", color = "black", size = 0.1) + 
  scale_fill_manual(values = viridis(10)[c(3,8)]) + 
  geom_vline(xintercept  = 1, size = 1, color = "black") + 
  xlim(0.0,3) + 
  mytheme

plot_distribution

ggsave(plot_distribution, filename = "Figures/Figure 1.png",  width = 6, height = 4, units = "in", dpi = 600) 


