##############
### Graph ####
##############

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2)
pacman::p_load(grid)#for arrow in graph
suppressMessages(pacman::p_load(gridExtra))
pacman::p_load(scales)
pacman::p_load(ggthemes)
suppressMessages(pacman::p_load(extrafont)) #The package extrafont imports fonts that can be used with ggplot2 
#font_import()
suppressMessages(loadfonts(device = "win")) #Load the fonts
pacman::p_load(viridis)


#Setup for the figures
mytheme <-  theme(plot.title = element_text(size = 10, colour = 'black', family = "Times New Roman"),
                  text=element_text(size=10, family = "Times New Roman", colour = 'black'),
                  axis.ticks.x = element_line(color='black'), 
                  axis.ticks.y = element_line(color='black'), 
                  axis.text.y = element_text(colour = 'black', family = "Times New Roman"), 
                  axis.text.x = element_text(size = 10, colour = 'black', family = "Times New Roman"),
                  panel.border = element_blank(), 
                  axis.line.x = element_line(color='black'),  
                  axis.line.y = element_line(color='black'),  
                  plot.margin=unit(c(0.5,0.5,0.5,0.5), "lines"), 
                  legend.title=element_blank(), 
                  legend.text = element_text(size = 8, colour = 'black', family = "Times New Roman"), 
                  legend.background=element_blank(),
                  legend.key=element_blank(),
                  legend.position="bottom")




