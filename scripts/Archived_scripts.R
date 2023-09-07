### SYSTEMATIC MAP #####

# TRACKING R CoDE THAT WILL NOT BE USED IN PUBLICATION

######################################
########### FIGURES ##################
######################################

#plotting articles over time
yyear_av<- ggplot(year.count, aes(x= Year, y=Year_count))  + 
  geom_point(size= 1.0, colour= "#c51b8a") + lab + geom_line(size= 0.7, colour= "#c51b8a") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 7.5, angle = 90, 
                                   colour = "black")) + 
  scale_x_continuous(breaks = c(1980, 1985, 1990, 1995,2000, 2005, 2010, 2015, 2020), limits = c(1979, 2021)) +
  scale_y_continuous(breaks = c(5,10,15,20,25), 
                     limits = c(0,25))
byyear_av + alltheme + geom_point(data= fyear.count, aes(x=Year, y = Year_count), size= 1.0, colour = "#31a354") 
  geom_line(data= fyear.count, aes(x=Year, y = Year_count), size= 0.7, colour = "#31a354") 


