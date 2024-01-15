#packagesneeded
source('scripts/0-packages.R')

# themes 
source('scripts/0-themes.R')



# Figure 4 ----------------------------------------------------------------
# Publications by Year

fyear.count<- read.csv("out/FYear.count.csv")
year.count <- read.csv("out/Year.count.csv")
#remove 2022 since we don't have the entire year
year.count <- year.count[-19,]
fyear.count <- fyear.count[-4,]

#set x and y labels
lab<- labs(x= "Year", y= "Number of Publications")

#plotting articles over time
byyear_av<- ggplot(year.count, aes(x= Year, y=Year_count))  + 
  geom_line(size= 0.7, colour= birdcol) + lab  +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 25, angle = 90, 
                                   colour = "black")) + 
  scale_x_continuous(breaks = c(1980, 1985, 1990, 1995,2000, 2005, 2010, 2015, 2020), limits = c(1979, 2021)) +
  scale_y_continuous(breaks = c(5,10,15,20,25), 
                     limits = c(0,25)) + geom_area(fill = birdcol)

publication.years<- byyear_av + alltheme +
  geom_line(data= fyear.count, aes(x=Year, y = Year_count), size= 0.7, colour = forestcol) +
  geom_area(data= fyear.count, aes(x=Year, y = Year_count), fill= forestcol) 

publication.years 

#### SAVE FILE
ggsave(filename ="graphics/Figure4.png", width = 16, height = 13, dpi=300)  

