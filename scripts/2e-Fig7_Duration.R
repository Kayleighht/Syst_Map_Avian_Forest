#packagesneeded
source('scripts/0-packages.R')

# themes 
source('scripts/0-themes.R')

# Figure 7 ----------------------------------------------------------------
# Number of publications according to study duration

#AVIAN
Duration<- read.csv("out/Duration.count2.csv")
sum(Duration$n)

#create column for percentage for plotting
Duration$percent <- ((Duration$n/277)*100)
sum(Duration$percent)   #confirm the percentage adds to 100
Duration <- Duration[-5,]

#PLOT 

duration.plot <- ggplot(Duration, aes(x= reorder(duration_bin, -percent) , y= percent)) +
  geom_bar(stat= 'identity', fill = birdcol, width = 0.6) + 
  theme_hc() + labs(x= "Time-scale considered", y= "Percent of Studies") +
  
  theme(axis.text.x = element_text(size = 11, angle = -20), axis.ticks.x = element_blank()) + 
  
  scale_x_discrete(labels = c("[0-1 years]", "[1-5 years]", "[5-10 years]", "[10-100 years]")) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50), limits = c(0,55))


bird.durationplot<- duration.plot + alltheme
bird.durationplot

#FOREST#

Duration2<- read.csv("out/Studydurationall.csv")
sum(Duration2$n)

#create column for percentage for plotting
Duration2$percent <- ((Duration2$n/169)*100)

duration.plot <- ggplot(Duration2, aes(x= reorder(duration_bin, -percent) , y= percent)) +
  geom_bar(stat= 'identity', position = 'dodge', fill= forestcol, width = 0.6) + theme_hc() + labs(x= "Time-scale considered", y= "Percent of Studies") +
  
  theme(axis.text.x = element_text(size = 11, angle = -20), axis.ticks.x = element_blank()) + 
  
  scale_x_discrete(labels = c("[0-1 years]", "[1-5 years]", "[5-10 years]", "[10-100 years]")) + 
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50), limits = c(0,55))

forest.durationplot<- duration.plot + alltheme 
forest.durationplot

#COMBINE
durationall<- ggarrange(bird.durationplot, forest.durationplot,
                        labels = c("A", "B"),
                        ncol = 2, nrow = 1,
                        hjust = -1,
                        common.legend = TRUE)
durationall


## COMBINE IN ONE STACKED BAR CHART
durationall<- rbind(Duration2, Duration)
durationall$component <- c("Forest", "Forest", "Forest", "Forest", "Avian", "Avian", "Avian", "Avian")
durationall$duration_binclean <- c("0-1", "2-5", "6-10", "11-100", "0-1", "2-5", "6-10", "11-100")

#factor the duration
durationall$duration_binclean <- factor(durationall$duration_binclean,levels = c("0-1", "2-5", "6-10", "11-100"))

duration.plotall <- ggplot(durationall, aes(x= duration_binclean, y=percent, fill=component)) +
  geom_bar(stat='identity', position='dodge', width= 0.9) +
  labs(x= "Time-Scale Considered (years)", y= "Percent of Studies") +
  
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50), limits = c(0,55)) +
  scale_fill_manual(values=c(birdcol, forestcol)) + alltheme + theme_legend5 +
  labs(fill = "Topic")
duration.plotall

ggsave(filename ="graphics/Figure7.png", width = 16, height = 13, device='tiff', dpi=300)  
