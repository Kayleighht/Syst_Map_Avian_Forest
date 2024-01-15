#packagesneeded
source('scripts/0-packages.R')

# themes 
source('scripts/0-themes.R')

# Figure X ----------------------------------------------------------------
# Percent Publications According to One/Many Indicators

BIndicator.ct<- read.csv("out/Allind.count.csv")
sum(BIndicator.ct$number_publications)
#add column with percents
BIndicator.ct$percent<- (BIndicator.ct$number_publications/347)*100

indicator.plot <- ggplot(BIndicator.ct, aes(x= reorder(number_topics, -percent), y= percent)) +
  geom_bar(stat= 'identity', fill= birdpalette4) + theme(axis.text.x = element_text(size= 10))+
  theme_hc() + labs(x = "", y=" Percent of Studies") + coord_flip()
bird.indicatorplot<- indicator.plot + alltheme

#FOREST#
Indicator.ct<- read.csv("out/FAllind.count.csv")
sum(Indicator.ct$number_publications)
#add column with percents
Indicator.ct$percent<- (Indicator.ct$number_publications/278)*100

indicator.plot <- ggplot(Indicator.ct, aes(x= reorder(number_topics, -percent), y= percent)) +
  geom_bar(stat= 'identity', fill= forestpalette5) + theme(axis.text.x = element_text(size= 10)) +
  theme_hc() + labs(x = "", y=" Percent of Studies") + scale_y_continuous(limits = c(0,80))
forest.indicatorplot<- indicator.plot + alltheme +coord_flip()

all.indicatorplot<- ggarrange(bird.indicatorplot, forest.indicatorplot,
                              labels = c("Bird", "Forest"),
                              font.label = list(color= "black"),
                              ncol = 2, nrow = 1)
all.indicatorplot

## COMBINED

indicatorall<- rbind(BIndicator.ct, Indicator.ct)
indicatorall$component <- c("Avian", "Avian", "Avian", "Avian", "Forest", "Forest", "Forest", "Forest", "Forest")

#factor indicators for ordering
indicatorall$number_topics <- factor(indicatorall$number_topics, levels = c("One Indicator", "Two Indicators", "Three Indicators", 
                                                                            "Four Indicators", "Five Indicators"))

#PLOT
ind.plotall <- ggplot(indicatorall, aes(x= number_topics, y=percent, fill=component)) +
  geom_col(position= position_dodge2(preserve = "single"), width= 0.9) +
  labs(x= "Indicators measured", y= "Percent of Studies") +
  
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100), limits = c(0,100)) +
  scale_fill_manual(values=c(birdcol, forestcol)) + alltheme + theme_legend5 +
  labs(fill = "Topic") + scale_x_discrete(labels=c("One Indicator" = "One", "Two Indicators" = "Two",
                                                   "Three Indicators" = "Three", "Four Indicators" = "Four", "Five Indicators" = "Five"))


ind.plotall

ggsave(filename ="graphics/FigureX.png", width = 16, height = 13, device='tiff', dpi=300)  

