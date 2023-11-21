#packages needed
Packages <- c("tidyverse", "ggplot2", "maps", "ggthemes", "cartography", "sf", "ggpubr", "plotly", "data.table", "cowplot", "janitor")
lapply(Packages, library,character.only= TRUE)


getwd()
setwd("C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out")

############################# THEME CREATION ####################################

alltheme <- theme(
  panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.background = element_rect(fill = "white", color = NA),
  plot.background = element_rect(fill = "white", color = NA),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
  text = element_text(family = "serif"),
  axis.title.y = element_text(colour = "black",face = "bold", size = '40', vjust = +1),
  axis.title.x = element_text(colour = "black", face = "bold", size= '40', vjust= -0.5),
  axis.ticks = element_line(size = 0.7),
  axis.text = element_text(size = 28, colour = "black")
  )

#LEGEND 1 THEME
theme_legend<- theme(
  legend.title = element_text(colour = "black", size = 12, face= "bold"),
  legend.position = c(0.85, 0.80),
  legend.box.background =  element_rect(colour = "black"), legend.box.margin = margin(5,5,5,5))

#LEGEND 2 THEME
theme_legend2<- theme(
  legend.title = element_text(colour = "black", size = 12, face= "bold"),
  legend.position = c(0.70, 0.80),
  legend.box.background =  element_rect(colour = "black"), legend.box.margin = margin(5,5,5,5))

#LEGEND 3 THEME
theme_legend3<- theme(
  legend.title = element_text(colour = "black", size = 12, face= "bold"),
  legend.position = c("right"),
  legend.box.background =  element_rect(colour = "black"), legend.box.margin = margin(5,5,5,5))


forestcol<- ("#DCE319FF")
birdcol<- ("#440154FF")

################################ FIGURE 1 ##################################################################
########################## PUBLICATIONS BY YEAR ###########################################

fyear.count<- read.csv("FYear.count.csv")
year.count <- read.csv("Year.count.csv")
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
ggsave(filename ="UFCBFig1.png", width = 16, height = 13, device='tiff', dpi=300)  

########################## FIGURE _ ###################################################
################### Study Country Heat Map ##################################

# will add Riikka's code here

########################## FIGURE 3 #########################################################
############### # Publication according to Top 10 Journals with Country ###########################################

#will add Riikka's code here

################################## FIGURE __ #########################################################
####################### NUMBER OF PUBLICATIONS ACCORDING TO STUDY DURATION ###############################################

#AVIAN
Duration<- read.csv("Duration.count2.csv")
sum(Duration$n)

#create column for percentage for plotting
Duration$percent <- ((Duration$n/255)*100)
sum(Duration$percent)   #confirm the percentage adds to 100

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

Duration2<- read.csv("FDuration.count2.csv")
sum(Duration2$n)

#create column for percentage for plotting
Duration2$percent <- ((Duration2$n/112)*100)

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
                    labs(x= "Time-scale considered (years)", y= "Percent of Studies") +
                    
                    scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50), limits = c(0,55)) +
                    scale_fill_manual(values=c(birdcol, forestcol)) + alltheme + theme_legend +
                    labs(fill = "Topic")
duration.plotall

ggsave(filename ="UFCBFig2.png", width = 16, height = 13, device='tiff', dpi=300)  

#################################### FIGURE __ ######################################################
############################ TOPICS ONLY ###########################################################

#AVIAN
birdtopics<- read.csv("bird.component.csv")

#create new column with percent of studies
#sum(birdtopics$total)
#birdtopics$percent <- (birdtopics$total/348)*100

birdtopic.plot <- ggplot(birdtopics, aes(x= reorder(Component, -percent) , y= percent)) +
                  geom_bar(stat= 'identity', position = 'dodge', fill= birdcol, width = 0.8) + 
                  
                  theme(axis.text.x = element_text(size= 12), 
                  axis.text.y = (element_text(size = 13, angle = 0))) + alltheme + 
                  labs(x= "", y= "Percent of Studies") + 
                  scale_y_continuous(limits = c(0,75), breaks = c(25, 50, 75)) +
                  coord_flip()

birdtopic.plot<- birdtopic.plot + alltheme
birdtopic.plot

#FOREST 
foresttopics<- read.csv("forest.component.csv")

#create new column with percent of studies

#sum(foresttopics$total)
#foresttopics$percent <- (foresttopics$total/267)*100


foresttopic.plot <- ggplot(foresttopics, aes(x= reorder(Component, -percent) , y= percent)) +
  geom_bar(stat= 'identity', position = 'dodge', fill= forestcol, width = 0.8) + 
  
  theme(axis.text.x = element_text(size= 12), 
        axis.text.y = (element_text(size = 13, angle = 0)))  + alltheme + 
  labs(x= "", y= "Percent of Studies") + 
  scale_y_continuous(limits = c(0,320), breaks = c(50, 100, 150, 200, 250, 300, 350)) +
  coord_flip()

foresttopic.plot<- foresttopic.plot + alltheme
foresttopic.plot

componentall<- ggarrange(birdtopic.plot, foresttopic.plot,
                        ncol = 1, nrow = 2,
                        hjust = -1)

componentall

###################################### FIGURE __ ######################################################################
######################### BIRD SUCCESS COMPONENT BY COMPARATOR #################################################

Compmeta<- read.csv(Birdcomp.counts.csv)

#create column for percentage for plotting
birdpalette2<- c("#481567FF","#404788FF")
#factor the duration
Compmeta$value <- factor(Compmeta$value, levels = c("Biodiversity", "Survival", "Demographics/Patterns", "Breeding", "Behaviour", "Resources", "Foraging"))

comparator.plot<- ggplot(Compmeta, aes(fill = comp, x= value, y= comparator)) +
                  geom_bar(stat= 'identity') + theme(axis.text.y = element_text(size= 13, angle = 0), 
                                                     axis.text.x = element_text(size = 13))+
                  alltheme + labs(x= "", y= "Percent of Studies") + scale_y_continuous(limits = c(0,105), 
                                                                   breaks = c(0, 20, 40, 60, 80, 100)) +
                  scale_fill_manual(labels= c("No", "Yes"), values = birdpalette2) + coord_flip() 
  
bird.comparatorplot<- comparator.plot + alltheme + theme_legend3 + labs(fill= "Comparator 
used?")
bird.comparatorplot


#PLOTTING

Forestcompmeta<- read.csv("FComp.count.csv")

forestpalette2<- c("#B8DE29FF", "#55C667FF")

Forestcompmeta$value <- factor(Forestcompmeta$value, levels = c("Composition", "Land use type", "Canopy cover", "Individual tree management", 
                                                    "Forested area", "Native species", "Exotic/invasive species", "Connectivity",
                                                     "Diversity metric", "Fragmentation"))

comparator.plot<- ggplot(Forestcompmeta, aes(fill = comp, x= value, y= comparator)) +
  geom_bar(stat= 'identity') + theme(axis.text.y = element_text(size= 13, angle = 0), 
                                     axis.text.x = element_text(size = 13))+
  alltheme + labs(x= "", y= "Percent of Studies") + scale_y_continuous(limits = c(0,101), 
                                                                         breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_fill_manual(values= forestpalette2, labels= c("No", "Yes")) + coord_flip()

forest.comparatorplot<-comparator.plot + alltheme + theme_legend3 + labs(fill= "Comparator
used?")
forest.comparatorplot

all.comparatorplot<- ggarrange(bird.comparatorplot, forest.comparatorplot,
                               font.label = list(color= "black"),
                               ncol = 2, nrow = 1)
all.comparatorplot

#######################################      ######################################################################
############################ BIRD SUCCESS COMPONENT BY URBAN SCALE ###########################################

Urban <- read.csv("birdurb.count.csv")
#remove n/a's created from empty rows
Urban <- Urban %>% drop_na(value)
#drop additional N/A
Urban <-Urban[!grepl('N/A', Urban$value),]

#sort alphabetically to create percentages
Urban <- Urban[order(Urban[,2]), ]
#get total for totals
sum(Urban$n)

#spread rows so that we can create percent bars that reach 100%
Urban<- Urban %>% spread(value, n)

#replace NA with 0
Urban[is.na(Urban)] <- 0

#pivot table
Urban <- Urban %>% pivot_longer(., cols = c(`Behaviour`, `Biodiversity`, Breeding, `Demographics/Patterns`, Foraging, Resources, Survival))
Urban <- as.data.frame(Urban)
#sort
Urban <- Urban[order(Urban[,2]), ]
Totals<- c(29,29,29,29,189,189,189,189,49,49,49,49,46,46,46,46,1,1,1,1,7,7,7,7,28,28,28,28)

Urban <-cbind(Urban, Totals)

#now add column of percentages 
Urban$percent <- (Urban$value/Urban$Totals)*100

#PLOTTING ####################################################################

Urban$name <- factor(Urban$name, levels = c("Biodiversity", "Survival", "Demographics/Patterns", "Breeding", "Behaviour", "Resources", "Foraging"))

birdpalette4 <- c("#481567FF", "#404788FF", "#33638DFF", "#238A8Dff")

scale.plot <- ggplot(Urban, aes(fill= Urb.scale, x= name , y= percent)) +
              geom_bar(stat= 'identity') + alltheme + labs(x= "", y= "Percent of Studies") +
              theme(axis.text.x = element_text(colour= "black", size= 12), axis.text.y = element_text(size = 13, colour= "black", angle = 0)) + 
              coord_flip() + scale_y_continuous(limits= c(0,100)) + scale_fill_manual(values = birdpalette4)

bird.scaleplot<- scale.plot + labs(fill= "Urban Scale") + theme_legend3 + alltheme
bird.scaleplot

#FOREST#
Urban <- read.csv("FUrb.count.csv")
#remove n/a's created from empty rows
Urban <- Urban %>% drop_na(value)

#drop additional N/A
Urban <-Urban[!grepl('N/A', Urban$value),]

#sort alphabetically to create percentages
Urban <- Urban[order(Urban[,"value"]), ]
#get total for totals
sum(Urban$n)

#spread rows so that we can create percent bars that reach 100%
Urban<- Urban %>% spread(value, n)

#replace NA with 0
Urban[is.na(Urban)] <- 0

#pivot table
Urban <- Urban %>% pivot_longer(., cols = c(`Composition`, `Connectivity`, `Diversity metric`, `Exotic/invasive species`, 
                                            `Forested area`, Fragmentation, `Individual tree management`, `Land use type`, `Native species`, `Canopy cover`))
Urban <- as.data.frame(Urban)
#sort
Urban <- Urban[order(Urban[,2]), ]
Totals<- c(33,33,33,33,120,120,120,120,3,3,3,3,4,4,4,4,10,10,10,10,29,29,29,29,2,2,2,2,27,27,27,27,101,101,101,101,16,16,16,16)

Urban <-cbind(Urban, Totals)

#now add column of percentages 
Urban$percent <- (Urban$value/Urban$Totals)*100

## PLOT #############################################################################################

Urban$name <- factor(Urban$name, levels = c("Composition", "Land use type", "Canopy cover", "Individual tree management", 
                                                                "Forested area", "Native species", "Exotic/invasive species", "Connectivity",
                                                                "Diversity metric", "Fragmentation"))

forestpalette4 <- c("#20A387FF", "#55C667FF", "#B8DE29FF", "#FDE725FF")


scale.plot <- ggplot(Urban, aes(fill= Urb.scale, x= name, y= percent)) +
  geom_bar(stat= 'identity') + alltheme + labs(x= "", y= "Percent of Studies") +
  theme(axis.text.x = element_text(colour= "black", size= 12), axis.text.y = element_text(size = 13, colour= "black", angle = 0)) + 
  coord_flip() + scale_y_continuous(limits= c(0,100)) + scale_fill_manual(values= forestpalette4)

forest.scaleplot<- scale.plot + alltheme + labs(fill= "Urban Scale") + theme_legend3
forest.scaleplot

#COMBINE ALL
all.scaleplot<- ggarrange(bird.scaleplot, forest.scaleplot,
                               labels = c("Bird", "Forest"),
                               font.label = list(color= "black"),
                               ncol = 2, nrow = 1)
all.scaleplot


############################################ FIGURE _ ############################################
###################################################################################################
############################# CATEGORY BY RECOMMENDATION TYPE #####################################

recs<- read.csv("Allrecraw.count.csv")

#sort based on bird category
recs <- recs[order(recs[,2]), ]
#manually add a column of totals
totals<- c(31,31,31,31,31,224,224,224,224,224,224,224,224,53,53,53,53,53,53,50,50,50,50,50,50,50,1,7,7,7,29,29,29,29,29)
#bind
recs<- cbind(recs, totals)

recs$percent<- (recs$n/recs$totals)*100

##### PLOT

#factor columns to correct order
recs$value <- factor(recs$value, levels = c("Biodiversity", "Survival", "Demographics/Patterns", "Breeding", "Behaviour", "Resources", "Foraging"))
recs$Rec.1 <- factor(recs$Rec.1, levels= c("Restoration", "Management", "Conservation", "No Recommendations"))

birdpalette4s <- c("#2D708EFF", "#481567FF", "#404788FF", "#787276")

rec.plot <- ggplot(recs, aes(fill= Rec.1, x= value , y= percent)) +
  geom_bar(stat= 'identity') + alltheme + labs(x= "", y= "Percent of Studies") +
  theme(axis.text.x = element_text(colour= "black", size= 12), axis.text.y = element_text(size = 13, colour= "black", angle = 0)) + 
  coord_flip() + scale_y_continuous(limits= c(0,100)) + scale_fill_manual(values = birdpalette4s, name =  "Recommendation 
Type") + theme_legend3

birdrecoplot <- rec.plot
birdrecoplot

##### FOREST
recs<- read.csv("FAllrec.count.csv")
#remove leading and trailing zeros
recs<- recs %>% 
  mutate(across(where(is.character), str_trim))

#sort based on bird category
recs <- recs[order(recs[,"value"]), ]

#manually add a column of totals
totals<- c(36,36,36,36,36,36,36, 136,136,136,136,136,136,136,136,136,136,136,136,136,5,5,5,5,5,5,5,5,5,11,
           11,11,11,11,11, 33,33,33,33,33,33,33,33,33,33,2,2,31,31,31,31,31,31,31,31,31,120,120,120,120,
           120,120,120,120,120,120,120,120,120,120,21,21,21,21,21,21,21,21,21)
#bind
recs<- cbind(recs, totals)

recs$percent<- (recs$n/recs$totals)*100

##### PLOT
recs$value <- factor(recs$value, levels = c("Composition", "Land use type", "Canopy cover", "Individual tree management", 
                                            "Forested area", "Native species", "Exotic/invasive species", "Connectivity",
                                            "Diversity metric", "Fragmentation"))
recs$Rec1 <- factor(recs$Rec1, levels= c("Restoration", "Management", "Conservation", "No Recommendations"))


forestpalette4s <- c("#B8DE29FF","#29AF7FFF","#FDE725FF", "#787276")

rec.plot <- ggplot(recs, aes(fill= Rec1, x= value , y= percent)) +
  geom_bar(stat= 'identity') + alltheme + labs(x= "", y= "Percent of Studies") +
  theme(axis.text.x = element_text(colour= "black", size= 12), axis.text.y = element_text(size = 13, colour= "black", angle = 0)) + 
  coord_flip() + scale_y_continuous(limits= c(0,100)) + scale_fill_manual(values = forestpalette4s, 
                                                                          name =  "Recommendation 
Type")

forestrecoplot <- rec.plot + theme_legend3 + alltheme 
                  
forestrecoplot

#COMBINE IN ONE PLOT

all.recoplot<- ggarrange(birdrecoplot, forestrecoplot,
                              font.label = list(color= "black"),
                              ncol = 2, nrow = 1)
all.recoplot

#################################################### EMBEDDED FIGURE WITH CATEGORY, RECOMMENDATION, COMPARATOR, URBAN SCALE ###################################


embeddedfig<- ggarrange(birdtopic.plot, foresttopic.plot,
                        bird.comparatorplot, forest.comparatorplot,
                        bird.scaleplot, forest.scaleplot,
                        birdrecoplot, forestrecoplot,
                        font.label = list(color= "black"),
                        ncol = 2, nrow = 4)

embeddedfig

-############################# FIGURE 7 ####################################################################
####################### PERCENT PUBLICATIONS ACCORDING TO ONE/MANY INDICATORS ##########################
birdpalette3 <- c("#ae017e","#f768a1","#fbb4b9","#feebe2")

Indicator.ct<- read.csv("Allind.count.csv")
sum(Indicator.ct$number_publications)
#add column with percents
Indicator.ct$percent<- (Indicator.ct$number_publications/347)*100
  
indicator.plot <- ggplot(Indicator.ct, aes(x= reorder(number_topics, -percent), y= percent)) +
                  geom_bar(stat= 'identity', fill= birdpalette3) + theme(axis.text.x = element_text(size= 10))+
                  theme_hc() + labs(x = "", y=" Percent of Studies") + coord_flip()
bird.indicatorplot<- indicator.plot + alltheme

#FOREST#
forestpalette5 <- c("#006837","#31a354", "#78c679", "#c2e699","#ffffcc")

Indicator.ct<- read.csv("FAllind.count.csv")
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




