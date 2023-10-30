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
  axis.title.y = element_text(colour = "black",face = "bold", size = '16'),
  axis.title.x = element_text(colour = "black", face = "bold", size= '16'),
  axis.ticks = element_line(size = 0.7),
  axis.text = element_text(size = 12, colour = "black")
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
        axis.text.x = element_text(size = 7.5, angle = 90, 
                                   colour = "black")) + 
  scale_x_continuous(breaks = c(1980, 1985, 1990, 1995,2000, 2005, 2010, 2015, 2020), limits = c(1979, 2021)) +
  scale_y_continuous(breaks = c(5,10,15,20,25), 
                     limits = c(0,25)) + geom_area(fill = birdcol)

publication.years<- byyear_av + alltheme +
                    geom_line(data= fyear.count, aes(x=Year, y = Year_count), size= 0.7, colour = forestcol) +
                    geom_area(data= fyear.count, aes(x=Year, y = Year_count), fill= forestcol) 

publication.years 

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
durationall$duration_binclean <- c("0-1", "1-5", "5-10", "10-100", "0-1", "1-5", "5-10", "10-100")

#order dataframe for plotting
durationsorted <- within(durationall, 
                   duration_binclean <- factor(duration_binclean, 
                                      levels=names(sort(table(duration_binclean), 
                                                        decreasing=TRUE))))

duration.plotall <- ggplot(durationsorted, aes(x= duration_binclean, y=percent, fill=component)) +
                    geom_bar(stat='identity', position='dodge', width= 0.8) +
                    labs(x= "Time-scale considered (years)", y= "Percent of Studies") +
                    
                    scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50), limits = c(0,55)) +
                    scale_fill_manual(values=c(birdcol, forestcol)) + alltheme + theme_legend +
                    labs(fill = "Topic")
duration.plotall

#################################### FIGURE __ ######################################################
############################ TOPICS ONLY ###########################################################

#AVIAN
birdtopics<- read.csv("bird.component.csv")

#create new column with percent of studies
sum(birdtopics$total)
birdtopics$percent <- (birdtopics$total/348)*100

birdtopic.plot <- ggplot(birdtopics, aes(x= reorder(birdomain, -percent) , y= percent)) +
                  geom_bar(stat= 'identity', position = 'dodge', fill= birdcol, width = 0.8) + 
                  
                  theme(axis.text.x = element_text(size= 12), 
                  axis.text.y = (element_text(size = 13, angle = 15))) + alltheme + 
                  labs(x= "", y= "Percent of Studies") + 
                  scale_y_continuous(limits = c(0,55), breaks = c(10, 20, 30, 40, 50)) +
                  coord_flip()

birdtopic.plot<- birdtopic.plot + alltheme
birdtopic.plot

#FOREST 
#AVIAN
foresttopics<- read.csv("forest.component.csv")

#create new column with percent of studies
sum(foresttopics$total)
foresttopics$percent <- (foresttopics$total/267)*100
  
foresttopic.plot <- ggplot(foresttopics, aes(x= reorder(forestcomp, -percent) , y= percent)) +
  geom_bar(stat= 'identity', position = 'dodge', fill= forestcol, width = 0.8) + 
  
  theme(axis.text.x = element_text(size= 12), 
        axis.text.y = (element_text(size = 13, angle = 15)))  + alltheme + 
  labs(x= "", y= "Percent of Studies") + 
  scale_y_continuous(limits = c(0,55), breaks = c(10, 20, 30, 40, 50)) +
  coord_flip()

foresttopic.plot<- foresttopic.plot + alltheme
foresttopic.plot

#COMBINEPLOT
componentall<- ggarrange(birdtopic.plot, foresttopic.plot,
                        ncol = 1, nrow = 2,
                        hjust = -1)

componentall

######################################    ######################################################################
######################### BIRD SUCCESS COMPONENT BY COMPARATOR #################################################

Comp<- read.csv("Comp.count.csv")
#remove n/a's created from empty rows
Comp <-Comp[!grepl('N/A', Comp$Comparator),]
Comp <-Comp[!grepl('N/A', Comp$value),]
Comp <- Comp %>% drop_na(Comparator)
Comp <- Comp %>% drop_na(value)

#sort alphabetically to create percentages
Comp <- Comp[order(Comp[,2]), ]
Comp2<- Comp
sum(Comp$n)

#spread rows so that we can create percent bars that reach 100%
Comp<- Comp %>% spread(Comparator, n)

#replace NA with 0
Comp[is.na(Comp)] <- 0
#create column of totals
Comp$total <- Comp$Y + Comp$N
#create percent yes column
Comp$yespercent <- (Comp$Y/Comp$total)*100
Comp$nopercent <- (Comp$N/Comp$total)*100
sum(Comp$total)

compyes<- subset(Comp, select = c(value, yespercent, Y))
colnames(compyes)[2] = "comparator"
colnames(compyes)[3] = "yes/no"
compno<- subset(Comp, select = c(value, nopercent, N))
colnames(compno)[2] = "comparator"
colnames(compno)[3] = "yes/no"

Compmeta<- rbind(compyes, compno)
#sort
Compmeta <- Compmeta[order(Compmeta[,1]), ]
Comp2[nrow(Comp2) + 1,] <- c("N", "Foraging", 0)
#resort
Comp2 <- Comp2[order(Comp2[,2], Comp2[,3]), ]
Compmeta$comp <- Comp2$Comparator

#swap Y/N because not sorted properly
Compmeta$comp[Compmeta$comp=="N"]<-"Yes"
Compmeta$comp[Compmeta$comp=="Y"]<-"No"

#create column for percentage for plotting
birdpalette2<- c("#481567FF","#404788FF")

comparator.plot<- ggplot(Compmeta, aes(fill = comp, x= value, y= comparator)) +
                  geom_bar(stat= 'identity') + theme(axis.text.y = element_text(size= 13, angle = 15), 
                                                     axis.text.x = element_text(size = 13))+
                  alltheme + labs(x= "", y= "Percent of Studies") + scale_y_continuous(limits = c(0,105), 
                                                                   breaks = c(0, 20, 40, 60, 80, 100)) +
                  scale_fill_manual(labels= c("No", "Yes"), values = birdpalette2) + coord_flip() 
  
bird.comparatorplot<- comparator.plot + alltheme + theme_legend3 + labs(fill= "Comparator 
used?")
bird.comparatorplot

#FOREST#
Comp<- read.csv("FComp.count.csv")
#remove n/a's created from empty rows
Comp <-Comp[!grepl('N/A', Comp$value),]
sum(Comp$n)

#sort alphabetically to create percentages
Comp <- Comp[order(Comp[,2]), ]
Comp2<- Comp

#spread rows so that we can create percent bars that reach 100%
Comp<- Comp %>% spread(Comparator, n)

#replace NA with 0
Comp[is.na(Comp)] <- 0
#create column of totals
Comp$total <- Comp$Y + Comp$N
#create percent yes column
Comp$yespercent <- (Comp$Y/Comp$total)*100
Comp$nopercent <- (Comp$N/Comp$total)*100
sum(Comp$total)

compyes<- subset(Comp, select = c(value, yespercent, Y))
colnames(compyes)[2] = "comparator"
colnames(compyes)[3] = "yes/no"
compno<- subset(Comp, select = c(value, nopercent, N))
colnames(compno)[2] = "comparator"
colnames(compno)[3] = "yes/no"

Compmeta<- rbind(compyes, compno)
#sort
Compmeta <- Compmeta[order(Compmeta[,1]), ]
Comp2[nrow(Comp2) + 1,] <- c("Y", "Fragmentation", 0)
Comp2[nrow(Comp2) + 1,] <- c("Y", "Connectivity", 0)
#resort
Comp2 <- Comp2[order(Comp2[,2], Comp2[,3]), ]
Compmeta$comp <- Comp2$Comparator

#swap Y/N because not sorted properly
Compmeta$comp[Compmeta$comp=="N"]<-"Yes"
Compmeta$comp[Compmeta$comp=="Y"]<-"No"

#PLOTTING
forestpalette2<- c("#B8DE29FF", "#55C667FF")

comparator.plot<- ggplot(Compmeta, aes(fill = comp, x= value, y= comparator)) +
  geom_bar(stat= 'identity') + theme(axis.text.y = element_text(size= 13, angle = 15), 
                                     axis.text.x = element_text(size = 13))+
  alltheme + labs(x= "", y= "Percent of Studies") + scale_y_continuous(limits = c(0,101), 
                                                                         breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_fill_manual(values= forestpalette2, labels= c("No", "Yes")) + coord_flip()

forest.comparatorplot<-comparator.plot + alltheme + theme_legend3 + labs(fill= "Comparator
used?")

all.comparatorplot<- ggarrange(bird.comparatorplot, forest.comparatorplot,
                               font.label = list(color= "black"),
                               ncol = 2, nrow = 1)
all.comparatorplot

#######################################      ######################################################################
############################ BIRD SUCCESS COMPONENT BY URBAN SCALE ###########################################

Urban <- read.csv("Urb.count.csv")
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

birdpalette4 <- c("#481567FF", "#404788FF", "#33638DFF", "#238A8Dff")

scale.plot <- ggplot(Urban, aes(fill= Urb.scale, x= name , y= percent)) +
              geom_bar(stat= 'identity') + alltheme + labs(x= "", y= "Percent of Studies") +
              theme(axis.text.x = element_text(colour= "black", size= 12), axis.text.y = element_text(size = 13, colour= "black", angle = 15)) + 
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
Urban <- Urban[order(Urban[,2]), ]
#get total for totals
sum(Urban$n)

#spread rows so that we can create percent bars that reach 100%
Urban<- Urban %>% spread(value, n)

#replace NA with 0
Urban[is.na(Urban)] <- 0

#pivot table
Urban <- Urban %>% pivot_longer(., cols = c(`Composition`, `Connectivity`, `Diversity metric`, `Exotic/invasive species`, 
                                            `Forested area`, Fragmentation, `Individual tree management`, `Land use type`, `Native species`))
Urban <- as.data.frame(Urban)
#sort
Urban <- Urban[order(Urban[,2]), ]
Totals<- c(119,119,119,119,3,3,3,3,4,4,4,4,10,10,10,10,29,29,29,29,2,2,2,2,26,26,26,26,101,101,101,101,16,16,16,16)

Urban <-cbind(Urban, Totals)

#now add column of percentages 
Urban$percent <- (Urban$value/Urban$Totals)*100

## PLOT #############################################################################################

forestpalette4 <- c("#20A387FF", "#55C667FF", "#B8DE29FF", "#FDE725FF")


scale.plot <- ggplot(Urban, aes(fill= Urb.scale, x= name, y= percent)) +
  geom_bar(stat= 'identity') + alltheme + labs(x= "", y= "Percent of Studies") +
  theme(axis.text.x = element_text(colour= "black", size= 12), axis.text.y = element_text(size = 13, colour= "black", angle = 15)) + 
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
birdpalette4s <- c("#481567FF", "#404788FF", "#A9A9A9" , "#33638DFF")

rec.plot <- ggplot(recs, aes(fill= Rec.1, x= value , y= percent)) +
  geom_bar(stat= 'identity') + alltheme + labs(x= "", y= "Percent of Studies") +
  theme(axis.text.x = element_text(colour= "black", size= 12), axis.text.y = element_text(size = 13, colour= "black", angle = 15)) + 
  coord_flip() + scale_y_continuous(limits= c(0,100)) + scale_fill_manual(values = birdpalette4s, name =  "Recommendation 
Type", 
                                                                                                  labels = c("Conservation", "Management", 
                                                                                                            "None", "Restoration")) + theme_legend3

birdrecoplot <- rec.plot
birdrecoplot

##### FOREST
recs<- read.csv("FAllrec.count.csv")

#manually add a column of totals
totals<- c(138,138,138,138,138,138,138,138,6,6,6,6,5,5,5,5,11,11,11,11,11,
           34,34,34,34,34,34,2,2,28,28,28,28,28,120,120,120,120,120,120,120,120,
           20,20,20,20,20,20)
#bind
recs<- cbind(recs, totals)

recs$percent<- (recs$n/recs$totals)*100

##### PLOT
forestpalette4s <- c("#20A387FF", "#3CBB75FF", "#A9A9A9", "#B8DE29FF")

rec.plot <- ggplot(recs, aes(fill= Rec1, x= value , y= percent)) +
  geom_bar(stat= 'identity') + alltheme + labs(x= "", y= "Percent of Studies") +
  theme(axis.text.x = element_text(colour= "black", size= 12), axis.text.y = element_text(size = 13, colour= "black", angle = 15)) + 
  coord_flip() + scale_y_continuous(limits= c(0,100)) + scale_fill_manual(values = forestpalette4s, 
                                                                          name =  "Recommendation 
Type", 
                                                                          labels = c("Conservation", "Management", 
                                                                                     "None", "Restoration"))

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

############################# FIGURE 7 ####################################################################
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




