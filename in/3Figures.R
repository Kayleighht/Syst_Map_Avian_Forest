#packages needed
Packages <- c("tidyverse", "ggplot2", "maps", "ggthemes", "cartography", "sf", "ggpubr", "plotly", "data.table", "cowplot", "janitor")
lapply(Packages, library,character.only= TRUE)


getwd()
setwd("C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out")

############################# THEME CREATION ####################################

alltheme <- theme(
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


forestcol<- ("#31a354")
birdcol<- ("#c51b8a")

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
  geom_line(size= 0.7, colour= "#c51b8a") + lab  +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 7.5, angle = 90, 
                                   colour = "black")) + 
  scale_x_continuous(breaks = c(1980, 1985, 1990, 1995,2000, 2005, 2010, 2015, 2020), limits = c(1979, 2021)) +
  scale_y_continuous(breaks = c(5,10,15,20,25), 
                     limits = c(0,25)) + geom_area(fill = "#c51b8a")

publication.years<- byyear_av + alltheme +
                    geom_line(data= fyear.count, aes(x=Year, y = Year_count), size= 0.7, colour = "#31a354") +
                    geom_area(data= fyear.count, aes(x=Year, y = Year_count), fill= "#31a354")

publication.years 

########################## FIGURE _ ###################################################
################### Study Country Heat Map ##################################

cutoffs <- data.frame(id = 1:1, 
                      lat_1 = c(23.5, -23.5), 
                      lon_1 = c(-170.5, -170.5), 
                      lat_2 = c(23.5, -23.5),
                      lon_2 = c(170.5, 170.5))

### AVIAN ###
av.meta <-read.csv("Heatmap.count.csv")

#subsetting columns we need first
region.df<- av.meta[,c("COUNTRY","country.count")]

#remove duplicates
region.df <- region.df[!duplicated(region.df), ]

#remove columns with multiple (don't specificy region)
#filter rows that contain the string 'Multiple' in the country column
region.df <- region.df %>% filter(!grepl('Multiple', COUNTRY))

#load shape file with global map including all countries
mapdata<- st_read("World_Countries_Generalized.shp")
mapdata$COUNTRY[mapdata$COUNTRY=="United States"]<-"USA"

#merge data with studies countries location from systematic map
mapdata2<- merge(mapdata, region.df, by="COUNTRY")

#upload layer of simple coordinates of global map for bubblemaps 
coordmap<- read.csv("countries.csv")
coordmap$COUNTRY[coordmap$COUNTRY=="United States"]<-"USA"

#merge with country count data
coordmap<- merge(mapdata2, coordmap, by="COUNTRY")

#PLOTTING     
#####WITH MID range 
heatmap<- ggplot(data= mapdata) + geom_sf(color= "white", fill= "lightgrey") +
  geom_sf(data= mapdata2, aes(fill= country.count), color= "white") +
  xlab("Longitude")+ ylab("Latitude") +
  ggtitle("Studies by Country Avian") +
  theme(
    legend.position = "bottom",
    text = element_text(color = "black"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 12, hjust=0.1, color = "black", 
                              margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),) + 
  guides(
    fill = guide_legend("# of Publications")) + geom_segment(data = cutoffs, 
                                                             aes(x = lon_1, y = lat_1, xend = lon_2, yend = lat_2), 
                                                             color = "black", linewidth = 1.3, lineend = "round") +
  scale_colour_continuous(palette = "BuGN")

heatmap

### FOREST ###
forest.meta <-read.csv("FHeatmap.count.csv")

#subsetting columns we need first
region.df<- forest.meta[,c("COUNTRY","country.count")]

#remove duplicates
region.df <- region.df[!duplicated(region.df), ]

#remove columns with multiple (don't specificy region)
#filter rows that contain the string 'Multiple' in the country column
region.df <- region.df %>% filter(!grepl('Multiple', COUNTRY))

#load shape file with global map including all countries
mapdata<- st_read("World_Countries_Generalized.shp")
mapdata$COUNTRY[mapdata$COUNTRY=="United States"]<-"USA"

#merge data with studies countries location from systematic map
mapdata2<- merge(mapdata, region.df, by="COUNTRY")

#upload layer of simple coordinates of global map for bubblemaps 
coordmap<- read.csv("countries.csv")
coordmap$COUNTRY[coordmap$COUNTRY=="United States"]<-"USA"

#merge with country count data
coordmap<- merge(mapdata2, coordmap, by="COUNTRY")

#PLOTTING     
#####WITH MID range 

heatmap<- ggplot(data= mapdata) + geom_sf(color= "white", fill= "lightgrey") +
  geom_sf(data= mapdata2, aes(fill= country.count), color= "white") +
  xlab("Longitude")+ ylab("Latitude") +
  ggtitle("Studies by Country Forest") +
  
  theme(
    legend.position = "bottom",
    text = element_text(color = "black"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 12, hjust=0.1, color = "black", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),) +
  guides(
    fill = guide_legend("# of Publications")) + geom_segment(data = cutoffs, 
                                                             aes(x = lon_1, y = lat_1, xend = lon_2, yend = lat_2), 
                                                             color = "black", linewidth = 1.3, lineend = "round") +
  scale_colour_continuous(palette = "BuGN")


heatmap

########################## FIGURE 3 #########################################################
############### # Publication according to Top 10 Journals with Country ###########################################

#AVIAN#
country.count<- read.csv("Country.count.csv")

#subset columns needed
journal.df <- country.count[,c("COUNTRY", "Journal", "journal.count")]
#sort column descending order
journal.df <- arrange(journal.df, -journal.count)
#remove duplicates
journal.df <- journal.df[!duplicated(journal.df), ]

#select only top ten
journal.df <- journal.df[1:13,]

journal.plot <- ggplot(journal.df, aes(x= reorder(Journal, -journal.count), y= journal.count, fill = COUNTRY)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(size= 10)) +
  labs(x= "", y= "Number of Publications") + scale_fill_brewer(palette = "Set2") +
  theme_hc() + theme(axis.text.x = element_text(colour = "black", size= 10.5),
                     axis.text.y = element_text(colour = "black", size = 12, face= "bold"))


journal.plotbird<- journal.plot + alltheme + labs(fill = "Country") + theme_legend + coord_flip()
  
#FOREST#

country.count<- read.csv("FCountry.count.csv")

#subset columns needed
journal.df <- country.count[,c("COUNTRY", "Journal", "journal.count")]
#sort column descending order
journal.df <- arrange(journal.df, -journal.count)
#remove duplicates
journal.df <- journal.df[!duplicated(journal.df), ]

#select only top ten
journal.df <- journal.df[1:14,]

journal.plot <- ggplot(journal.df, aes(x= reorder(Journal, -journal.count), y= journal.count, fill = COUNTRY)) + 
  geom_bar(stat = 'identity')  + labs(x= "", y= "Number of Publications") + scale_fill_brewer(palette = "Set2") +
  theme_hc() + theme(axis.text.x = element_text(colour = "black", size= 10.5), 
                     axis.text.y = element_text(colour= "black", size = 12, face = "bold")) + coord_flip() +
  scale_y_continuous(breaks = c(0,5,10,15), lim = c(0,10))

theme_legend<- theme(
  legend.title = element_text(colour = "black", size = 12, face= "bold"),
  legend.position = c(0.7, 0.8),
  legend.box.background =  element_rect(colour = "black"), legend.box.margin = margin(5,5,5,5))

journal.plotforest<- journal.plot + alltheme + labs(fill = "Country") + theme_legend 
journal.plotforest

journal.allplot<- ggarrange(journal.plotbird, journal.plotforest,
                            labels = c("A", "B"),
                            ncol = 2, nrow = 1,
                            hjust = -2.3)
journal.allplot


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
                 geom_bar(stat= 'identity', fill = "#c51b8a", width = 0.6) + 
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
  geom_bar(stat= 'identity', position = 'dodge', fill= "#31a354", width = 0.6) + theme_hc() + labs(x= "Time-scale considered", y= "Percent of Studies") +
  
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


#################################### FIGURE __ ######################################################
############################ TOPICS ONLY ###########################################################

#AVIAN
birdtopics<- read.csv("bird.component.csv")

#create new column with percent of studies
sum(birdtopics$total)
birdtopics$percent <- (birdtopics$total/348)*100

#PLOT
birdpalette6 <- c("#fa9fb5","#7a0177" ,"#c51b8a", "#f768a1", "#feebe2", "#fcc5c0")
  
  
 
birdtopic.plot <- ggplot(birdtopics, aes(x= reorder(birdomain, -percent) , y= percent)) +
                  geom_bar(stat= 'identity', position = 'dodge', fill= birdpalette6, width = 0.8) + 
                  
                  theme(axis.text.x = element_text(size= 12), 
                  axis.text.y = (element_text(size = 13, face = "bold", angle = 15))) + theme_hc() + 
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

#PLOT
forestpalette8 <- c("#005a32","#f7fcb9","#ffffe5","#d9f0a3","#78c679","#41ab5d", "#238443","#addd8e")
  
  
foresttopic.plot <- ggplot(foresttopics, aes(x= reorder(forestcomp, -percent) , y= percent)) +
  geom_bar(stat= 'identity', position = 'dodge', fill= forestpalette8, width = 0.8) + 
  
  theme(axis.text.x = element_text(size= 12), 
        axis.text.y = (element_text(size = 13, face = "bold", angle = 15))) + theme_hc() + 
  labs(x= "", y= "Percent of Studies") + 
  scale_y_continuous(limits = c(0,55), breaks = c(10, 20, 30, 40, 50)) +
  coord_flip()

foresttopic.plot<- foresttopic.plot + alltheme
foresttopic.plot

#COMBINEPLOT
componentall<- ggarrange(birdtopic.plot, foresttopic.plot,
                        labels = c("A", "B"),
                        ncol = 1, nrow = 2,
                        hjust = -1)

componentall

################################### FIGURE 4 ###########################################################
###################### RECOMMENDATION TYPE (USED + TYPE) ############################################

#read in files
rec.type<- read.csv("Allrec.count.csv")
no.rec<- read.csv("Norec.count.csv")

rec.meta<- rbind(rec.type, no.rec)
sum(rec.meta$n)
rec.meta$totalpercent <- ((rec.meta$n)/366)*100

#### PLOTTING ##############

rec.plot<- ggplot(rec.meta, aes(fill = Rec.1 , x= reorder(value,-totalpercent), 
                                            y= totalpercent)) +
                  geom_bar(stat= 'identity') + theme(axis.text.x = element_text(size= 12), 
                                                     axis.text.y = (element_text(size = 13, face = "bold",
                                                                    angle = 15))) +
                  theme_hc() + labs(x= "", y= "Percent of Studies") + 
                  scale_y_continuous(limits = c(0,40), breaks = c(5, 10, 15, 20, 25, 30, 35, 40)) +
                  scale_fill_brewer(palette = "Set2")


bird.recplot<- rec.plot + alltheme + theme_legend2 + labs(fill = "Recommendation Type") + coord_flip()
bird.recplot 

###################### RECOMMENDATION TYPE (USED + TYPE) ############################################

#read in files
rec.type<- read.csv("FAllrec.count.csv")
no.rec<- read.csv("FNorec.count.csv")

rec.meta<- rbind(rec.type, no.rec)

#remove N/As
rec.meta<- rec.meta[!grepl("N/A", rec.meta$value),]
rec.meta<- rec.meta[!grepl("N/A", rec.meta$Rec1),]

#create new column with TOTAL percent
sum(rec.meta$n)
rec.meta$totalpercent<- (rec.meta$n/332)*100

#### PLOTTING ##############

rec.plot<- ggplot(rec.meta, aes(fill = Rec1 , x= reorder(value,-totalpercent), 
                                y= totalpercent)) +
  geom_bar(stat= 'identity') + theme(axis.text.x = element_text(size= 12,), axis.text.y = element_text(size= 13, face = "bold"
                                                                                                       , angle = 15)) +
  theme_hc() + labs(x= "", y= "Percent of Studies") + 
  scale_y_continuous(limits = c(0,40), breaks = c(5, 10, 15, 20, 25, 30, 35, 40)) +
  scale_fill_brewer(palette = "Set2")

theme_legend2<- theme(
  legend.title = element_text(colour = "black", size = 12, face= "bold"),
  legend.position = c(0.70, 0.80),
  legend.box.background =  element_rect(colour = "black"), legend.box.margin = margin(5,5,5,5)
)

forest.recplot<- rec.plot + alltheme + theme_legend2 + labs(fill = "Recommendation Type") + coord_flip()
forest.recplot

#ALL#
all.recplot <- ggarrange(bird.recplot, forest.recplot,
                         labels = c("A", "B"),
                         ncol = 2, nrow = 1,
                         hjust = -1)
all.recplot


###################################### FIGURE 5 ######################################################################
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
birdpalette2<- c("#fa9fb5","#c51b8a")

comparator.plot<- ggplot(Compmeta, aes(fill = comp, x= reorder(value, -comparator), y= comparator)) +
                  geom_bar(stat= 'identity') + theme(axis.text.y = element_text(size= 13, angle = 15), 
                                                     axis.text.x = element_text(size = 13))+
                  theme_hc() + labs(x= "", y= "Percent of Studies") + scale_y_continuous(limits = c(0,100), 
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
forestpalette2<- c("#addd8e", "#31a354")

comparator.plot<- ggplot(Compmeta, aes(fill = comp, x= reorder(value, -comparator), y= comparator)) +
  geom_bar(stat= 'identity') + theme(axis.text.y = element_text(size= 13, angle = 15), 
                                     axis.text.x = element_text(size = 13))+
  theme_hc() + labs(x= "", y= "Percent of Studies") + scale_y_continuous(limits = c(0,110), 
                                                                         breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_fill_manual(values= forestpalette2, labels= c("No", "Yes")) + coord_flip()

forest.comparatorplot<-comparator.plot + alltheme + theme_legend3 + labs(fill= "Comparator
used?")

all.comparatorplot<- ggarrange(bird.comparatorplot, forest.comparatorplot,
                               labels = c("Bird", "Forest"),
                               font.label = list(color= "black"),
                               ncol = 2, nrow = 1)
all.comparatorplot

####################################### FIGURE 6 ######################################################################
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

birdpalette4 <- c("#feebe2", "#fbb4b9", "#f768a1", "#ae017e")

scale.plot <- ggplot(Urban, aes(fill= Urb.scale, x= reorder(name, -percent) , y= percent)) +
              geom_bar(stat= 'identity') + theme_hc() + labs(x= "", y= "Percent of Studies") +
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

forestpalette4 <- c("#ffffcc", "#c2e699", "#78c679", "#238443")

scale.plot <- ggplot(Urban, aes(fill= Urb.scale, x= reorder(name, -percent), y= percent)) +
  geom_bar(stat= 'identity') + theme_hc() + labs(x= "", y= "Percent of Studies") +
  theme(axis.text.x = element_text(colour= "black", size= 12), axis.text.y = element_text(size = 13, colour= "black", angle = 15)) + 
  coord_flip() + scale_y_continuous(limits= c(0,100)) + scale_fill_manual(values= forestpalette4)

forest.scaleplot<- scale.plot + alltheme + labs(fill= "Urban Scale") + theme_legend3
forest.scaleplot

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
birdpalette5 <- c("#feebe2", "#fbb4b9", "#f768a1", "#c51b8a", "#7a0177")

rec.plot <- ggplot(recs, aes(fill= Rec.1, x= reorder(value, -percent) , y= percent)) +
  geom_bar(stat= 'identity') + theme_hc() + labs(x= "", y= "Percent of Studies") +
  theme(axis.text.x = element_text(colour= "black", size= 12), axis.text.y = element_text(size = 13, colour= "black", angle = 15)) + 
  coord_flip() + scale_y_continuous(limits= c(0,100)) + scale_fill_manual(values = birdpalette5)

birdrecoplot <- rec.plot + labs(fill= "Recommendation Type") + theme_legend3 + alltheme
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
forestpalette5 <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")

rec.plot <- ggplot(recs, aes(fill= Rec1, x= reorder(value, -percent) , y= percent)) +
  geom_bar(stat= 'identity') + theme_hc() + labs(x= "", y= "Percent of Studies") +
  theme(axis.text.x = element_text(colour= "black", size= 12), axis.text.y = element_text(size = 13, colour= "black", angle = 15)) + 
  coord_flip() + scale_y_continuous(limits= c(0,100)) + scale_fill_manual(values = forestpalette5)

forestrecoplot <- rec.plot + labs(fill= "Recommendation Type") + theme_legend3 + alltheme
forestrecoplot

all.recoplot<- ggarrange(birdrecoplot, forestrecoplot,
                              labels = c("Bird", "Forest"),
                              font.label = list(color= "black"),
                              ncol = 2, nrow = 1)
all.recoplot


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




