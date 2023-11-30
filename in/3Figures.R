#packages needed
Packages <- c("tidyverse", "ggplot2", "maps", "ggthemes", "cartography", "sf", "ggpubr", "plotly", "data.table", "cowplot", "janitor",
              "rphylopic", "png", "viridis", "ggrepel" , "cowplot")
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
  axis.title.x = element_text(colour = "black", face = "bold", size= '40', vjust= 0),
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

#LEGEND 4 THEME
theme_legend4<- theme(
  legend.title = element_text(colour = "black", size = 12, face= "bold"),
  legend.position = c(0.90,0.55),
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
ggsave(filename ="EV_Fig1.png", width = 16, height = 13, dpi=300)  

########################## FIGURE _ ###################################################
################### Study Country Heat Map ##################################
journal.df.av <- read.csv("journal.df.av.csv")
journal.df.av$percent <- (journal.df.av$journal.count/277*100)

#read file
bird<-readPNG("bird.1.png")

av.plot<- journal.df.av %>%
  filter(!is.na(COUNTRY)) %>%
  ggplot() +
  aes(x = reorder(Journal, count), y = percent, fill = COUNTRY) +
  geom_col() + scale_fill_viridis(discrete=TRUE, direction=-1, end = 1, begin = 0.1, name="Country") +
  labs(x= "", y= "") +
  coord_flip() + alltheme + theme_legend4 + 
  theme(axis.text = element_text(size = 18, colour = "black")) +
  add_phylopic(bird, alpha = 1, x = 2, y = 35.4, ysize =0.8) + 
  scale_y_continuous(breaks = c(0, 5, 10, 15), limits = c(0,15))
av.plot

#read file
journal.df <- read.csv("journal.df.csv")
journal.df$percent <- (journal.df$journal.count/169*100)
tree<-readPNG("tree.1.png")

for.plot <-journal.df %>%
  filter(!is.na(COUNTRY)) %>%
  ggplot() +
  aes(x = reorder(Journal, count), y = percent, fill = COUNTRY) +
  geom_col() + scale_fill_viridis(discrete=TRUE, direction=-1, end = 1, begin = 0.1, name="Country") +
  labs(x= "", y= "Percent of Publications") +
  coord_flip() + alltheme + theme_legend4 + theme(axis.text = element_text(size = 18, colour = "black")) +
  add_phylopic(tree, alpha = 1, x = 1.8, y = 20.4, ysize =0.8) + 
  scale_y_continuous(breaks = c(0, 5, 10, 15), limits = c(0,15))

for.plot

#combined plot
av_for_maps<- plot_grid(av.plot, for.plot, align = "v", nrow = 2, ncol= 1)
av_for_maps

ggsave(filename ="UFCBFig2.png", width = 350, units="mm", height = 260 , device='tiff', dpi=300)  

########################## FIGURE 3 #########################################################
############### # Publication according to Top 10 Journals with Country ###########################################
world<- map_data("world")

cutoffs <- data.frame(id = 1:1, 
                      lat_1 = c(23.5, -23.5), 
                      lon_1 = c(-170.5, -170.5), 
                      lat_2 = c(23.5, -23.5),
                      lon_2 = c(170.5, 170.5))

data <- read.csv("data.csv")
# Reorder data to show biggest cities on top
data <- data %>%
  arrange(-count)
mybreaks <- c(1,5,15,35,95) 

# Build the map

av.map<- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=longitude, y=latitude, size=count, color=count), shape=20, stroke=FALSE) + 
  geom_text_repel(data=data %>% arrange(count) %>% tail(10), aes(x=longitude, y=latitude, label=COUNTRY),     size=3, box.padding = 0.7, family = "serif", segment.color="grey50", max.overlaps = Inf) +
  scale_size_continuous(range=c(2.5,16), breaks=mybreaks,name="Number of Studies") +
  scale_color_viridis(option="viridis", end = 1, begin = 0.1, breaks=mybreaks, direction=-1, discrete =        FALSE, name="Number of Studies") +
  theme_void() +  
  coord_quickmap() +
  ggtitle("") + 
  theme(legend.position ="bottom",legend.direction = "horizontal",
        text = element_text(color = "#22211d", size=10,family = "serif"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.background = element_rect(fill = "white", color = NA), 
        legend.background = element_rect(fill = "white", color = NA)) +
  guides(color = guide_legend(nrow=1, byrow=TRUE)) +
  geom_segment(data = cutoffs,aes(x = lon_1, y = lat_1, xend = lon_2, yend = lat_2), color = "grey", alpha           = 0.5, linewidth = 0.6, linetype=2 , lineend = "round") 

av.map

# Reorder data to show biggest cities on top

data1<- read.csv("data1.csv")
  
data1 <- data1 %>%
  arrange(count)

mybreaks <- c(1,5,15,25,60)

# Build the map
for.map<-  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data1, aes(x=longitude, y=latitude, size=count, color=count), shape=20, stroke=FALSE) + 
  geom_text_repel(data=data1 %>% arrange(count) %>% tail(10), aes(x=longitude, y=latitude, label=COUNTRY),     size=3, box.padding = 0.7, family = "serif", segment.color="grey50", max.overlaps = Inf) +
  scale_size_continuous(range=c(2.5,16), breaks=mybreaks,name="Number of Studies") +
  scale_color_viridis(option="turbo", end = 0.8, begin = 0.3, breaks=mybreaks, direction=-1, discrete =        FALSE, name="Number of Studies") +
  theme_void() +  
  coord_quickmap() +
  ggtitle("") + 
  theme(legend.position ="bottom",legend.direction = "horizontal",
        text = element_text(color = "#22211d", size=10,family = "serif"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.background = element_rect(fill = "white", color = NA), 
        legend.background = element_rect(fill = "white", color = NA)) +
  guides(color = guide_legend(nrow=1, byrow=TRUE)) +
  geom_segment(data = cutoffs,aes(x = lon_1, y = lat_1, xend = lon_2, yend = lat_2), color = "grey",          alpha = 0.5, linewidth = 0.6, linetype=2 , lineend = "round") 

for.map

av_for_maps<- plot_grid(av.map, for.map)
av_for_maps

ggsave(filename ="UFCBFig3.png", width = 230, units="mm", height = 90 , dpi=300)  

################################## FIGURE __ #########################################################
####################### NUMBER OF PUBLICATIONS ACCORDING TO STUDY DURATION ###############################################

#AVIAN
Duration<- read.csv("Duration.count2.csv")
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

Duration2<- read.csv("Studydurationall.csv")
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

#legend
#LEGEND 5 THEME
theme_legend5<- theme(
  legend.key.size = unit(2, 'cm'),
  legend.title = element_text(colour = "black", size = 15, face= "bold"),
  legend.position = c(0.88,0.85), ## (h,v)
  legend.text = element_text(size=15),
  legend.box.background =  element_rect(colour = "black"), legend.box.margin = margin(5,5,5,5))

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

ggsave(filename ="UFCBFig4.png", width = 16, height = 13, device='tiff', dpi=300)  

#################################### FIGURE __ ######################################################
############################ TOPICS ONLY ###########################################################

bigtheme <- theme(
  panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.background = element_rect(fill = "white", color = NA),
  plot.background = element_rect(fill = "white", color = NA),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
  text = element_text(family = "serif"),
  axis.title.y = element_text(colour = "black",face = "bold", size = '200', vjust = +1),
  axis.title.x = element_text(colour = "black", face = "bold", size= '200', vjust= 0),
  axis.ticks = element_line(size = 0.7),
  axis.text = element_text(size = 170, colour = "black")
)

#LEGEND 3 THEME
theme_legend3<- theme(
                      legend.title = element_text(colour = "black", size = 120, face= "bold"),
                      legend.text = element_text(colour = "black", size = 110),
                      legend.position = c("right"),
                      legend.key.size = unit(3.5, "cm"),
                      legend.box.background =  element_rect(colour = "black", size = 3), 
                      legend.box.margin = margin(5,5,5,5))

#AVIAN
birdtopics<- read.csv("bird.component.csv")
#remove row 8 NA since the metric didn't fit into our topic categories
birdtopics<- birdtopics[-8,]

#create new column with percent of studies
#sum(birdtopics$total)
#birdtopics$percent <- (birdtopics$total/348)*100

birdtopic.plot <- ggplot(birdtopics, aes(x= reorder(Component, -percent) , y= percent)) +
                  geom_bar(stat= 'identity', position = 'dodge', fill= birdcol, width = 0.8)  + 
                  bigtheme + 
                  labs(x= "", y= "") + 
                  scale_y_continuous(limits = c(0,72), breaks = c(10,20, 30,40,50,60,70)) +
                  coord_flip()

birdtopic.plot
#FOREST 
foresttopics<- read.csv("forest.component.csv")

foresttopic.plot <- ggplot(foresttopics, aes(x= reorder(Component, -percent) , y= percent)) +
                    geom_bar(stat= 'identity', position = 'dodge', fill= forestcol, width = 0.8) +
                    bigtheme + 
                    labs(x= "", y= "") + 
                    scale_y_continuous(limits = c(0,70), breaks = c(10, 20, 30 ,40 ,50, 60, 70)) +
                    coord_flip()

foresttopic.plot

componentall<- ggarrange(birdtopic.plot, foresttopic.plot,
                        ncol = 1, nrow = 2,
                        hjust = -1)

componentall

###################################### FIGURE __ ######################################################################
######################### BIRD SUCCESS COMPONENT BY COMPARATOR #################################################

Compmeta<- read.csv("Comp.meta.csv")

#create column for percentage for plotting
birdpalette2<- c("#481567FF","#404788FF")
#factor the duration
Compmeta$value <- factor(Compmeta$value, levels = c("Biodiversity", "Survival", "Demographics/Patterns", "Breeding", "Behaviour", "Resources", "Foraging"))

comparator.plot<- ggplot(Compmeta, aes(fill = comp, x= value, y= comparator)) +
                  geom_bar(stat= 'identity') +
                  bigtheme + 
                  labs(x= "", y= "") + 
                  scale_y_continuous(limits = c(0,100), breaks = c(0, 20, 40, 60, 80, 100)) +
                  scale_fill_manual(labels= c("No", "Yes"), values = birdpalette2) + 
                  coord_flip() + 
                  theme_legend3
  
bird.comparatorplot<- comparator.plot + labs(fill= "Comparator 
used?")
bird.comparatorplot

#PLOTTING

Forestcompmeta<- read.csv("FComp.count.csv")

forestpalette2<- c("#B8DE29FF", "#55C667FF")

Forestcompmeta$value <- factor(Forestcompmeta$value, levels = c("Composition", "Land use type", "Canopy cover", "Individual tree management", 
                                                    "Forested area", "Native species", "Exotic/invasive species", "Connectivity",
                                                     "Diversity metric", "Fragmentation"))

comparator.plot<- ggplot(Forestcompmeta, aes(fill = comp, x= value, y= comparator)) +
                  geom_bar(stat= 'identity') +
                  bigtheme + 
                  labs(x= "", y= "") + 
                  scale_y_continuous(limits = c(0,100), breaks = c(0, 20, 40, 60, 80, 100)) +
                  scale_fill_manual(values= forestpalette2, labels= c("No", "Yes")) + 
                  coord_flip() +
                  theme_legend3

forest.comparatorplot<- comparator.plot + labs(fill= "Comparator
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
              geom_bar(stat= 'identity') + 
              labs(x= "", y= "") + 
              coord_flip() + 
              scale_y_continuous(limits= c(0,100)) + 
              scale_fill_manual(values = birdpalette4) +
              bigtheme +
              theme_legend3
              
bird.scaleplot<- scale.plot + labs(fill= "Urban Scale") 
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
Urban <- Urban %>% pivot_longer(., cols = c(`Canopy cover`,`Composition`, `Connectivity`, `Diversity metric`, `Exotic/invasive species`, 
                                          `Forested area`, `Fragmentation`, `Individual tree management`, `Land use type`, `Native species`))

colnames(Urban)
Urban <- as.data.frame(Urban)
#sort
Urban <- Urban[order(Urban[,2]), ]
Totals<- c(33,33,33,33,120,120,120,120,3,3,3,3,4,4,4,4,10,10,10,10,
           29,29,29,29,2,2,2,2,27,27,27,27,101,101,101,101,16,16,16,16)

Urban <-cbind(Urban, Totals)

#now add column of percentages 
Urban$percent <- (Urban$value/Urban$Totals)*100

## PLOT #############################################################################################

Urban$name <- factor(Urban$name, levels = c("Composition", "Land use type", "Canopy cover", "Individual tree management", 
                                                                "Forested area", "Native species", "Exotic/invasive species", "Connectivity",
                                                                "Diversity metric", "Fragmentation"))

forestpalette4 <- c("#20A387FF", "#55C667FF", "#B8DE29FF", "#FDE725FF")

scale.plot <- ggplot(Urban, aes(fill= Urb.scale, x= name, y= percent)) +
              geom_bar(stat= 'identity') + 
              labs(x= "", y= "") + 
              coord_flip() + 
              scale_y_continuous(limits= c(0,100)) + 
              scale_fill_manual(values= forestpalette4) +
              bigtheme +
              theme_legend3

forest.scaleplot<- scale.plot + labs(fill= "Urban Scale") 

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
totals<- c(31,31,31,31,31,224,224,224,224,224,224,224,224,53,53,53,53,53,53,
           50,50,50,50,50,50,50,1,7,7,7,29,29,29,29,29)
#bind
recs<- cbind(recs, totals)

recs$percent<- (recs$n/recs$totals)*100
recs$Rec.1[recs$Rec.1 == 'No Recommendations'] <- 'None'

##### PLOT

#factor columns to correct order
recs$value <- factor(recs$value, levels = c("Biodiversity", "Survival", "Demographics/Patterns", "Breeding", "Behaviour", "Resources", "Foraging"))
recs$Rec.1 <- factor(recs$Rec.1, levels= c("Restoration", "Management", "Conservation", "None"))

birdpalette4s <- c("#2D708EFF", "#481567FF", "#404788FF", "#787276")

rec.plot <- ggplot(recs, aes(fill= Rec.1, x= value , y= percent)) +
            geom_bar(stat= 'identity') + 
            labs(x= "", y= "Percent of Studies") +
            coord_flip() + 
            scale_y_continuous(limits= c(0,100)) + 
            scale_fill_manual(values = birdpalette4s, name =  "Recommendation 
Type") + 
            theme_legend3 + 
            bigtheme

birdrecoplot <- rec.plot

birdrecoplot

#theme(axis.text.x = element_text(colour= "black", size= 150), axis.text.y = element_text(size = 150, colour= "black", angle = 0)) 

##### FOREST
recs<- read.csv("FAllrec.count.csv")
#remove leading and trailing zeros
recs<- recs %>% 
  mutate(across(where(is.character), str_trim))

#sort based on bird category
recs <- recs[order(recs[,"value"]), ]

#manually add a column of totals
totals<- c(36,36,36,36,36,139,139,139,139,139,139,139,139,6,6,6,6,5,5,5,5,11,
           11,11,11,11,34,34,34,34,34,34,2,2,29,29,29,29,29,120,120,120,120,
           120,120,120,120,20,20,20,20,20,20)
#bind
recs<- cbind(recs, totals)

recs$percent<- (recs$n/recs$totals)*100
recs$Rec1[recs$Rec1 == 'No Recommendations'] <- 'None'

##### PLOT
recs$value <- factor(recs$value, levels = c("Composition", "Land use type", "Canopy cover", "Individual tree management", 
                                            "Forested area", "Native species", "Exotic/invasive species", "Connectivity",
                                            "Diversity metric", "Fragmentation"))
recs$Rec1 <- factor(recs$Rec1, levels= c("Restoration", "Management", "Conservation", "None"))


forestpalette4s <- c("#B8DE29FF","#29AF7FFF","#FDE725FF", "#787276")

rec.plot <- ggplot(recs, aes(fill= Rec1, x= value , y= percent)) +
            geom_bar(stat= 'identity') + labs(x= "", y= "Percent of Studies") +
            coord_flip() + 
            bigtheme +
            scale_y_continuous(limits= c(0,100)) + 
            theme_legend3 +
            scale_fill_manual(values = forestpalette4s, 
                                                                          name =  "Recommendation 
Type")

forestrecoplot <- rec.plot
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

ggsave(filename ="UFCBFig5.png", width = 120, height = 95, dpi=100, limitsize = FALSE)  


############################# FIGURE 7 ####################################################################
####################### PERCENT PUBLICATIONS ACCORDING TO ONE/MANY INDICATORS ##########################
birdpalette4 <- c("#481567FF", "#404788FF", "#33638DFF", "#238A8Dff")

BIndicator.ct<- read.csv("Allind.count.csv")
sum(BIndicator.ct$number_publications)
#add column with percents
BIndicator.ct$percent<- (BIndicator.ct$number_publications/347)*100
  
indicator.plot <- ggplot(BIndicator.ct, aes(x= reorder(number_topics, -percent), y= percent)) +
                  geom_bar(stat= 'identity', fill= birdpalette4) + theme(axis.text.x = element_text(size= 10))+
                  theme_hc() + labs(x = "", y=" Percent of Studies") + coord_flip()
bird.indicatorplot<- indicator.plot + alltheme

#FOREST#
forestpalette5 <- c("#1F968BFF", "#29AF7FFF", "#55C667FF", "#B8DE29FF", "#FDE725FF")

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
  scale_fill_manual(values=c(birdcol, forestcol)) + alltheme + theme_legend +
  labs(fill = "Topic") + scale_x_discrete(labels=c("One Indicator" = "One", "Two Indicators" = "Two",
                                                   "Three Indicators" = "Three", "Four Indicators" = "Four", "Five Indicators" = "Five"))


ind.plotall

ggsave(filename ="UFCBFig_.png", width = 16, height = 13, device='tiff', dpi=300)  

