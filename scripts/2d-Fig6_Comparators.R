#packagesneeded
source('scripts/0-packages.R')

# themes 
source('scripts/0-themes.R')
 # FOREST TO FOREST

geom_col(color= "black", size= 0.30)

# Figure 6 ----------------------------------------------------------------
# topics only
bird<-readPNG("graphics/bird.1.png", native = T)

#AVIAN
birdforesttopics<- read.csv("out/birdforestall.csv")

#PLOT

birdtopic.plot <- ggplot(birdforesttopics, aes(x= reorder(forest, -percent) , y= percent)) +
  geom_bar(fill =  birdcol , colour= "black", size = 2.0, stat= 'identity', position = 'dodge', width = 0.8) + 
  scale_fill_manual(values = c(birdcol)) +
  bigtheme + 
  labs(x= "", y= "") + 
  scale_y_continuous(limits = c(0,72), breaks = c(10,20, 30,40,50,60,70)) +
  coord_flip() +
  annotation_custom(grid::rasterGrob(bird,
                                     width=ggplot2::unit(1,"npc"),
                                     height=ggplot2::unit(1,"npc")),
                    ymin = 1, ymax = 1, xmin = 2, xmax = 2) 

birdtopic.plot

#FOREST 
tree<-readPNG("graphics/tree.1.png", native = T)

foresttopics<- read.csv("out/forest.component.csv")

foresttopic.plot <- ggplot(foresttopics, aes(x= reorder(Component, -percent) , y= percent)) +
  geom_bar(fill = forestcol, colour = "black", size = 2.0, stat= 'identity') + #position = 'dodge') +
  bigtheme + 
  labs(x= "", y= "") + 
  scale_y_continuous(limits = c(0,70), breaks = c(10, 20, 30 ,40 ,50, 60, 70)) +
  coord_flip() +
  annotation_custom(grid::rasterGrob(tree,
                                     width=ggplot2::unit(1,"npc"),
                                     height=ggplot2::unit(1,"npc")),
                    ymin = -7.7, ymax = -8.7, xmin = 12.0, xmax = 13.5)

foresttopic.plot

componentall<- ggarrange(birdtopic.plot, foresttopic.plot,
                         ncol = 1, nrow = 2,
                         hjust = -1)

componentall


###################################### FIGURE __ ######################################################################
######################### BIRD SUCCESS COMPONENT BY COMPARATOR #################################################

Compmeta<- read.csv("out/ForestComp.meta.csv")

#create column for percentage for plotting
#factor the duration
Compmeta$value <- factor(Compmeta$value, levels = c("Composition", "Forested area", "Land use type", "Canopy cover", 
                                                    "Fragmentation", "Connectivity", "Diversity metric",
                                                    "Individual tree management", "Native species","Exotic/invasive species"))


comparator.plot<- ggplot(Compmeta, aes(fill = yes.no, x= value, y= comparator)) +
  geom_bar(stat= 'identity', colour = "black", size = 2.0) + 
  bigtheme + 
  labs(x= "", y= "") + 
  scale_y_continuous(limits = c(0,100), breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_fill_viridis_d(option= "viridis", labels = c("Yes", "No")) +
  #scale_fill_manual(labels= c("Yes", "No"),values = birdpalette2) + 
  coord_flip() + 
  theme_legend7

bird.comparatorplot<- comparator.plot + labs(fill= "Comparator used?")
bird.comparatorplot

#PLOTTING

Forestcompmeta<- read.csv("out/FComp.count.csv")


Forestcompmeta$value <- factor(Forestcompmeta$value, levels = c("Composition", "Land use type", "Canopy cover", "Individual tree management", 
                                                                "Forested area", "Native species", "Exotic/invasive species", "Connectivity",
                                                                "Diversity metric", "Fragmentation"))

comparator.plot<- ggplot(Forestcompmeta, aes(fill = comp, x= value, y= comparator)) +
  geom_bar(stat= 'identity', colour= "black", size = 2.0) +
  bigtheme + 
  labs(x= "", y= "") + 
  scale_y_continuous(limits = c(0,100), breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_fill_viridis_d(option= "viridis", labels = c("Yes", "No")) +
  #scale_fill_manual(values= forestpalette2, labels= c("Yes", "No")) + 
  coord_flip() +
  theme_legend7

forest.comparatorplot<- comparator.plot + labs(fill= "Comparator used?")

forest.comparatorplot

all.comparatorplot<- ggarrange(bird.comparatorplot, forest.comparatorplot,
                               font.label = list(color= "black"),
                               ncol = 2, nrow = 1)
all.comparatorplot

#######################################      ######################################################################
############################ BIRD SUCCESS COMPONENT BY URBAN SCALE ###########################################

Urban <- read.csv("out/forestbird.urbcount.csv")
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
Urban <- Urban %>% pivot_longer(., cols = c(`Canopy cover`, `Composition`, Connectivity, `Diversity metric`, `Exotic/invasive species`, 
                                            `Forested area`, `Fragmentation`,`Individual tree management`, `Land use type`, `Native species`))
Urban <- as.data.frame(Urban)
#sort
Urban <- Urban[order(Urban[,2]), ]
Totals<- c(95,95,95,95,164,164,164,164,24,24,24,24,21,21,21,21,16,16,16,16,127,127,127,127,
           21,21,21,21,18,18,18,18,96,96,96,96,21,21,21,21)

Urban <-cbind(Urban, Totals)

#now add column of percentages 
Urban$percent <- (Urban$value/Urban$Totals)*100

Urban$name <- factor(Urban$name, levels = c("Composition", "Forested area", "Land use type", "Canopy cover", 
                                            "Fragmentation", "Connectivity", "Diversity metric",
                                            "Individual tree management", "Native species","Exotic/invasive species"))

#PLOTTING ####################################################################

scale.plot <- ggplot(Urban, aes(fill= Urb.scale, x= name , y= percent)) +
  geom_bar(stat= 'identity', colour= "black", size = 2.0) + 
  labs(x= "", y= "") + 
  coord_flip() + 
  scale_y_continuous(limits= c(0,101)) + 
  scale_fill_viridis_d(option= "viridis") +
  #scale_fill_manual(values = birdpalette4) +
  bigtheme +
  theme_legend6 +
  guides(fill = guide_legend(nrow = 2))

bird.scaleplot<- scale.plot + labs(fill= "") 
bird.scaleplot

#FOREST#
Urban <- read.csv("out/FUrb.count.csv")
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

scale.plot <- ggplot(Urban, aes(fill= Urb.scale, x= name, y= percent)) +
  geom_bar(stat= 'identity', colour= "black", size = 2.0) + 
  labs(x= "", y= "") + 
  coord_flip() + 
  scale_y_continuous(limits= c(0,100)) + 
  scale_fill_viridis_d(option= "viridis") +
  #scale_fill_manual(values= forestpalette4) +
  bigtheme +
  theme_legend6 +
  guides(fill = guide_legend(nrow = 2))

forest.scaleplot<- scale.plot + labs(fill= "") 

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

recs<- read.csv("out/forestAllrecraw.count.csv")

#sort based on bird category
recs <- recs[order(recs[,2]), ]
#manually add a column of totals
totals<- c(108,108,108,108,108,108,108,191,191,191,191,191,191,191,29,29,29,29,29,21,21,21,  21,21,21,21,21,21,152,152,152,
           152,152,152,152,152,23,23,23,23,23,20,20,20,20,20,113,113,113,113,113,113,113,28,28,28,28,28,28)
#bind
recs<- cbind(recs, totals)

recs$percent<- (recs$n/recs$totals)*100
recs$Rec.1[recs$Rec.1 == 'No Recommendations'] <- 'None'

##### PLOT

#factor columns to correct order
recs$value <- factor(recs$value , levels = c("Composition", "Forested area", "Land use type", "Canopy cover", 
                                             "Fragmentation", "Connectivity", "Diversity metric",
                                             "Individual tree management", "Native species","Exotic/invasive species"))

recs$Rec.1 <- factor(recs$Rec.1, levels= c("Restoration", "Management", "Conservation", "None"))

recsbird <-recs %>%
  group_by(Rec.1, value) %>%
  summarise(percent = sum(percent), count = first(n)) 


rec.plot <- ggplot(recsbird, aes(fill= Rec.1, x= value , y= percent)) +
  geom_bar(stat= 'identity', colour = "black", size = 2.0) + 
  labs(x= "", y= "Percent of Studies") +
  coord_flip() + 
  scale_y_continuous(limits= c(0,100)) + 
  scale_fill_viridis_d(option= "viridis") +
  labs(fill = "") +
  #scale_fill_manual(values = birdpalette4s, name =  "") + 
  theme_legend6 +
  bigtheme +
  guides(fill = guide_legend(nrow = 2))

birdrecoplot <- rec.plot

birdrecoplot

#theme(axis.text.x = element_text(colour= "black", size= 150), axis.text.y = element_text(size = 150, colour= "black", angle = 0)) 

##### FOREST
recs<- read.csv("out/FAllrec.count.csv")
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

recsforest <-recs %>%
  group_by(Rec1, value) %>%
  summarise(percent = sum(percent), count = first(n)) 


rec.plot <- ggplot(recsforest, aes(fill= Rec1, x= value , y= percent)) +
  geom_bar(stat= 'identity', colour= "black", size = 2.0) + 
  labs(x= "", y= "Percent of Studies") +
  coord_flip() + 
  bigtheme +
  scale_y_continuous(limits= c(0,100)) + 
  theme_legend6 +
  scale_fill_viridis_d(option= "viridis") +
  labs(fill ="") +
  #scale_fill_manual(values = forestpalette4s, name =  "") +
  guides(fill = guide_legend(nrow = 2))

forestrecoplot <- rec.plot
forestrecoplot

#COMBINE IN ONE PLOT

all.recoplot<- ggarrange(birdrecoplot, forestrecoplot,
                         font.label = list(color= "black"),
                         ncol = 2, nrow = 1)
all.recoplot

#COMBINE IN ONE PLOT

all.recoplot<- ggarrange(birdrecoplot, forestrecoplot,
                         font.label = list(color= "black"),
                         ncol = 2, nrow = 1)
all.recoplot

#################################################### EMBEDDED FIGURE WITH CATEGORY, RECOMMENDATION, COMPARATOR, URBAN SCALE ###################################


embeddedfigforest<- ggarrange(birdtopic.plot, foresttopic.plot,
                        bird.comparatorplot, forest.comparatorplot,
                        bird.scaleplot, forest.scaleplot,
                        birdrecoplot, forestrecoplot,
                        labels= c("A", "B", "C", "D", "E", "F", "G", "H"),
                        font.label = list(color= "black", size = 200, family = "helvetica"),
                        ncol = 2, nrow = 4)
embeddedfigforest

ggsave(filename ="graphics/Figure6.png", width = 160, height = 121, dpi=100, limitsize = FALSE)  

