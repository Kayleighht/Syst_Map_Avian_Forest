#packagesneeded
source('scripts/0-packages.R')

# themes 
source('scripts/0-themes.R')
# FOREST TO FOREST

# Figure 6 ----------------------------------------------------------------
# topics only
bird<-readPNG("graphics/bird.1.png", native = T)

#AVIAN
birdtopics<- read.csv("out/bird.component.csv")
#remove row 8 NA since the metric didn't fit into our topic categories
birdtopics<- birdtopics[-8,]

#create new column with percent of studies
#sum(birdtopics$total)
#birdtopics$percent <- (birdtopics$total/348)*100

birdtopic.plot <- ggplot(birdtopics, aes(x= reorder(Component, -percent) , y= percent)) +
  geom_bar(stat= 'identity', position = 'dodge', width = 0.9, fill = birdcol, colour = "black", size = 2)  + 
  bigtheme + 
  labs(x= "", y= "") + 
  scale_y_continuous(limits = c(0,100), breaks = c(20, 40, 60, 80 ,100)) +
  coord_flip() 
#  annotation_custom(grid::rasterGrob(bird,
#                                     width=ggplot2::unit(1,"npc"),
#                                     height=ggplot2::unit(1,"npc")),
#                    ymin = 1, ymax = 1, xmin = 2, xmax = 2) 

birdtopic.plot

#FOREST 
tree<-readPNG("graphics/tree.1.png", native = T)

foresttopics<- read.csv("out/forest.carbon.csv")

foresttopic.plot <- ggplot(foresttopics, aes(x= reorder(carbon, -percent) , y= percent)) +
  geom_bar(stat= 'identity', position = 'dodge', fill= forestcol, width = 0.9, colour = "black", size = 2) +
  bigtheme + 
  labs(x= "", y= "") + 
  scale_y_continuous(limits = c(0,100), breaks = c(20, 40, 60, 80 ,100)) +
  coord_flip() 
#  annotation_custom(grid::rasterGrob(tree,
#                                     width=ggplot2::unit(1,"npc"),
#                                    height=ggplot2::unit(1,"npc")),
#                    ymin = -7.7, ymax = -8.7, xmin = 12.0, xmax = 13.5)

foresttopic.plot

componentall<- ggarrange(birdtopic.plot, foresttopic.plot,
                         ncol = 1, nrow = 2,
                         hjust = -1)

componentall


###################################### FIGURE __ ######################################################################
######################### BIRD SUCCESS COMPONENT BY COMPARATOR #################################################

Compmeta<- read.csv("out/Comp.meta.csv")

#create column for percentage for plotting
#factor the duration
Compmeta$value <- factor(Compmeta$value, levels = c("Biodiversity", "Survival", "Demographics/Patterns", "Breeding", "Behaviour", "Resources", "Foraging"))

comparator.plot<- ggplot(Compmeta, aes(fill = comp, x= value, y= comparator)) +
  geom_bar(stat= 'identity', colour = "black", size = 2) +
  bigtheme + 
  labs(x= "", y= "") + 
  scale_y_continuous(limits = c(0,100), breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_fill_manual(labels= c("No", "Yes"), values = birdpalette2) + 
  coord_flip() + 
  theme_legend6

bird.comparatorplot<- comparator.plot + labs(fill= "Comparator used?")
bird.comparatorplot

#PLOTTING

Forestcompmeta<- read.csv("out/CarbComp.count.csv")


Forestcompmeta$value <- factor(Forestcompmeta$value, levels = c("Above-ground biomass", "Below-ground biomass", "Soil", "Dead wood/organic matter", "Infrastructure"))

comparator.plot<- ggplot(Forestcompmeta, aes(fill = Comparator, x= value, y= percent)) +
  geom_bar(stat= 'identity', colour = "black", size = 2) +
  bigtheme + 
  labs(x= "", y= "") + 
  scale_y_continuous(limits = c(0,100), breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_fill_manual(values= forestpalette2, labels= c("No", "Yes")) + 
  coord_flip() +
  theme_legend6

forest.comparatorplot<- comparator.plot + labs(fill= "Comparator used?")

forest.comparatorplot

all.comparatorplot<- ggarrange(bird.comparatorplot, forest.comparatorplot,
                               font.label = list(color= "black"),
                               ncol = 2, nrow = 1)
all.comparatorplot

#######################################      ######################################################################
############################ BIRD SUCCESS COMPONENT BY URBAN SCALE ###########################################

Urban <- read.csv("out/birdurb.count.csv")
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
Totals<- c(29,29,29,29,189,189,189,189,50,50,50,50,46,46,46,46,1,1,1,1,8,8,8,8,28,28,28,28)

Urban <-cbind(Urban, Totals)

#now add column of percentages 
Urban$percent <- (Urban$value/Urban$Totals)*100

Urban$name <- factor(Urban$name, levels = c("Biodiversity", "Survival", "Demographics/Patterns", "Breeding", "Behaviour", "Resources", "Foraging"))

#PLOTTING ####################################################################

scale.plot <- ggplot(Urban, aes(fill= Urb.scale, x= name , y= percent)) +
  geom_bar(stat= 'identity', colour = "black", size = 2) + 
  labs(x= "", y= "") + 
  coord_flip() + 
  scale_y_continuous(limits= c(0,100)) +
  scale_fill_viridis_d(option= "viridis") +
#  scale_fill_manual(values = birdpalette4) +
  bigtheme +
  theme_legend6 +
  guides(fill = guide_legend(nrow = 2))

bird.scaleplot<- scale.plot + labs(fill= "") 

bird.scaleplot

#FOREST###########################################################

Urban <- read.csv("out/CarbUrb.count.csv")
#remove n/a's created from empty rows
Urban <- Urban %>% drop_na(value)

#sort alphabetically to create percentages
Urban <- Urban[order(Urban[,"Urb.scale"]), ]
unique(Urban$value)
unique(Urban$Urb.scale)
#get total for totals
sum(Urban$n)

#spread rows so that we can create percent bars that reach 100%
Urban<- Urban %>% spread(value, n)
#replace NA with 0
Urban[is.na(Urban)] <- 0

#pivot table
Urban <- Urban %>% pivot_longer(., cols = c(`Above-ground biomass`,`Below-ground biomass`, `Dead wood/organic matter`, `Infrastructure`, Soil))

colnames(Urban)
Urban <- as.data.frame(Urban)
#sort
Urban <- Urban[order(Urban[,2]), ]
Totals<- c(164,164,164,164,58,58,58,58,15,15,15,15,3,3,3,3,30,30,30,30)

Urban <-cbind(Urban, Totals)

#now add column of percentages 
Urban$percent <- (Urban$value/Urban$Totals)*100

## PLOT #############################################################################################

Urban$name <- factor(Urban$name, levels = c("Above-ground biomass", "Below-ground biomass", "Soil", "Dead wood/organic matter", "Infrastructure"))


scale.plot <- ggplot(Urban, aes(fill= Urb.scale, x= name, y= percent)) +
  geom_bar(stat= 'identity', colour = "black", size = 2) + 
  labs(x= "", y= "") + 
  coord_flip() + 
  scale_y_continuous(limits= c(0,100)) + 
  scale_fill_viridis_d(option= "viridis") +
#  scale_fill_manual(values= forestpalette4) +
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

recs<- read.csv("out/Allrecraw.count.csv")

#sort based on bird category
recs <- recs[order(recs[,2]), ]
#manually add a column of totals
totals<- c(31,31,31,31,31,224,224,224,224,224,224,224,224,53,53,53,53,53,53,
           50,50,50,50,50,50,50,1,8,8,8,29,29,29,29,29)
#bind
recs<- cbind(recs, totals)

recs$percent<- (recs$n/recs$totals)*100
recs$Rec.1[recs$Rec.1 == 'No Recommendations'] <- 'None'

recs <- as.data.frame(recs %>% group_by(Rec.1, value)) %>% 
        summarise(n / sum(n))

#factor columns to correct order
recs$value <- factor(recs$value, levels = c("Biodiversity", "Survival", "Demographics/Patterns", "Breeding", "Behaviour", "Resources", "Foraging"))
recs$Rec.1 <- factor(recs$Rec.1, levels= c("Restoration", "Management", "Conservation", "None"))


rec.plot <- ggplot(recs, aes(fill= Rec.1, x= value , y= percent)) +
  geom_bar(stat= 'identity', colour = "black", size = 2) + 
  labs(x= "", y= "Percent of Studies") +
  coord_flip() + 
  scale_y_continuous(limits= c(0,100)) + 
  scale_fill_viridis_d(option= "viridis", name= "") +
#  scale_fill_manual(values = birdpalette4s, name =  "") + 
  theme_legend6 +
  bigtheme +
  guides(fill = guide_legend(nrow = 2))

birdrecoplot <- rec.plot

birdrecoplot

#theme(axis.text.x = element_text(colour= "black", size= 150), axis.text.y = element_text(size = 150, colour= "black", angle = 0)) 

##### FOREST
recs<- read.csv("out/CarbAllrec.count.csv")
#remove leading and trailing zeros
recs<- recs %>% 
  mutate(across(where(is.character), str_trim))

#sort based on bird category
recs <- recs[order(recs[,"value"]), ]

#manually add a column of totals
totals<- c(164,164,164,164,58,58,58,58,15,15,15,15,3,30,30,30,30)
#bind
recs<- cbind(recs, totals)

recs$percent<- (recs$n/recs$totals)*100
recs$Rec1[recs$Rec1 == 'No Recommendations'] <- 'None'

##### PLOT
recs$value <- factor(recs$value, levels = c("Above-ground biomass", "Below-ground biomass", "Soil", "Dead wood/organic matter", "Infrastructure"))


recs$Rec1 <- factor(recs$Rec1, levels= c("Restoration", "Management", "Conservation", "None"))


rec.plot <- ggplot(recs, aes(fill= Rec1, x= value , y= percent)) +
  geom_bar(stat= 'identity', colour = "black", size = 2) + labs(x= "", y= "Percent of Studies") +
  coord_flip() + 
  bigtheme +
  scale_y_continuous(limits= c(0,100)) + 
  theme_legend6 +
  scale_fill_viridis_d(option= "viridis", name = "") +
#  scale_fill_manual(values = forestpalette4s, name =  "") +
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


embeddedfig<- ggarrange(birdtopic.plot, foresttopic.plot,
                        bird.comparatorplot, forest.comparatorplot,
                        bird.scaleplot, forest.scaleplot,
                        birdrecoplot, forestrecoplot,
                        labels= c("1", "", "2", "", "3", "", "4", ""),
                        font.label = list(color= "black", size = 200, family = "serif"),
                        ncol = 2, nrow = 4)

embeddedfig

ggsave(filename ="graphics/Figure6b.png", width = 160, height = 121, dpi=100, limitsize = FALSE)  

