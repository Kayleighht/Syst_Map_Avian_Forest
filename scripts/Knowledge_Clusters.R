#packagesneeded
Packages <- c("tidyverse", "ggplot2", "maps", "bibliometrix", "ggthemes", "cartography", "sf", "janitor")
lapply(Packages, library,character.only= TRUE)

getwd()
setwd("C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/in")

#loading avian data and formatting
av.data <- read.csv("Meta_Avian_Data_Extraction - Master_Data.csv")

########## CLEANING AND ORGANIZING DATA ###################################################################################

#cut down to only necessary columns for figures (so far..)
av.meta <- av.data[,c("Year", "Journal", "Study.country", "Urb.scale", 
                      "Start.year", "End.year", "Comparator", "birddomain1", 
                      "birddomain2", "birddomain3", "birddomain4", "Rec.",
                      "Rec.1", "Rec.2", "Rec.3","Forest.comp")]

#separate forest comp column into new columns based on comma
av.meta <-separate(data = av.meta, col = Forest.comp, into = c("forest1", "forest2", "forest3", "forest4", "forest5"), sep = ",")

# urban scale by forest component
urb.counts <-av.meta %>%
  pivot_longer(cols = c(forest1:forest5)) %>%
  dplyr::count(Urb.scale,value)
urb.counts <- as.data.frame(urb.counts)

#remove empty columns before pivot wider
urb.counts <- urb.counts %>% drop_na()
#drop remaining empty rows
urb.counts<- urb.counts[!apply(urb.counts == "", 1, all), ]
#still empty rows to be deleted
urb.counts <- urb.counts[-c(1:2,18:19,50:51),]

urb.wide<- pivot_wider(urb.counts, names_from = value, values_from = n)
#merge matching columns and add numbers
#first make nas to zeros
urb.wide<- as.data.frame(urb.wide)
urb.wide[is.na(urb.wide)] <- 0

#make first column row names
urb.all<- urb.wide[,-1]
rownames(urb.all) <- urb.wide[,1]
as.data.frame(urb.all)

#remove white space in data
urb.all <- urb.all %>%
  clean_names(allow_dupes= TRUE)
#check names
colnames(urb.all)

urb.all<- t(rowsum(t(urb.all), group = colnames(urb.all), na.rm = F))

##############################################################################
#FOREST
###############################################################################

#loading avian data and formatting
forest.data <- read.csv("META_Forest_Climate_Data_Extraction.csv")

########## CLEANING AND ORGANIZING DATA ###################################################################################

#cut down to only necessary columns for figures (so far..)
forest.data <- forest.data[,c("Year", "Journal", "Study.Country", "Urb.scale", 
                      "Year.start", "Year.end", "Comparator", "Rec.included",
                      "Rec1", "Rec2", "Rec3","Forest.comp", "Carbon.metric")]

#separate forest comp column into new columns based on comma
forest.data <-separate(data = forest.data, col = Forest.comp, into = c("forest1", "forest2", "forest3", "forest4", "forest5"), sep = ",")

# urban scale by forest component
urb.countsf <-forest.data %>%
  pivot_longer(cols = c(forest1:forest5)) %>%
  dplyr::count(Urb.scale,value)
urb.countsf <- as.data.frame(urb.countsf)

#remove empty columns before pivot wider
urb.countsf <- urb.countsf %>% drop_na()
#drop remaining empty rows
urb.countsf <- urb.countsf[!apply(urb.countsf == "", 1, all), ]

urb.widef<- pivot_wider(urb.countsf, names_from = value, values_from = n)
#merge matching columns and add numbers
#first make nas to zeros
urb.widef<- as.data.frame(urb.widef)
urb.widef[is.na(urb.widef)] <- 0

#make first column row names
urb.allf<- urb.widef[,-1]
rownames(urb.allf) <- urb.widef[,1]
as.data.frame(urb.allf)

#remove white space in data
urb.allf <- urb.allf %>%
  clean_names(allow_dupes= TRUE)
#check names
colnames(urb.allf)

urb.allf <- t(rowsum(t(urb.allf), group = colnames(urb.allf), na.rm = F))

###################################################################
############################################################
#############################################################
# CARBON METRIC BY FOREST COMPONENT
#############################################################

#separate forest comp column into new columns based on comma (carbon)
forest.carb <-separate(data = forest.data, col = Carbon.metric, into = c("carbon1", "carbon2", "carbon3", "carbon4"), sep = ",")

#subset only carbon metrics
carbon<- subset(forest.carb,select = c('carbon1','carbon2', 'carbon3', 'carbon4','forest1', 'forest2', 
                                       'forest3', 'forest4', 'forest5'))

carbon2 <- as.data.frame(carbon %>%
  pivot_longer(carbon1:carbon4))
colnames(carbon2)

#COUNT TABLE
#create count table including all bird domain columns grouped by Indicator TYPE (first only)
# FIRST INDICATOR
forest1 <- carbon2 %>% count(forest1, value, sort = TRUE)
#clean duplicates and NAs
forest1<- forest1 %>% drop_na(value)
colnames(forest1)[1] = "forest"

forest2 <- carbon2 %>% count(forest2, value, sort = TRUE)
#clean duplicates and NAs
forest2<- forest2 %>% drop_na(value)
forest2<- forest2 %>% drop_na(forest2)
colnames(forest2)[1] = "forest"


forest3<- carbon2 %>% count(forest3, value, sort=TRUE)
#clean duplicates and NAs
forest3<- forest3 %>% drop_na(value)
forest3<- forest3 %>% drop_na(forest3)
colnames(forest3)[1] = "forest"


forest4<- carbon2 %>% count(forest4, value, sort=TRUE)
#clean duplicates and NAs
forest4<- forest4 %>% drop_na(value)
forest4<- forest4 %>% drop_na(forest4)
colnames(forest4)[1] = "forest"


forest5<- carbon2 %>% count(forest5, value, sort=TRUE)
#clean duplicates and NAs
forest5<- forest5 %>% drop_na(value)
forest5<- forest5 %>% drop_na(forest5)
colnames(forest5)[1] = "forest"


forestmerged<- rbind(forest1, forest2, forest3, forest4, forest5)

#make forest columns the same name for merge
carbon3<- carbon2 %>% gather("forest1", "forest2", "forest3", "forest4", "forest5")
# urban scale by forest component
carb.counts <-forest.carb %>%
  pivot_longer(cols = c(forest1:forest5)) %>%
  dplyr::count(carbon1:carbon4 ,value)
urb.countsf <- as.data.frame(urb.countsf)
