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

#################################################################################################################################
##############################################################################################################################
       #     BIRD COMPONENT BY FOREST COMPONENT #
#############################################################################################################################
##########################################################################################################################

birdforest1 <- as.data.frame(av.meta %>%
                           pivot_longer(birddomain1:birddomain4))

birdforest1 <- birdforest1[,c("forest1", "forest2", "forest3", "forest4", "name", "value")]
#forest 5 is empty!!!!! don't need


#COUNT TABLE
#create count table including all bird domain columns grouped by Indicator TYPE (first only)
# FIRST INDICATOR
birdforest <- birdforest1 %>% count(forest1, value, sort = TRUE)
#clean duplicates and NAs
birdforest[birdforest == ""] <- NA
birdforest<- birdforest %>% drop_na()
birdforest <-birdforest[!grepl('N/A', birdforest$value),]
colnames(birdforest)[1] = "forest"

birdforest2 <- birdforest1 %>% count(forest2, value, sort = TRUE)
#clean duplicates and NAs
birdforest2[birdforest2 == ""] <- NA 
birdforest2<- birdforest2 %>% drop_na()
colnames(birdforest2)[1] = "forest"


birdforest3<- birdforest1 %>% count(forest3, value, sort=TRUE)
#clean duplicates and NAs
birdforest3[birdforest3 == ""] <- NA
birdforest3[birdforest3 == " "] <- NA
birdforest3<- birdforest3 %>% drop_na()
colnames(birdforest3)[1] = "forest"


birdforest4<- birdforest1 %>% count(forest4, value, sort=TRUE)
#clean duplicates and NAs
birdforest4[birdforest4 == ""] <- NA 
birdforest4<- birdforest4 %>% drop_na()
birdforest4 <-birdforest4[!grepl('N/A', birdforest4$value),]
colnames(birdforest4)[1] = "forest"


birdforestmerged<- rbind(birdforest, birdforest2, birdforest3, birdforest4)


###########################################################################################################################################################
############################################################################################################################################################
                                              # BIRD SPECIFIC CATEGORIES BY FOREST COMPONENT #
################################################################################################################################################################
################################################################################################################################################################


av.data

#separate bird categories(specific) column into new columns based on comma
av.data1 <-separate(data = av.data, col = Bird.category, into = c("bird1", "bird2", "bird3", "bird4", "bird5"), sep = ",")

#separate forest comp column into new columns based on comma
av.data1 <-separate(data = av.data1, col = Forest.comp, into = c("forest1", "forest2", "forest3", "forest4"), sep = ",")

#cut down to only necessary columns for figures (so far..)
av.data1 <- av.data1[,c("forest1", "forest2", "forest3", "forest4", 
                              "bird1", "bird2", "bird3", "bird4",
                              "bird5")] 

#make forest components columns 
av.data1 <- as.data.frame(av.data1 %>%
                               pivot_longer(forest1:forest4))
av.data1 <- as.data.frame(av.data1)

#remove NAs in value column (no data)
av.data1 <- av.data1 %>% drop_na(value)

#SEPARATE BY BIRD CATEGORY COLUMN

#COUNT TABLE
#create count table including all bird domain columns grouped by Indicator TYPE (first only)

# FIRST CATEGORY
birdcat1 <- av.data1 %>% count(bird1, value, sort = TRUE)
#clean duplicates and NAs
birdcat1[birdcat1 == ""] <- NA
birdcat1<- birdcat1 %>% drop_na()
birdcat1 <-birdcat1[!grepl('N/A', birdcat1$birdcategory),]
colnames(birdcat1)[1] = "birdcategory"
birdcat1

birdcat2 <- av.data1 %>% count(bird2, value, sort = TRUE)
#clean duplicates and NAs
birdcat2[birdcat2 == ""] <- NA 
birdcat2<- birdcat2 %>% drop_na()
colnames(birdcat2)[1] = "birdcategory"
birdcat2

birdcat3 <- av.data1 %>% count(bird3, value, sort=TRUE)
#clean duplicates and NAs
birdcat3[birdcat3 == ""] <- NA
birdcat3[birdcat3 == " "] <- NA
birdcat3<- birdcat3 %>% drop_na()
colnames(birdcat3)[1] = "birdcategory"
birdcat3

birdcat4<- av.data1 %>% count(bird4, value, sort=TRUE)
#clean duplicates and NAs
birdcat4[birdforest4 == ""] <- NA 
birdforest4<- birdcat4 %>% drop_na()
birdcat4 <-birdcat4[!grepl('N/A', birdcat4),]
colnames(birdcat4)[1] = "birdcategory"
birdcat4

birdcat5<- av.data1 %>% count(bird5, value, sort=TRUE)
#clean duplicates and NAs
birdcat5[birdcat5 == ""] <- NA 
birdcat5<- birdcat5 %>% drop_na()
birdcat5 <-birdcat5[!grepl('N/A', birdcat5),]
colnames(birdcat5)[1] = "birdcategory"
birdcat5


birdcatmerged <- rbind(birdcat1, birdcat2, birdcat3, birdcat4, birdcat5)
write.csv(birdcatmerged, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/birdcatmerged.csv", row.names = FALSE)

