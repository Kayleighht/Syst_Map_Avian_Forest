#packagesneeded
Packages <- c("tidyverse", "ggplot2", "maps", "bibliometrix", "ggthemes", "cartography", "sf")
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
                      "Rec.1", "Rec.2", "Rec.3")]

#rename column in av.meta to match shape files with countries
names(av.meta)[names(av.meta) == "Study.country"] <- "COUNTRY"

#replace empty cells with N/A
av.meta <- replace(av.meta, av.meta=='', NA)
print(av.meta)

#PUSH OUT CSV
write.csv(av.meta, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/AV.META.csv", row.names = FALSE)


############################### DATA PREPARATION FOR COUNT TABLES ##############################################################

#COUNT TABLE
#number of publications per year
year.count <- av.meta %>%
  group_by(Year) %>%
  dplyr::mutate(Year_count= n())
#remove duplicates
year.count <- year.count[!duplicated(year.count$Year), ]

#PUSH OUT CSV
write.csv(year.count, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/Year.count.csv", row.names = FALSE)

#COUNT TABLE
#journal name count by study country
country.count <- av.meta %>%
  group_by(COUNTRY, Journal) %>%
  dplyr::mutate(journal.count= n())

#PUSH OUT CSV
write.csv(country.count, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/Country.count.csv", row.names = FALSE)

#journal name country by recommendation included
journal.rec<- av.meta %>%
  group_by(Rec., Journal) %>%
  dplyr::mutate(journal.count=n())

#PUSH OUT CSV
write.csv(journal.rec, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/journal.rec.csv", row.names = FALSE)

#COUNT TABLE
#count only by COUNTRY
heatmap.count <- av.meta %>%
  group_by(COUNTRY) %>%
  dplyr::mutate(country.count= n())

#PUSH OUT CSV
write.csv(heatmap.count, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/Heatmap.count.csv", row.names = FALSE)

#COUNT TABLE
#all bird domain columns grouped by URBAN SCALE
urb.counts <-av.meta %>%
  pivot_longer(cols = c(birddomain1:birddomain4)) %>%
  dplyr::count(Urb.scale,value)

#PUSH OUT CSV
write.csv(urb.counts, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/Urb.count.csv", row.names = FALSE)

#COUNT TABLE
#all bird domain columns grouped by COMPARATOR USED Y/N
comp.counts <-av.meta %>%
  pivot_longer(cols = c(birddomain1:birddomain4)) %>%
  dplyr::count(Comparator,value)

#PUSH OUT CSV
write.csv(comp.counts, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/Comp.count.csv", row.names = FALSE)

########################################### DATA SUBSETTING AND ORGANIZING ##################################################

#subset data for STUDY DURATION 
duration.df<- av.meta[,c("birddomain1", "birddomain2", "birddomain3", "birddomain4", "Start.year", "End.year")]

##### CLEANING ########
#remove N/A's
duration.df<- duration.df[!grepl('N/A', duration.df$Start.year),]
duration.df<-duration.df[!grepl('N/A', duration.df$End.year),]

#ensure columns are formatted s numerics
duration.df <- duration.df %>% mutate_at(c('Start.year', 'End.year'), as.numeric)
duration.df <- as.data.frame(duration.df)

####### ORGANIZING #######
#create new column with total duration of study
duration.df$duration <- as.numeric(duration.df$End.year-duration.df$Start.year)

#since single year studies are zeros according to calculation, I will transform column by adding one to all values
duration.df$duration2 <-duration.df[,7]+1

#create BINS for values based on duration column
duration.df<- duration.df %>% mutate(duration_bin = cut(duration2, breaks=c(0, 1, 5, 10, 155)))

#COUNT TABLE
#all bird domain columns grouped by STUDY DURATION BIN
duration.counts <-duration.df %>%
  pivot_longer(cols = c(birddomain1:birddomain4)) %>%
  count(duration_bin,value)

duration.counts2 <- duration.df %>%
  count(duration_bin)

#remove duplicate N/As from additional columns for plotting
duration.counts <- duration.counts %>% drop_na(duration_bin)
duration.counts <- duration.counts %>% drop_na(value)
duration.counts2 <- duration.counts2 %>% drop_na(n)

#PUSH OUT CSV
write.csv(duration.counts, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/Duration.count.csv", row.names = FALSE)
write.csv(duration.counts2, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/Duration.count2.csv", row.names = FALSE)

############################################
######## SUBSETTING RECOMMENDATIONS #######
###########################################

################SUBSET FOR RECOMMENDATIONS

recdf<- av.meta[,c("birddomain1","birddomain2", "birddomain3", "birddomain4", "Rec.",
                   "Rec.1", "Rec.2", "Rec.3")]

#COUNT TABLE
#including all bird domain columns grouped by RECOMMENDATION TYPE 

#REC 1
rectype.counts <-as.data.frame(recdf %>%
                                 pivot_longer(cols = c(birddomain1:birddomain4)) %>%
                                 dplyr::count(Rec.1,value))
#clean nd remove N/As
rectype.counts <- rectype.counts %>% drop_na(value)
rectype.counts<- rectype.counts[!grepl("N/A", rectype.counts$Rec.1),]
rectype.counts<- rectype.counts[!grepl("N/A", rectype.counts$value),]

#REC 2
rectype.counts2 <-as.data.frame(recdf %>%
                                  pivot_longer(cols = c(birddomain1:birddomain4)) %>%
                                  dplyr::count(Rec.2,value))
#remove empty cells from studies with only 1 recommendation ##CHECK CHECK DOUBLE CHECK
rectype.counts2 <- rectype.counts2 %>% drop_na(Rec.2)
rectype.counts2 <- rectype.counts2 %>% drop_na(value)
#rename column to rec.1 to merge
names(rectype.counts2)[names(rectype.counts2) == "Rec.2"] <- "Rec.1"
sum(rectype.counts2$n)

#REC 3
rectype.counts3 <-as.data.frame(recdf %>%
                                  pivot_longer(cols = c(birddomain1:birddomain4)) %>%
                                  dplyr::count(Rec.3,value))
rectype.counts3 <- rectype.counts3 %>% drop_na(Rec.3)
rectype.counts3 <- rectype.counts3 %>% drop_na(value)
names(rectype.counts3)[names(rectype.counts3) == "Rec.3"] <- "Rec.1"
sum(rectype.counts3$n)

#MERGE tables of ALL recommendations
rectype.all <- bind_rows(rectype.counts, rectype.counts2, rectype.counts3) %>% 
  group_by(value, n) %>% 
  distinct(.keep_all = TRUE)

#calculate the percent according to topic and recommendation from TOTAL papers
rectype.all
sum(rectype.all$n)
rectype.all$percent<- ((rectype.all$n/274)*100)
rectype.all

#PUSH OUT CSV
write.csv(rectype.all, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/Allrec.count.csv", row.names = FALSE)

#create a table to count the number of "NO" recommendations
table(recdf['Rec.'])
norec<- data.frame(
  Rec.1 = "None",
  value= "No Recommendations",
  n = 99)
norec$percent <- ((99/274)*100)
norec

#PUSH OUT CSV
write.csv(norec, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/Norec.count.csv", row.names = FALSE)

#######                                           ############
####### SUBSETTING FOR MULTIPLE INDICATORS COUNTS ############
#######                                           ############

#subset data frame to sort # publications with multiple indicators
Indicators <- av.meta[,c("birddomain1","birddomain2", "birddomain3", "birddomain4")]

#COUNT TABLE
#create count table including all bird domain columns grouped by Indicator TYPE (first only)
# FIRST INDICATOR
Ind1 <- journal.rec<- Indicators %>%
  group_by(birddomain1) %>%
  dplyr::mutate(indicator1.count=n())
#remove duplicates in first indicator column
Ind1 <- Ind1[!duplicated(Ind1$birddomain1),]
#remove N/A columns
Ind1<- Ind1 %>% drop_na(birddomain1)
#calculate sum
sum(Ind1$indicator1.count)


# SECOND INDICATOR
Ind2 <- journal.rec<- Indicators %>%
  group_by(birddomain2) %>%
  dplyr::mutate(indicator2.count=n())
Ind2 <- Ind2[!duplicated(Ind2$birddomain2), ]
Ind2<- Ind2 %>% drop_na(birddomain2)
#calculate sum
sum(Ind2$indicator2.count)

# THIRD INDICATOR
Ind3 <- journal.rec<- Indicators %>%
  group_by(birddomain3) %>%
  dplyr::mutate(indicator3.count=n())
Ind3 <- Ind3[!duplicated(Ind3$birddomain3), ]
Ind3<- Ind3 %>% drop_na(birddomain3)
#calculate sum
sum(Ind3$indicator3.count)

#FOURTH INDICATOR
Ind4 <- journal.rec<- Indicators %>%
  group_by(birddomain4) %>%
  dplyr::mutate(indicator4.count=n())
Ind4 <- Ind4[!duplicated(Ind4$birddomain4), ]
Ind4<- Ind4 %>% drop_na(birddomain4)
sum(Ind4$indicator4.count)

#create data frame to sort # publications with multiple indicators
#use calculate sums ^above
number_topics <- c("One Indicator", "Two Indicators", "Three Indicators", "Four Indicators")
number_publications <- c("277","61", "12", "2")

#create dataframe
allindicators<- data.frame(number_topics, number_publications)  
#PUSH OUT CSV
write.csv(allindicators, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/Allind.count.csv", row.names = FALSE)


####################################
####### TOPICS ONLY COUNT ##########
####################################

######### Categories only count ##############
#create dataframes for each indicator/topic and align column names
birddomain1 <- Ind1[,-c(2:4)]
colnames(birddomain1)[2] = "indicatorcount"
colnames(birddomain1)[1] = "birdomain"
birdomain2<- Ind2[,-c(1,3:4)]
colnames(birdomain2)[2] = "indicatorcount"
colnames(birdomain2)[1] = "birdomain"
birdomain3<- Ind3[,-c(1:2,4)]
colnames(birdomain3)[2] = "indicatorcount"
colnames(birdomain3)[1] = "birdomain"
birdomain4<- Ind4[,-c(1:3)]
colnames(birdomain4)[2] = "indicatorcount"
colnames(birdomain4)[1] = "birdomain"

#BIND
allbird<-rbind(birddomain1, birdomain2, birdomain3, birdomain4)
merge1<- merge(birddomain1, birdomain2, by= "birdomain", all= TRUE)
merge2<- merge(birdomain3, birdomain4, by= "birdomain", all= TRUE)
merge1$total<- merge1$indicatorcount.x + merge1$indicatorcount.y
merge1<- merge1[,-c(2:3)]
merge2[is.na(merge2)] <- 0
merge2$total<- merge2$indicatorcount.x + merge2$indicatorcount.y
merge2<- merge2[,-c(2:3)]

#combine all for a total across multiple indictors *** will have more articles than total
birdcomponent<- merge(merge1, merge2, by= "birdomain")
birdcomponent$total<- birdcomponent$total.x + birdcomponent$total.y
#remove N/A
birdcomponent<- birdcomponent[!grepl('N/A', birdcomponent$birdomain),]

#PUSH OUT CSV
write.csv(birdcomponent, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/bird.component.csv", row.names = FALSE)











