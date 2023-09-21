#packagesneeded
Packages <- c("tidyverse", "ggplot2", "maps", "bibliometrix", "ggthemes", "cartography", "sf")
lapply(Packages, library,character.only= TRUE)

getwd()
setwd("C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/in")

#loading avian data and formatting
forest.data<- read.csv("META_Forest_Climate_Data_Extraction.csv")

########################################################################################################################################
########################################### FOREST COMPONENT ########################################################################
###########################################################################################################################################

#cut down to only necessary columns for figures (so far..)
forest.meta <- forest.data[,c("Year", "Journal", "Study.Country", "Urb.scale", 
                      "Year.start", "Year.end", "Comparator", "Forest_Comp1", 
                      "Forest_Comp2", "Forest_Comp3", "Forest_Comp4", "Forest_Comp5", "Rec.included",                    
                      "Rec1", "Rec2", "Rec3")]

#rename column in av.meta to match shape files with countries
names(forest.meta)[names(forest.meta) == "Study.Country"] <- "COUNTRY"

#replace empty cells with N/A
forest.meta <- replace(forest.meta, forest.meta=='United States of America', "USA")
print(forest.meta)

#PUSH OUT CSV
write.csv(forest.meta, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FOREST.META.csv", row.names = FALSE)

#COUNT TABLE
#number of publications per year
fyear.count <- forest.meta %>%
  group_by(Year) %>%
  dplyr::mutate(Year_count= n())

#remove duplicates
fyear.count <- fyear.count[!duplicated(fyear.count$Year), ]

#PUSH OUT CSV
write.csv(fyear.count, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FYear.count.csv", row.names = FALSE)

#COUNT TABLE
#journal name count by study country
fcountry.count <- forest.meta %>%
  group_by(COUNTRY, Journal) %>%
  dplyr::mutate(journal.count= n())

#PUSH OUT CSV
write.csv(fcountry.count, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FCountry.count.csv", row.names = FALSE)

#COUNT TABLE
#journal name country by recommendation included
fjournal.rec<- forest.meta %>%
  group_by(Rec.included, Journal) %>%
  dplyr::mutate(journal.count=n())

#PUSH OUT CSV
write.csv(fjournal.rec, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/fjournal.rec.csv", row.names = FALSE)

#COUNT TABLE
#count only by COUNTRY
fheatmap.count <- forest.meta %>%
  group_by(COUNTRY) %>%
  dplyr::mutate(country.count= n())

#PUSH OUT CSV
write.csv(fheatmap.count, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FHeatmap.count.csv", row.names = FALSE)

#COUNT TABLE
#create count table including all bird domain columns grouped by URBAN SCALE
furb.counts <-forest.meta %>%
  pivot_longer(cols = c(Forest_Comp1:Forest_Comp5)) %>%
  dplyr::count(Urb.scale,value)

#PUSH OUT CSV
write.csv(furb.counts, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FUrb.count.csv", row.names = FALSE)

#COUNT TABLE
#create count table including all bird domain columns grouped by COMPARATOR USED Y/N
fcomp.counts <-forest.meta %>%
  pivot_longer(cols = c(Forest_Comp1:Forest_Comp5)) %>%
  dplyr::count(Comparator,value)

#PUSH OUT CSV
write.csv(fcomp.counts, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FComp.count.csv", row.names = FALSE)

#subset data for STUDY DURATION 
#clean and organize data
fduration.df<- forest.meta[,c("Forest_Comp1", "Forest_Comp2", "Forest_Comp3", "Forest_Comp4", "Forest_Comp5", "Year.start", "Year.end")]

#remove N/A
fduration.df<- fduration.df[!grepl('N/A', fduration.df$Year.start),]
fduration.df<-fduration.df[!grepl('N/A', fduration.df$Year.end),]

#ensure columns are numerics
fduration.df <- fduration.df %>% mutate_at(c('Year.start', 'Year.end'), as.numeric)
fduration.df <- as.data.frame(fduration.df)

#create new column with total duration of study
fduration.df$duration <- as.numeric(fduration.df$Year.end-fduration.df$Year.start)

#since single year studies are zeros according to calculation, I will transform column by adding one to all values
fduration.df$duration2 <-fduration.df[,8]+1

#create BINS for values based on duration column
fduration.df<- fduration.df %>% mutate(duration_bin = cut(duration2, breaks=c(0, 1, 5, 10, 100)))

#create count table including all bird domain columns grouped by STUDY DURATION BIN
fduration.counts <-fduration.df %>%
  pivot_longer(cols = c(Forest_Comp1:Forest_Comp5)) %>%
  count(duration_bin,value)

fduration.counts2 <- fduration.df %>%
  count(duration_bin)

#remove duplicate N/As from additional columns for plotting
fduration.counts <- fduration.counts %>% drop_na(duration_bin)
fduration.counts <- fduration.counts %>% drop_na(value)
fduration.counts2 <- fduration.counts2 %>% drop_na(duration_bin)

#PUSH OUT CSV
write.csv(fduration.counts, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FDuration.count.csv", row.names = FALSE)
write.csv(fduration.counts2, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FDuration.count2.csv", row.names = FALSE)

############################################
######## SUBSETTING RECOMMENDATIONS ######
##########################################

################SUBSET FOR RECOMMENDATIONS
frecdf<- forest.meta[,c("Forest_Comp1","Forest_Comp2", "Forest_Comp3", "Forest_Comp4", "Forest_Comp5", "Rec.included",
                   "Rec1", "Rec2", "Rec3")]

#replace N/As with not recommendation in Rec1 column to incorporate "NO" recommendations
frecdf <- frecdf %>% 
  mutate(across('Rec1', str_replace, 'N/A', 'No Recommendations'))

#COUNT TABLE
#create count table including all bird domain columns grouped by RECOMMENDATION TYPE (first only)

#REC 1
frectype.counts <-as.data.frame(frecdf %>%
                                 pivot_longer(cols = c(Forest_Comp1:Forest_Comp5)) %>%
                                 dplyr::count(Rec1,value))
frectype.counts <- frectype.counts[!grepl("N/A", frectype.counts$Rec1),]
frectype.counts <- frectype.counts[!grepl("N/A", frectype.counts$value),]
sum(frectype.counts$n)

#REC 2
frectype.counts2 <-as.data.frame(frecdf %>%
                                  pivot_longer(cols = c(Forest_Comp1:Forest_Comp5)) %>%
                                  dplyr::count(Rec2,value))

#remove empty cells from studies with only 1 recommendation ##CHECK CHECK DOUBLE CHECK
frectype.counts2 <- frectype.counts2[!grepl("N/A", frectype.counts2$Rec2),]
frectype.counts2 <- frectype.counts2[!grepl("N/A", frectype.counts2$value),]
#rename column to rec.1 to merge
names(frectype.counts2)[names(frectype.counts2) == "Rec2"] <- "Rec1"
sum(frectype.counts2$n)

#REC 3
frectype.counts3 <-as.data.frame(frecdf %>%
                                  pivot_longer(cols = c(Forest_Comp1:Forest_Comp5)) %>%
                                  dplyr::count(Rec3,value))
frectype.counts3 <- frectype.counts3[!grepl("N/A", frectype.counts3$Rec3),]
frectype.counts3 <- frectype.counts3[!grepl("N/A", frectype.counts3$value),]

names(frectype.counts3)[names(frectype.counts3) == "Rec3"] <- "Rec1"
sum(frectype.counts3$n)

#MERGE tables of ALL recommendations
frectype.all <- bind_rows(frectype.counts, frectype.counts2, frectype.counts3) %>% 
  group_by(value, n) %>% 
  distinct(.keep_all = TRUE)

#resort by category
frectype.all <- as.data.frame(frectype.all)
frectype.all <- frectype.all[order(frectype.all[,2]), ]

#PUSH OUT CSV
write.csv(frectype.all, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FAllrec.count.csv", row.names = FALSE)

#calculate the percent according to topic and recommendation from TOTAL papers
frectype.all
sum(frectype.all$n)
frectype.all$percent<- ((frectype.all$n/287)*100)
frectype.all

#create a table to count the number of "NO" recommendations
table(frecdf['Rec.included'])
fnorec<- data.frame(
  Rec1 = "None",
  value= "No Recommendations",
  n = 45)
fnorec$percent <- ((45/287)*100)
fnorec

#PUSH OUT CSV
write.csv(fnorec, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FNorec.count.csv", row.names = FALSE)

##############################################################                                       
####### SUBSETTING FOR MULTIPLE INDICATORS COUNTS ############
#############################################################

#create data frame to sort # publications with multiple indicators
FIndicators <- forest.meta[,c("Forest_Comp1","Forest_Comp2", "Forest_Comp3", "Forest_Comp4", "Forest_Comp5")]

#create count table including all bird domain columns grouped by RECOMMENDATION TYPE (first only)
#FIRST INDICATOR
fInd1 <- fjournal.rec<- FIndicators %>%
  group_by(Forest_Comp1) %>%
  dplyr::mutate(indicator1.count=n())
fInd1 <- fInd1[!duplicated(fInd1$Forest_Comp1),]
#remove N/A columns
fInd1<- fInd1[!grepl('N/A', fInd1$Forest_Comp1),]
#calculate sum
sum(fInd1$indicator1.count)

#SECOND INDICATOR
fInd2 <- fjournal.rec<- FIndicators %>%
  group_by(Forest_Comp2) %>%
  dplyr::mutate(indicator2.count=n())
fInd2 <- fInd2[!duplicated(fInd2$Forest_Comp2), ]
#drop N/A
fInd2<- fInd2[!grepl('N/A', fInd2$Forest_Comp2),]
#calculate sum
sum(fInd2$indicator2.count)

#THIRD INDICATOR
fInd3 <- fjournal.rec<- FIndicators %>%
  group_by(Forest_Comp3) %>%
  dplyr::mutate(indicator3.count=n())
fInd3 <- fInd3[!duplicated(fInd3$Forest_Comp3), ]
#drop N/A
fInd3<- fInd3[!grepl('N/A', fInd3$Forest_Comp3),]
sum(fInd3$indicator3.count)

fInd4 <- fjournal.rec<- FIndicators %>%
  group_by(Forest_Comp4) %>%
  dplyr::mutate(indicator4.count=n())
fInd4 <- fInd4[!duplicated(fInd4$Forest_Comp4), ]
#drop N/A
fInd4<- fInd4[!grepl('N/A', fInd4$Forest_Comp4),]
sum(fInd4$indicator4.count)

fInd5 <- fjournal.rec<- FIndicators %>%
  group_by(Forest_Comp5) %>%
  dplyr::mutate(indicator5.count=n())
fInd5 <- fInd5[!duplicated(fInd5$Forest_Comp5), ]
#drop N/A
fInd5<- fInd5[!grepl('N/A', fInd5$Forest_Comp5),]
sum(fInd5$indicator5.count)

#create data frame to sort # publications with multiple indicators
number_topics <- c("One Indicator", "Two Indicators", "Three Indicators", "Four Indicators", "Five Indicators")
number_publications <- c("156","98", "41", "12", "3")

#PUSH OUT CSV
fallindicators<- data.frame(number_topics, number_publications)  
write.csv(fallindicators, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FAllind.count.csv", row.names = FALSE)


####################################
####### TOPICS ONLY COUNT ##########
####################################

######### Categories only count ##############
#create dataframes for each indicator/topic and align column names
forestcomp1 <- fInd1[,-c(2:5)]
colnames(forestcomp1)[2] = "indicatorcount"
colnames(forestcomp1)[1] = "forestcomp"
forestcomp2<- fInd2[,-c(1,3:5)]
colnames(forestcomp2)[2] = "indicatorcount"
colnames(forestcomp2)[1] = "forestcomp"
forestcomp3<- fInd3[,-c(1:2,3:4)]
colnames(forestcomp3)[2] = "indicatorcount"
colnames(forestcomp3)[1] = "forestcomp"
forestcomp4<- fInd4[,-c(1:3,5)]
colnames(forestcomp4)[2] = "indicatorcount"
colnames(forestcomp4)[1] = "forestcomp"
forestcomp5<- fInd5[,-c(1:4)]
colnames(forestcomp5)[2] = "indicatorcount"
colnames(forestcomp5)[1] = "forestcomp"

#BIND
allforest<-rbind(forestcomp1, forestcomp2 ,forestcomp3 ,forestcomp4, forestcomp5 )
merge1<- merge(forestcomp1, forestcomp2, by= "forestcomp", all= TRUE)
merge2<- merge(forestcomp3, forestcomp4, by= "forestcomp", all= TRUE)
merge3 <- merge(forestcomp5, forestcomp4, by= "forestcomp", all= TRUE)

merge1$total<- merge1$indicatorcount.x + merge1$indicatorcount.y
merge1<- merge1[,-c(2:3)]
merge1[is.na(merge1)] <- 0

merge2[is.na(merge2)] <- 0
merge2$total<- merge2$indicatorcount.x + merge2$indicatorcount.y
merge2<- merge2[,-c(2:3)]
#remove remaining n/a
merge2<- merge2[!grepl("N/A", merge2$forestcomp),]

#since this includes duplicated fourth comp we must delete it
merge3<- merge3[,-3]
merge3[is.na(merge3)] <- 0
merge3$total<- merge3$indicatorcount.x
merge3<- merge3[,-c(2)]


#combine all for a total across multiple indictors *** will have more articles than total
forestcomponent<- merge(merge1, merge2, by= "forestcomp", all= TRUE)
#create new total column
forestcomponent[is.na(forestcomponent)] <- 0
forestcomponent<- forestcomponent[!grepl("N/A", forestcomponent$forestcomp),]
#create new column of totals
forestcomponent$total<- (forestcomponent$total.x + forestcomponent$total.y)
forestcomponent<- forestcomponent[,-c(2:3)]

forestcomptotal <- merge(forestcomponent, merge3, by= "forestcomp", all= TRUE)
#replace N/s with zero
forestcomptotal[is.na(forestcomptotal)] <- 0
forestcomptotal$total<- (forestcomptotal$total.x + forestcomptotal$total.y)
#remove irrelevant rows
forestcomptotal <- forestcomptotal[, -c(2:3)]

#PUSH OUT CSV
write.csv(forestcomptotal, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/forest.component.csv", row.names = FALSE)


