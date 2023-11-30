#packagesneeded
Packages <- c("tidyverse", "ggplot2", "maps", "bibliometrix", "ggthemes", "cartography", "sf")
lapply(Packages, library,character.only= TRUE)

getwd()
setwd("C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/in")

#loading avian data and formatting
av.data <- read.csv("Meta_Avian_Data_Extraction - Master_Data.csv")
forest.data<- read.csv("META_Forest_Climate_Data_Extraction.csv")

########## CLEANING AND ORGANIZING DATA ###################################################################################

################################################### AVIAN COMPONENT #########################################################

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
#save as output csv
write.csv(av.meta, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/AV.META.csv", row.names = FALSE)

#number of publications per year
year.count <- av.meta %>%
  group_by(Year) %>%
  dplyr::mutate(Year_count= n())

#remove duplicates
year.count <- year.count[!duplicated(year.count$Year), ]
write.csv(year.count, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/Year.count.csv", row.names = FALSE)


#journal name count by study country
country.count <- av.meta %>%
  group_by(COUNTRY, Journal) %>%
  dplyr::mutate(journal.count= n())
write.csv(country.count, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/Country.count.csv", row.names = FALSE)

#journal name country by recommendation included
journal.rec<- av.meta %>%
  group_by(Rec., Journal) %>%
  dplyr::mutate(journal.count=n())
write.csv(journal.rec, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/journal.rec.csv", row.names = FALSE)

#count only by COUNTRY
heatmap.count <- av.meta %>%
  group_by(COUNTRY) %>%
  dplyr::mutate(country.count= n())
write.csv(heatmap.count, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/Heatmap.count.csv", row.names = FALSE)


#create count table including all bird domain columns grouped by URBAN SCALE
urb.counts <-av.meta %>%
  pivot_longer(cols = c(birddomain1:birddomain4)) %>%
  dplyr::count(Urb.scale,value)
write.csv(urb.counts, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/Urb.count.csv", row.names = FALSE)

#create count table including all bird domain columns grouped by COMPARATOR USED Y/N
comp.counts <-av.meta %>%
  pivot_longer(cols = c(birddomain1:birddomain4)) %>%
  dplyr::count(Comparator,value)
write.csv(comp.counts, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/Comp.count.csv", row.names = FALSE)

#subset data for STUDY DURATION 
#clean and organize data
duration.df<- av.meta[,c("birddomain1", "birddomain2", "birddomain3", "birddomain4", "Start.year", "End.year")]

#remove N/A
duration.df<- duration.df[!grepl('N/A', duration.df$Start.year),]
duration.df<-duration.df[!grepl('N/A', duration.df$End.year),]

#ensure columns are numerics
duration.df <- duration.df %>% mutate_at(c('Start.year', 'End.year'), as.numeric)
duration.df <- as.data.frame(duration.df)

#create new column with total duration of study
duration.df$duration <- as.numeric(duration.df$End.year-duration.df$Start.year)

#since single year studies are zeros according to calculation, I will transform column by adding one to all values
duration.df$duration2 <-duration.df[,7]+1

#create BINS for values based on duration column
duration.df<- duration.df %>% mutate(duration_bin = cut(duration2, breaks=c(0, 1, 5, 10, 155)))

#create count table including all bird domain columns grouped by STUDY DURATION BIN
duration.counts <-duration.df %>%
  pivot_longer(cols = c(birddomain1:birddomain4)) %>%
  count(duration_bin,value)

duration.counts2 <- duration.df %>%
                    count(duration_bin)

#remove duplicate N/As from additional columns for plotting
duration.counts <- duration.counts %>% drop_na(duration_bin)
duration.counts <- duration.counts %>% drop_na(value)
duration.counts2 <- duration.counts2 %>% drop_na(n)

write.csv(duration.counts, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/Duration.count.csv", row.names = FALSE)
write.csv(duration.counts2, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/Duration.count2.csv", row.names = FALSE)

################SUBSET FOR RECOMMENDATIONS
recdf<- av.meta[,c("birddomain1","birddomain2", "birddomain3", "birddomain4", "Rec.",
                    "Rec.1", "Rec.2", "Rec.3")]

#create count table including all bird domain columns grouped by RECOMMENDATION TYPE (first only)
rectype.counts <-as.data.frame(recdf %>%
  pivot_longer(cols = c(birddomain1:birddomain4)) %>%
  dplyr::count(Rec.1,value))
rectype.counts <- rectype.counts %>% drop_na(value)
rectype.counts <- rectype.counts[-c(7,12,15:23,28),]


rectype.counts2 <-as.data.frame(recdf %>%
  pivot_longer(cols = c(birddomain1:birddomain4)) %>%
  dplyr::count(Rec.2,value))
#remove empty cells from studies with only 1 recommendation ##CHECK CHECK DOUBLE CHECK
rectype.counts2 <- rectype.counts2[-c(5, 10, 15:24),]
#rename column to rec.1 to merge
names(rectype.counts2)[names(rectype.counts2) == "Rec.2"] <- "Rec.1"
sum(rectype.counts2$n)

rectype.counts3 <-as.data.frame(recdf %>%
  pivot_longer(cols = c(birddomain1:birddomain4)) %>%
  dplyr::count(Rec.3,value))
rectype.counts3 <- rectype.counts3[-c(3:12),]
names(rectype.counts3)[names(rectype.counts3) == "Rec.3"] <- "Rec.1"
sum(rectype.counts3$n)

#MERGE tables
rectype.all <- bind_rows(rectype.counts, rectype.counts2, rectype.counts3) %>% 
  group_by(value, n) %>% 
  distinct(.keep_all = TRUE)

rectype.all
sum(rectype.all$n)
rectype.all$percent<- ((rectype.all$n/115)*100)
rectype.all
write.csv(rectype.all, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/Allrec.count.csv", row.names = FALSE)

#create a table to count the number of "NO" recommendations
table(recdf['Rec.'])
norec<- data.frame(
  Rec.1 = "None",
  value= "None",
  n = 99)
norec$percent <- ((99/274)*100)
norec
write.csv(norec, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/Norec.count.csv", row.names = FALSE)

#MULTIPLE INDICATORS COUNTS
#create data frame to sort # publications with multiple indicators

Indicators <- av.meta[,c("birddomain1","birddomain2", "birddomain3", "birddomain4")]

#create count table including all bird domain columns grouped by RECOMMENDATION TYPE (first only)
Ind1 <- journal.rec<- Indicators %>%
        group_by(birddomain1) %>%
        dplyr::mutate(indicator1.count=n())
Ind1 <- Ind1[!duplicated(Ind1),]
#remove N/A columns
Ind1<- Ind1 %>% drop_na(birddomain1)
#remove remaining irrelevant rows
Ind1 <- Ind1[-c(6,7,9:30),]
sum(Ind1$indicator1.count)

Ind2 <- journal.rec<- Indicators %>%
        group_by(birddomain2) %>%
        dplyr::mutate(indicator2.count=n())
Ind2 <- Ind2[!duplicated(Ind2), ]
Ind2<- Ind2 %>% drop_na(birddomain2)
#remove remaining irrelevant rows
Ind2 <- Ind2[-c(2,6,8:10, 12:24),]
sum(Ind2$indicator2.count)

Ind3 <- journal.rec<- Indicators %>%
        group_by(birddomain3) %>%
        dplyr::mutate(indicator3.count=n())
Ind3 <- Ind3[!duplicated(Ind3), ]
Ind3<- Ind3 %>% drop_na(birddomain3)
Ind3 <- Ind3[-c(4,6,8:11),]
sum(Ind3$indicator3.count)

Ind4 <- journal.rec<- Indicators %>%
        group_by(birddomain4) %>%
        dplyr::mutate(indicator4.count=n())
Ind4 <- Ind4[!duplicated(Ind4), ]
Ind4<- Ind4 %>% drop_na(birddomain4)
sum(Ind4$indicator4.count)

#create data frame to sort # publications with multiple indicators
number_topics <- c("One Indicator", "Two Indicators", "Three Indicators", "Four Indicators")
number_publications <- c("274","60", "11", "2")

allindicators<- data.frame(number_topics, number_publications)  
write.csv(allindicators, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/Allind.count.csv", row.names = FALSE)

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
#save as output csv
write.csv(forest.meta, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FOREST.META.csv", row.names = FALSE)

#number of publications per year
fyear.count <- forest.meta %>%
  group_by(Year) %>%
  dplyr::mutate(Year_count= n())

#remove duplicates
fyear.count <- fyear.count[!duplicated(fyear.count$Year), ]
write.csv(fyear.count, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FYear.count.csv", row.names = FALSE)

#journal name count by study country
fcountry.count <- forest.meta %>%
  group_by(COUNTRY, Journal) %>%
  dplyr::mutate(journal.count= n())
write.csv(fcountry.count, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FCountry.count.csv", row.names = FALSE)

#journal name country by recommendation included
fjournal.rec<- forest.meta %>%
  group_by(Rec.included, Journal) %>%
  dplyr::mutate(journal.count=n())
write.csv(fjournal.rec, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/fjournal.rec.csv", row.names = FALSE)

#count only by COUNTRY
fheatmap.count <- forest.meta %>%
  group_by(COUNTRY) %>%
  dplyr::mutate(country.count= n())
write.csv(fheatmap.count, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FHeatmap.count.csv", row.names = FALSE)


#create count table including all bird domain columns grouped by URBAN SCALE
furb.counts <-forest.meta %>%
  pivot_longer(cols = c(Forest_Comp1:Forest_Comp5)) %>%
  dplyr::count(Urb.scale,value)
write.csv(furb.counts, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FUrb.count.csv", row.names = FALSE)

#create count table including all bird domain columns grouped by COMPARATOR USED Y/N
fcomp.counts <-forest.meta %>%
  pivot_longer(cols = c(Forest_Comp1:Forest_Comp5)) %>%
  dplyr::count(Comparator,value)
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

write.csv(fduration.counts, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FDuration.count.csv", row.names = FALSE)
write.csv(fduration.counts2, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FDuration.count2.csv", row.names = FALSE)


################SUBSET FOR RECOMMENDATIONS
frecdf<- forest.meta[,c("Forest_Comp1","Forest_Comp2", "Forest_Comp3", "Forest_Comp4", "Forest_Comp5", "Rec.included",
                   "Rec1", "Rec2", "Rec3")]

#create count table including all bird domain columns grouped by RECOMMENDATION TYPE (first only)
frectype.counts <-as.data.frame(frecdf %>%
                                 pivot_longer(cols = c(Forest_Comp1:Forest_Comp5)) %>%
                                 dplyr::count(Rec1,value))
frectype.counts <- frectype.counts[-c(9,11:20,25),]


frectype.counts2 <-as.data.frame(frecdf %>%
                                  pivot_longer(cols = c(Forest_Comp1:Forest_Comp5)) %>%
                                  dplyr::count(Rec2,value))

#remove empty cells from studies with only 1 recommendation ##CHECK CHECK DOUBLE CHECK
frectype.counts2 <- frectype.counts2[-c(6, 8:17,24),]
#rename column to rec.1 to merge
names(frectype.counts2)[names(frectype.counts2) == "Rec2"] <- "Rec1"
sum(frectype.counts2$n)

frectype.counts3 <-as.data.frame(frecdf %>%
                                  pivot_longer(cols = c(Forest_Comp1:Forest_Comp5)) %>%
                                  dplyr::count(Rec3,value))
frectype.counts3 <- frectype.counts3[-c(6, 8:17),]
names(frectype.counts3)[names(frectype.counts3) == "Rec3"] <- "Rec1"
sum(frectype.counts3$n)

#MERGE tables
frectype.all <- bind_rows(frectype.counts, frectype.counts2, frectype.counts3) %>% 
  group_by(value, n) %>% 
  distinct(.keep_all = TRUE)

frectype.all
sum(frectype.all$n)
frectype.all$percent<- ((frectype.all$n/248)*100)
frectype.all
write.csv(frectype.all, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FAllrec.count.csv", row.names = FALSE)

#create a table to count the number of "NO" recommendations
table(frecdf['Rec.included'])
fnorec<- data.frame(
  Rec1 = "None",
  value= "None",
  n = 45)
fnorec$percent <- ((45/170)*100)
fnorec
write.csv(fnorec, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FNorec.count.csv", row.names = FALSE)

#MULTIPLE INDICATORS COUNTS
#create data frame to sort # publications with multiple indicators

FIndicators <- forest.meta[,c("Forest_Comp1","Forest_Comp2", "Forest_Comp3", "Forest_Comp4", "Forest_Comp5")]

#create count table including all bird domain columns grouped by RECOMMENDATION TYPE (first only)
fInd1 <- fjournal.rec<- FIndicators %>%
  group_by(Forest_Comp1) %>%
  dplyr::mutate(indicator1.count=n())
fInd1 <- fInd1[!duplicated(fInd1),]
#remove N/A columns
fInd1<- fInd1 %>% drop_na(Forest_Comp1)
#remove remaining irrelevant rows
fInd1 <- fInd1[-c(2,3,5,11:28,30:52),]
sum(fInd1$indicator1.count)

fInd2 <- fjournal.rec<- FIndicators %>%
  group_by(Forest_Comp2) %>%
  dplyr::mutate(indicator2.count=n())
fInd2 <- fInd2[!duplicated(fInd2), ]
fInd2<- fInd2 %>% drop_na(Forest_Comp2)
#remove remaining irrelevant rows
fInd2 <- fInd2[-c(1,4,6:12,14,16:24,26:52),]
sum(fInd2$indicator2.count)

fInd3 <- fjournal.rec<- FIndicators %>%
  group_by(Forest_Comp3) %>%
  dplyr::mutate(indicator3.count=n())
fInd3 <- fInd3[!duplicated(fInd3), ]
fInd3<- fInd3 %>% drop_na(Forest_Comp3)
fInd3 <- fInd3[-c(1,3:8,11:19, 22:23, 25:27,29:51),]
sum(fInd3$indicator3.count)

fInd4 <- fjournal.rec<- FIndicators %>%
  group_by(Forest_Comp4) %>%
  dplyr::mutate(indicator4.count=n())
fInd4 <- fInd4[!duplicated(fInd4), ]
fInd4<- fInd4 %>% drop_na(Forest_Comp4)
fInd4 <- fInd4[-c(1:9, 11,12, 14:16, 18:45, 46:52),]
sum(fInd4$indicator4.count)

fInd5 <- fjournal.rec<- FIndicators %>%
  group_by(Forest_Comp5) %>%
  dplyr::mutate(indicator5.count=n())
fInd5 <- fInd5[!duplicated(fInd5), ]
fInd5<- fInd5 %>% drop_na(Forest_Comp5)
fInd5 <- fInd5[-c(1:16, 18:21, 23:34, 36:52),]
sum(fInd5$indicator5.count)

#create data frame to sort # publications with multiple indicators
number_topics <- c("One Indicator", "Two Indicators", "Three Indicators", "Four Indicators", "Five Indicators")
number_publications <- c("156","67", "41", "11", "3")

fallindicators<- data.frame(number_topics, number_publications)  
write.csv(fallindicators, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FAllind.count.csv", row.names = FALSE)
