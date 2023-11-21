#packagesneeded
Packages <- c("tidyverse", "ggplot2", "maps", "bibliometrix", "ggthemes", "cartography", "sf")
lapply(Packages, library,character.only= TRUE)

getwd()
setwd("C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/in")

#loading avian data and formatting
forest.data<- read.csv("Metadata_ForestComponent.csv")

########################################################################################################################################
########################################### FOREST COMPONENT ########################################################################
###########################################################################################################################################

#cut down to only necessary columns for figures (so far..)
forest.meta <- forest.data[,c("Year", "Journal", "Publication.Type.", "Study.Country", "Urb.scale", 
                      "Year.start", "Year.end", "Comparator", "Forest.comp", "Rec.included",                    
                      "Rec1", "Rec2", "Rec3")]

ForestComps <- separate_wider_delim(forest.meta, cols = Forest.comp, delim = ",", names = c("forestcomp1", "forestcomp2", "forestcomp3", "forestcomp4",
                                                                                            "forestcomp5"),
                                    too_few = "align_start", too_many = "debug")


#rename column in av.meta to match shape files with countries
names(forest.meta)[names(forest.meta) == "Study.Country"] <- "COUNTRY"

#replace empty cells with N/A
forest.meta <- replace(forest.meta, forest.meta=='United States of America', "USA")
print(forest.meta)

#PUSH OUT CSV
write.csv(forest.meta, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FOREST.META.csv", row.names = FALSE)

###### STUDY BY YEAR COUNTS ################################################################################
#subset necessary columns
#subset only columns needed
forestyear<- subset(forest.meta, select = c(Year, Journal))

#number of publications per year
fyear.count <- forestyear %>%
  group_by(Year) %>%
  dplyr::mutate(Year_count= n())

#sort years for results section
yearscolumn <- sort(fyear.count$Year)

#remove duplicates
forestyearcount <- fyear.count[!duplicated(fyear.count$Year), ]

#PUSH OUT CSV
write.csv(forestyearcount, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FYear.count.csv", row.names = FALSE)

##### CARBON METRICS ##########################################################################################

#COUNT TABLE
#carbon metrics considered
#subset columns needed
carbon<- as.data.frame(forest.data[,c("Carbon.metric")])
colnames(carbon) <- "Carbon.metric"
carbon <- separate_wider_delim(carbon, cols = Carbon.metric, delim = ",", names = c("Carb1", "Carb2", "Carb3", "Carb4"),
                     too_few = "align_start")

carb1<- carbon %>%
  dplyr:: count(Carb1)
sum(carb1$n)

carb2<- carbon %>%
  dplyr:: count(Carb2)
carb2<- carb2 %>% filter(row_number() <= n()-1)

carb3<- carbon %>%
  dplyr:: count(Carb3)
carb3<- carb3 %>% filter(row_number() <= n()-1)

carb4<- carbon %>%
  dplyr:: count(Carb4)

#bind all together
cbind(carb1, carb2, carb3, carb4)

###### COUNTRY #######################################################################

#subset neccesary columns
df<- subset(forest.meta, select = c(COUNTRY, Journal))

#COUNT TABLE
#journal name count by study country
forestcountrycount <- df %>%
  group_by(COUNTRY, Journal) %>%
  dplyr::mutate(journal.count= n())

#PUSH OUT CSV
write.csv(forestcountrycount, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest
                               /Syst_Map_Avian_Forest/out/FCountry.count.csv", row.names = FALSE)

#COUNT TABLE
#count only by COUNTRY

#subset 
df<- subset(forest.meta, select = COUNTRY)

countrycount <- df %>%
  group_by(COUNTRY) %>%
  dplyr::mutate(country.count= n())

#PUSH OUT CSV
write.csv(countrycount, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest
                        /Syst_Map_Avian_Forest/out/FHeatmap.count.csv", row.names = FALSE)

#### RECOMMENDATIONS ###############################################################################

#COUNT TABLE
#journal name country by recommendation included

#subset columns needed
df<- subset(forest.meta, select = c(Rec.included, Journal))

journalandrec<- df %>%
  group_by(Rec.included, Journal) %>%
  dplyr::mutate(journal.count=n())

#get total count on journals
journal.count<- journalandrec[,c("Journal", "journal.count")]
journal.count<- journal.count %>% distinct(Journal, .keep_all = TRUE)

#PUSH OUT CSV
write.csv(journalandrec, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest
                          /Syst_Map_Avian_Forest/out/fjournal.rec.csv", row.names = FALSE)

#COUNT TABLE
#create count table including all forest domain columns grouped by URBAN SCALE
furb.counts <-ForestComps %>%
              pivot_longer(cols = c(forestcomp1:forestcomp5)) %>%
              dplyr::count(Urb.scale,value)

#non-categorized urban count
urbanallcount<- forest.meta %>%
                dplyr:: count(Urb.scale)

#PUSH OUT CSV
write.csv(furb.counts, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FUrb.count.csv", row.names = FALSE)

###### COMPARATOR ############################################################################################################################

#COUNT TABLE
#create count table including all bird domain columns grouped by COMPARATOR USED Y/N
#trim all leading or trailing spaces so that values match
ForestComps <- ForestComps %>%
  mutate_if(is.character, str_trim)

fcomp.counts <-ForestComps %>%
  pivot_longer(cols = c(forestcomp1:forestcomp5)) %>%
  dplyr::count(Comparator,value)

#convert to dataframe
Comp<- as.data.frame(fcomp.counts)

#remove n/a's created from empty rows
Comp <-Comp %>% drop_na(value)
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
compyes
compno<- subset(Comp, select = c(value, nopercent, N))
colnames(compno)[2] = "comparator"
colnames(compno)[3] = "yes/no"
compno

Compmeta<- rbind(compyes, compno)
#sort
Compmeta <- Compmeta[order(Compmeta[,1]), ]

#add zero values to Comp2 for binding
Comp2[nrow(Comp2) + 1,] <- c("Y", "Fragmentation", 0)
Comp2[nrow(Comp2) + 1,] <- c("Y", "Connectivity", 0)
#resort
Comp2 <- Comp2[order(Comp2[,2], Comp2[,3]), ]
Compmeta$comp <- Comp2$Comparator

#swap Y/N because not sorted properly
Compmeta$comp[Compmeta$comp=="N"]<-"Yes"
Compmeta$comp[Compmeta$comp=="Y"]<-"No"

Compmeta

#PUSH OUT CSV
write.csv(Compmeta, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FComp.count.csv", row.names = FALSE)

########### STUDY DURATION ############################################################################################################

#subset data for STUDY DURATION 
#clean and organize data
fduration.df<- ForestComps[,c("forestcomp1", "forestcomp2", "forestcomp3", "forestcomp4",
                             "forestcomp5", "Year.start", "Year.end")]

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
  pivot_longer(cols = c(forestcomp1:forestcomp5)) %>%
  count(duration_bin,value)

fduration.counts2 <- fduration.df %>%
  count(duration_bin)

#remove duplicate N/As from additional columns for plotting
fduration.counts <- fduration.counts %>% drop_na(duration_bin)
fduration.counts <- fduration.counts %>% drop_na(value)
#total (not according to topic)
fduration.counts2 <- fduration.counts2 %>% drop_na(duration_bin)

#PUSH OUT CSV
write.csv(fduration.counts, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/Studydurationtopic.csv", row.names = FALSE)
write.csv(fduration.counts2, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/Studydurationall.csv", row.names = FALSE)

############## COMPARATOR ####################################################################################################################

#search for matching # of notes to subset and then recombine (2,3,4 etc)
Comp1 <- ForestComps[grep("1", ForestComps$Forest.comp_pieces), ]
#remove columns not needed
Comp1<- subset(Comp1, select = c("forestcomp1"))
colnames(Comp1) <- c("Component")

Comp2 <- ForestComps[grep("2", ForestComps$Forest.comp_pieces), ]
#remove columns not needed
Comp2<- subset(Comp2, select = c("forestcomp2"))
colnames(Comp2) <- c("Component")

Comp3 <- ForestComps[grep("3", ForestComps$Forest.comp_pieces), ]
#remove columns not needed
Comp3<- subset(Comp3, select = c("forestcomp3"))
colnames(Comp3) <- c("Component")

Comp4 <- ForestComps[grep("4", ForestComps$Forest.comp_pieces), ]
Comp4<- subset(Comp4, select = c("forestcomp4"))
colnames(Comp4) <- c("Component")

Comp5 <- ForestComps[grep("5", ForestComps$Forest.comp_pieces), ]
Comp5<- subset(Comp5, select = c("forestcomp5"))
colnames(Comp5) <- c("Component")

##MERGING
dfmerge12 <- merge(Comp1, Comp2, by=c("Component"),all.x=TRUE, all.y = TRUE) 
dfmerge34 <- merge(dfmerge12, Comp3, by=c("Component"),all.x=TRUE, all.y = TRUE)
dfmerge56 <- merge(dfmerge34,Comp4, by=c("Component"),all.x=TRUE, all.y = TRUE)
dfmergefinal<- merge(dfmerge56,Comp5, by=c("Component"),all.x=TRUE, all.y = TRUE)

dfmergefinal <- dfmergefinal %>%
  mutate_if(is.character, str_trim)
dfmergefinal$Component <- gsub("composition", "Composition", dfmergefinal$Component)

forestcomps <- dfmergefinal %>%
  dplyr:: count(Component)
forestcomps <- as.data.frame(forestcomps)

#unique(forestcomps$Component)

forestcomps$percent <- ((forestcomps$n/170)*100)
forestcomps

#PUSH OUT CSV
write.csv(forestcomps, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest
                       /Syst_Map_Avian_Forest/out/forest.component.csv", row.names = FALSE)

######## SUBSETTING RECOMMENDATIONS #########################################################################################################

################SUBSET FOR RECOMMENDATIONS

#open forest comps that we created already with forest components divided into separate columns
ForestComps

#clean dataframe of whitespace
ForestComps <- ForesrComps %>% 
  mutate(across(where(is.character), str_trim))

#replace N/As with not recommendation in Rec1 column to incorporate "NO" recommendations
Forestrec <- ForestComps %>% 
  mutate(across('Rec1', str_replace, 'N/A', 'No Recommendations'))

#raw table for results section
Forestrec

rec.1count<- Forestrec %>%
  dplyr:: count(Rec1)
sum(rec.1count$n)

rec2.count<- Forestrec %>%
  dplyr:: count(Rec2)

rec3.count<- Forestrec %>%
  dplyr:: count(Rec3)

cbind(rec.1count, rec2.count, rec3.count)

#COUNT TABLE
#create count table including all bird domain columns grouped by RECOMMENDATION TYPE (first only)

#REC 1
frectype.counts <-as.data.frame(Forestrec %>%
                                 pivot_longer(cols = c(forestcomp1:forestcomp5)) %>%
                                 dplyr::count(Rec1,value))
#remove NAs
frectype.counts <- frectype.counts[!grepl("N/A", frectype.counts$Rec1),]
frectype.counts<- frectype.counts %>% drop_na(value)
sum(frectype.counts$n)

#REC 2
frectype.counts2 <-as.data.frame(Forestrec %>%
                                  pivot_longer(cols = c(forestcomp1:forestcomp5)) %>%
                                  dplyr::count(Rec2,value))

#remove empty cells from studies with only 1 recommendation ##CHECK CHECK DOUBLE CHECK
frectype.counts2 <- frectype.counts2[!grepl("N/A", frectype.counts2$Rec2),]
#remove NA values in value column
frectype.counts2<- frectype.counts2 %>% drop_na(value)

#rename column to rec.1 to merge
names(frectype.counts2)[names(frectype.counts2) == "Rec2"] <- "Rec1"
sum(frectype.counts2$n)

#REC 3
frectype.counts3 <-as.data.frame(Forestrec %>%
                                  pivot_longer(cols = c(forestcomp1:forestcomp5)) %>%
                                  dplyr::count(Rec3,value))
frectype.counts3 <- frectype.counts3[!grepl("N/A", frectype.counts3$Rec3),]
frectype.counts3<- frectype.counts3 %>% drop_na(value)

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
frectype.all$percent<- ((frectype.all$n/170)*100)
frectype.all

#create a table to count the number of "NO" recommendations for ALL PAPERS
table(frecdf['Rec.included'])
fnorec<- data.frame(
  Rec1 = "None",
  value= "No Recommendations",
  n = 45)
fnorec$percent <- ((45/170)*100)
fnorec

rbind(fnorec, frectype.all)

#PUSH OUT CSV
write.csv(fnorec, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FNorec.count.csv", row.names = FALSE)


