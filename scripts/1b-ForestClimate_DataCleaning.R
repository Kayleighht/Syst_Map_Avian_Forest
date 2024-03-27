#packagesneeded
source('scripts/0-packages.R')

#loading avian data and formatting
forest.data<- read.csv("in/Metadata_ForestComponent.csv")

########################################################################################################################################
########################################### FOREST COMPONENT ########################################################################
###########################################################################################################################################

#cut down to only necessary columns for figures (so far..)
forest.meta <- forest.data[ ,c("Title","Year", "Journal", "Publication.Type.", "Country.of.First.Author",
                               "Study.Country", "Urb.scale", "Year.start", "Year.end", "Comparator", 
                               "Forest.comp", "Rec.included","Rec1", "Rec2", "Rec3", "Carbon.metric")]

#separate forest component column into multiple columns with one indicator each
forest.meta <- separate_wider_delim(forest.meta, cols = Forest.comp, delim = ",", names = c("forestcomp1", "forestcomp2", "forestcomp3", "forestcomp4",
                                                                                            "forestcomp5"),
                                    too_few = "align_start", too_many = "debug")

forest.meta <- separate_wider_delim(forest.meta, cols = Carbon.metric, delim = ",", names = c("Carb1", "Carb2", "Carb3", "Carb4"),
                               too_few = "align_start")

#remove white space (leading and trailing zeros)
forest.meta<- forest.meta %>% 
  mutate(across(where(is.character), str_trim))

#rename column in av.meta to match shape files with countries
names(forest.meta)[names(forest.meta) == "Country.of.First.Author"] <- "COUNTRY"

# Cleaning ----------------------------------------------------------------


#replace empty cells with N/A
forest.meta <- replace(forest.meta, forest.meta=='', NA) 

forest.meta$Carb1[forest.meta$Carb1 == 'Infrastructure (timber buildings)'] <- 'Infrastructure'
forest.meta$Carb2[forest.meta$Carb2 == 'Infrastructure (timber buildings)'] <- 'Infrastructure'
forest.meta$Carb3[forest.meta$Carb3 == 'Infrastructure (timber buildings)'] <- 'Infrastructure'
forest.meta$Carb4[forest.meta$Carb4 == 'Infrastructure (timber buildings)'] <- 'Infrastructure'

unique(forest.meta$COUNTRY)

forest.meta$COUNTRY[forest.meta$COUNTRY == 'United States'] <- 'USA'
forest.meta$COUNTRY[forest.meta$COUNTRY == 'United State of America'] <- 'USA'
forest.meta$COUNTRY[forest.meta$COUNTRY == 'United States of America'] <- 'USA'
forest.meta$COUNTRY[forest.meta$COUNTRY == 'Korea'] <- 'South Korea'
forest.meta$COUNTRY[forest.meta$COUNTRY == 'Helsinki'] <- 'Finland'
forest.meta$COUNTRY[forest.meta$COUNTRY == 'Republic of Korea'] <- 'South Korea'

unique(forest.meta$Journal)
forest.meta$Journal[forest.meta$Journal == 'Forests Multidisciplinary Digital Publishing Institute'] <- 'Forests'
forest.meta$Journal[forest.meta$Journal == 'Forests Multidisciplinary Digital Publishing'] <- 'Forests'
forest.meta$Journal[forest.meta$Journal == 'Multidisciplinary Digital Publishing Institute'] <- 'Forests'
forest.meta$Journal[forest.meta$Journal == 'Sustainability Multidisciplinary Digital Publishing Institute'] <- 'Sustainability'
forest.meta$Journal[forest.meta$Journal == 'Susainability'] <- 'Sustainability'
forest.meta$Journal[forest.meta$Journal == 'Urban Foresty & Urban Greening'] <- 'Urban Forestry & Urban Greening'

#cut down to only necessary columns for figures (so far..)
for.meta <- forest.meta[,c(3:5)]


# Journal and Country -----------------------------------------------------

country.count <- forest.meta %>%
  group_by(COUNTRY, Journal) %>%
  dplyr::mutate(journal.count= n())

#raw ncounts per country
authcountry <- forest.meta %>%
  group_by(COUNTRY) %>%
  dplyr::mutate(author.count= n())

#calculate average publication count from 2012 to 2022
authoravg<- authcountry[c("COUNTRY", "author.count")]
authoravg <- authoravg[order(-authoravg$author.count),]
#cut to relevant years
authoravg$percent <- (authoravg$author.count/169)*100
authoravg

country.count <- country.count %>%
  group_by(Journal) %>%
  dplyr::mutate(count= n())

#subset columns needed
journal.df <- country.count[,c("COUNTRY", "Journal","count","journal.count")]
#sort column descending order
journal.df <- arrange(journal.df, -count)
#remove duplicates
journal.df <- journal.df[!duplicated(journal.df), ]

#select only top ten
journal.df <- journal.df[1:47,]
journal.df$COUNTRY[journal.df$journal.count <= 1] <- "Other"
journal.df$COUNTRY[journal.df$COUNTRY == 'United States of America'] <- 'USA'

#PUSH OUT CSV
write.csv(journal.df, "out/journal.df.csv", row.names = FALSE)

# Bubblemaps Prep ---------------------------------------------------------


latlong<- read.csv("in/countries1.csv")
colnames(latlong)[4] ="COUNTRY"
latlong <- latlong[,c(2:4)]

latlong$COUNTRY[latlong$COUNTRY == 'United States'] <- 'USA'

#cut down to only necessary columns for bubblemap (so far..)
for.meta <- forest.meta[,c(1,2,6)]

#rename column in av.meta to match shape files with countries
names(for.meta)[names(for.meta) == "Study.Country"] <- "COUNTRY"

#replace empty cells with N/A
for.meta <- replace(for.meta, for.meta=='', NA) 

unique(for.meta$COUNTRY)

for.meta$COUNTRY[for.meta$COUNTRY == 'United States'] <- 'USA'
for.meta$COUNTRY[for.meta$COUNTRY == 'United State of America'] <- 'USA'
for.meta$COUNTRY[for.meta$COUNTRY == 'United States of America'] <- 'USA'
for.meta$COUNTRY[for.meta$COUNTRY == 'Spain and France'] <- 'Multiple'
for.meta$COUNTRY[for.meta$COUNTRY == 'Korea'] <- 'South Korea'
for.meta$COUNTRY[for.meta$COUNTRY == 'Almada'] <- 'Portugal'
for.meta$COUNTRY[for.meta$COUNTRY == 'Republic of Korea'] <- 'South Korea'
#for.meta$COUNTRY[for.meta$COUNTRY == 'France '] <- 'France'

meta_for<-for.meta %>%
  group_by(COUNTRY) %>%
  dplyr::mutate(count= n())


# merge two data frames by ID
merged_for <- merge(meta_for,latlong,by="COUNTRY", all.x=TRUE)
merged_for<-merged_for %>% select(1,4:6)
merged_for<-merged_for %>% distinct(COUNTRY, .keep_all = TRUE)

data1 <- arrange(merged_for, -count)
write.csv(data1, "out/map_data_for.csv")

data1$latitude[data1$latitude == "23.82408"] <- "25.562583"

data1$latitude[data1$latitude == '24.24902'] <- '25.24902'	

data1$latitude <- as.numeric(data1$latitude)

#write out CSV for bubblemap figure
write.csv(data1, "out/data1.csv", row.names = FALSE)


#subset only columns needed
forestyear<- subset(forest.meta, select = c(Year, Journal))

# Study by Year Counts ----------------------------------------------------


#number of publications per year
fyear.count <- forestyear %>%
  group_by(Year) %>%
  dplyr::mutate(Year_count= n())

#sort years for results section
yearscolumn <- sort(fyear.count$Year)

#remove duplicates
forestyearcount <- fyear.count[!duplicated(fyear.count$Year), ]

#PUSH OUT CSV
write.csv(forestyearcount, "out/FYear.count.csv", row.names = FALSE)

#calculate average publication count from 2012 to 2022
year.average<- fyear.count[c("Year", "Year_count")]
year.average <- year.average[order(year.average$Year),]
#cut to relevant years
year.average<- year.average[c(12:22),]
mean(year.average$Year_count)

# Study Duration ----------------------------------------------------------


#subset data for STUDY DURATION 
#clean and organize data
fduration.df<- forest.meta[,c("forestcomp1", "forestcomp2", "forestcomp3", "forestcomp4",
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
write.csv(fduration.counts, "out/Studydurationtopic.csv", row.names = FALSE)

write.csv(fduration.counts2, "out/Studydurationall.csv", row.names = FALSE)


# Carbon Metrics ----------------------------------------------------------

#COUNT TABLE
#carbon metrics considered
#subset columns needed

carb1<- forest.meta %>%
  dplyr:: count(Carb1)
sum(carb1$n)
carb1

carb2<- forest.meta %>%
  dplyr:: count(Carb2)
carb2<- carb2 %>% filter(row_number() <= n()-1)

carb3<- forest.meta %>%
  dplyr:: count(Carb3)
carb3<- carb3 %>% filter(row_number() <= n()-1)

carb4<- forest.meta %>%
  dplyr:: count(Carb4)

#remove columns not needed
carbon1<- subset(forest.meta, select = c("Carb1"))
colnames(carbon1) <- c("carbon")

#remove columns not needed
carbon2<- subset(forest.meta, select = c("Carb2"))
carbon2<- carbon2 %>% drop_na(Carb2)
colnames(carbon2) <- c("carbon")

#remove columns not needed
carbon3<- subset(forest.meta, select = c("Carb3"))
carbon3<- carbon3 %>% drop_na(Carb3)
colnames(carbon3) <- c("carbon")

#remove columns not needed
carbon4<- subset(forest.meta, select = c("Carb4"))
carbon4<- carbon4 %>% drop_na(Carb4)
colnames(carbon4) <- c("carbon")

##MERGING
dfmerge12 <- rbind(carbon1, carbon2) 
dfmerge34<- rbind(dfmerge12, carbon3)
dfmergefinal <- rbind(dfmerge34, carbon4)

dfmergefinal <- dfmergefinal %>%
  mutate_if(is.character, str_trim)


forestcarbon <- dfmergefinal %>%
  dplyr:: count(carbon)
forestcomps <- as.data.frame(forestcomps)

#unique(forestcomps$carbon)

forestcarbon$percent <- ((forestcarbon$n/169)*100)
forestcarbon

#PUSH OUT CSV
write.csv(forestcarbon, "out/forest.carbon.csv", row.names = FALSE)

# Journal -----------------------------------------------------------------


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
write.csv(journalandrec, "out/fjournal.rec.csv", row.names = FALSE)


#COUNT TABLE
#create count table including all forest domain columns grouped by URBAN SCALE
furb.counts <-forest.meta %>%
              pivot_longer(cols = c(forestcomp1:forestcomp5)) %>%
              dplyr::count(Urb.scale,value)

#COUNT TABLE
#create count table including all forest domain columns grouped by URBAN SCALE
forest.meta <- forest.meta %>% 
  mutate(across(where(is.character), str_trim))

carburb.counts <-forest.meta %>%
  pivot_longer(cols = c(Carb1:Carb4)) %>%
  dplyr::count(Urb.scale,value)
carburb.counts <- as.data.frame(carburb.counts)

# Urban Scale -------------------------------------------------------------


#non-categorized urban count
urbanallcount<- forest.meta %>%
                dplyr:: count(Urb.scale)

urbanallcount
#PUSH OUT CSV
write.csv(furb.counts, "out/FUrb.count.csv", row.names = FALSE)
write.csv(carburb.counts, "out/CarbUrb.count.csv", row.names = FALSE)

# Comparator --------------------------------------------------------------

#COUNT TABLE
#create count table including all bird domain columns grouped by COMPARATOR USED Y/N
#trim all leading or trailing spaces so that values match
ForestComps <- forest.meta %>%
  mutate_if(is.character, str_trim)

#raw comparator count ignored forest components
fcompraw <-ForestComps %>%
  dplyr::count(Comparator)


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
write.csv(Compmeta, "out/FComp.count.csv", row.names = FALSE)

## FOR CARBON METRICS ########################################################################

#create count table including all bird domain columns grouped by COMPARATOR USED Y/N
#trim all leading or trailing spaces so that values match
ForestComps <- forest.meta %>%
  mutate_if(is.character, str_trim)

fcomp.counts <-ForestComps %>%
  pivot_longer(cols = c(Carb1:Carb4)) %>%
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
#Comp<- Comp %>% spread(Comparator, n)

#create column of totals
Comp$total <- c(164,164,58,58,15,15,3,3,30,30)
#create percent yes column
Comp$percent <- (Comp$n/Comp$total)*100
sum(Comp$total)
#sort
Compmeta <- Comp[order(Comp[,2]), ]

Compmeta

#PUSH OUT CSV
write.csv(Compmeta, "out/CarbComp.count.csv", row.names = FALSE)

# Forest Components -------------------------------------------------------

#remove columns not needed
Comp1<- subset(forest.meta, select = c("forestcomp1"))
colnames(Comp1) <- c("Component")

Comp2 <- forest.meta[grep("2", ForestComps$Forest.comp_pieces), ]
#remove columns not needed
Comp2<- subset(Comp2, select = c("forestcomp2"))
colnames(Comp2) <- c("Component")

Comp3 <- forest.meta[grep("3", ForestComps$Forest.comp_pieces), ]
#remove columns not needed
Comp3<- subset(Comp3, select = c("forestcomp3"))
colnames(Comp3) <- c("Component")
Comp3 <- as.data.frame(Comp3)

Comp4 <- forest.meta[grep("4", ForestComps$Forest.comp_pieces), ]
Comp4<- subset(Comp4, select = c("forestcomp4"))
colnames(Comp4) <- c("Component")

Comp5 <- forest.meta[grep("5", ForestComps$Forest.comp_pieces), ]
Comp5<- subset(Comp5, select = c("forestcomp5"))
colnames(Comp5) <- c("Component")

##MERGING
dfmerge12 <- rbind(Comp1, Comp2) 
dfmerge34<- rbind(dfmerge12, Comp3)
dfmerge56 <- rbind(dfmerge34, Comp4)
dfmergefinal<- rbind(dfmerge56,Comp5)

dfmergefinal <- dfmergefinal %>%
  mutate_if(is.character, str_trim)
dfmergefinal$Component <- gsub("composition", "Composition", dfmergefinal$Component)

forestcomps <- dfmergefinal %>%
  dplyr:: count(Component)
forestcomps <- as.data.frame(forestcomps)

#unique(forestcomps$Component)

forestcomps$percent <- ((forestcomps$n/169)*100)
forestcomps

#PUSH OUT CSV
write.csv(forestcomps, "out/forest.component.csv", row.names = FALSE)


# Subsetting Recommendations ----------------------------------------------

#open forest comps that we created already with forest components divided into separate columns
forest.meta

#clean dataframe of whitespace
ForestComps <- forest.meta %>% 
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
write.csv(frectype.all, "out/FAllrec.count.csv", row.names = FALSE)

#calculate the percent according to topic and recommendation from TOTAL papers
frectype.all
sum(frectype.all$n)
frectype.all$percent<- ((frectype.all$n/170)*100)
frectype.all

#create a table to count the number of "NO" recommendations for ALL PAPERS
#table(frecdf['Rec.included'])
fnorec<- data.frame(
  Rec1 = "None",
  value= "No Recommendations",
  n = 45)
fnorec$percent <- ((45/170)*100)
fnorec

#PUSH OUT CSV
write.csv(fnorec, "out/FNorec.count.csv", row.names = FALSE)

#COUNT TABLE
#create count table including all bird domain columns grouped by RECOMMENDATION TYPE (first only)

#REC 1
carbrec.counts <-as.data.frame(Forestrec %>%
                                  pivot_longer(cols = c(Carb1:Carb4)) %>%
                                  dplyr::count(Rec1,value))
#remove NAs
carbrec.counts <- carbrec.counts[!grepl("N/A", frectype.counts$Rec1),]
carbrec.counts<- carbrec.counts %>% drop_na(value)
sum(frectype.counts$n)

#REC 2
carbrec.counts2 <-as.data.frame(Forestrec %>%
                                   pivot_longer(cols = c(Carb1:Carb4)) %>%
                                   dplyr::count(Rec2,value))

#remove empty cells from studies with only 1 recommendation ##CHECK CHECK DOUBLE CHECK
carbrec.counts2 <- carbrec.counts2[!grepl("N/A", frectype.counts2$Rec2),]
#remove NA values in value column
carbrec.counts2 <- carbrec.counts2 %>% drop_na(value)

#rename column to rec.1 to merge
names(carbrec.counts2)[names(carbrec.counts2) == "Rec2"] <- "Rec1"
sum(carbrec.counts2$n)
unique(carbrec.counts2$value)

#REC 3
carbrec.counts3 <-as.data.frame(Forestrec %>%
                                   pivot_longer(cols = c(Carb1:Carb4)) %>%
                                   dplyr::count(Rec3,value))
carbrec.counts3 <- carbrec.counts3[!grepl("N/A", frectype.counts3$Rec3),]
carbrec.counts3<- carbrec.counts3 %>% drop_na(value)

names(carbrec.counts3)[names(carbrec.counts3) == "Rec3"] <- "Rec1"
sum(carbrec.counts3$n)

#MERGE tables of ALL recommendations
carbrec.all <- bind_rows(carbrec.counts, carbrec.counts2, carbrec.counts3) %>% 
  group_by(value, n) %>% 
  distinct(.keep_all = TRUE)

#resort by category
carbrec.all <- as.data.frame(carbrec.all)
carbrec.all <- carbrec.all[order(carbrec.all[,2]), ]
unique(carbrec.all$value)
unique(carbrec.all$Rec1)

#PUSH OUT CSV
write.csv(carbrec.all, "out/CarbAllrec.count.csv", row.names = FALSE)


#MULTIPLE INDICATORS COUNTS
#create data frame to sort # publications with multiple indicators

FIndicators <- forest.meta[,c("forestcomp1", "forestcomp2", "forestcomp3", "forestcomp4",
                              "forestcomp5")]

#create count table including all bird domain columns grouped by RECOMMENDATION TYPE (first only)
fInd1 <- fjournal.rec<- FIndicators %>%
  group_by(forestcomp1) %>%
  dplyr::mutate(indicator1.count=n())
fInd1 <- fInd1[!duplicated(fInd1),]
#remove N/A columns
fInd1<- fInd1 %>% drop_na(forestcomp1)
#remove remaining irrelevant rows
fInd1 <- fInd1[-c(2,3,5,11:28,30:52),]
sum(fInd1$indicator1.count)

fInd2 <- fjournal.rec<- FIndicators %>%
  group_by(forestcomp2) %>%
  dplyr::mutate(indicator2.count=n())
fInd2 <- fInd2[!duplicated(fInd2), ]
fInd2<- fInd2 %>% drop_na(forestcomp2)
#remove remaining irrelevant rows
fInd2 <- fInd2[-c(1,4,6:12,14,16:24,26:52),]
sum(fInd2$indicator2.count)

fInd3 <- fjournal.rec<- FIndicators %>%
  group_by(forestcomp3) %>%
  dplyr::mutate(indicator3.count=n())
fInd3 <- fInd3[!duplicated(fInd3), ]
fInd3<- fInd3 %>% drop_na(forestcomp3)
fInd3 <- fInd3[-c(1,3:8,11:19, 22:23, 25:27,29:51),]
sum(fInd3$indicator3.count)

fInd4 <- fjournal.rec<- FIndicators %>%
  group_by(forestcomp4) %>%
  dplyr::mutate(indicator4.count=n())
fInd4 <- fInd4[!duplicated(fInd4), ]
fInd4<- fInd4 %>% drop_na(forestcomp4)
fInd4 <- fInd4[-c(1:9, 11,12, 14:16, 18:45, 46:52),]
sum(fInd4$indicator4.count)

fInd5 <- fjournal.rec<- FIndicators %>%
  group_by(forestcomp5) %>%
  dplyr::mutate(indicator5.count=n())
fInd5 <- fInd5[!duplicated(fInd5), ]
fInd5<- fInd5 %>% drop_na(forestcomp5)
fInd5 <- fInd5[-c(1:16, 18:21, 23:34, 36:52),]
sum(fInd5$indicator5.count)

#create data frame to sort # publications with multiple indicators
number_topics <- c("One Indicator", "Two Indicators", "Three Indicators", "Four Indicators", "Five Indicators")
number_publications <- c("156","67", "41", "11", "3")

fallindicators<- data.frame(number_topics, number_publications)  
write.csv(fallindicators, "out/FAllind.count.csv", row.names = FALSE)

