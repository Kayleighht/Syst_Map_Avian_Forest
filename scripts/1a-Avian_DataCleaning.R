#packagesneeded
source('scripts/0-packages.R')

#loading avian data and formatting
av.data <- read.csv("in/Metadata_Avian_Component.csv")

#spread bird topic into separate columns
av.data <- separate_wider_delim(av.data, cols = Bird.domainraw, delim = ",", names = c("birdcomp1", "birdcomp2", "birdcomp3", "birdcomp4"),
                                    too_few = "align_start", too_many = "debug")

av.data <- separate_wider_delim(av.data, cols = Rec.type, delim = ",", names = c("Rec.1", "Rec.2", "Rec.3"),
                                too_few = "align_start", too_many = "debug")

av.data <- separate_wider_delim(av.data, cols = Forest.comp, delim = ",", names = c("forest1", "forest2", "forest3",
                                                                                    "forest4"),
                                too_few = "align_start", too_many = "debug")


# Cleaning and Organizing Data --------------------------------------------


#cut down to only necessary columns for figures (so far..)
av.meta <- av.data[,c("Year", "Journal", "Country.Auth", "Study.country", "Urb.scale", 
                      "Start.year", "End.year", "Comparator", "birdcomp1", "birdcomp2", 
                      "birdcomp3", "birdcomp4", "Rec.","Rec.1", "Rec.2", "Rec.3","forest1", 
                      "forest2", "forest3", "forest4", "Forest.comp_pieces")]
#remove whitespace
av.meta<- av.meta %>% 
  mutate(across(where(is.character), str_trim))

#replace empty cells with N/A
av.meta <- replace(av.meta, av.meta=='', NA)
print(av.meta)

unique(av.meta$forest1)
av.meta$forest1[av.meta$forest1 == 'Green space type'] <- 'Land use type'
av.meta$forest1[av.meta$forest1 == 'Land Use Type'] <- 'Land use type'
av.meta$forest1[av.meta$forest1 == 'Canopy Cover'] <- 'Canopy cover'
av.meta$forest1[av.meta$forest1 == 'Forested Area'] <- 'Forested area'
unique(av.meta$forest2)
av.meta$forest2[av.meta$forest2 == 'Canopy Cover'] <- 'Canopy cover'
av.meta$forest2[av.meta$forest2 == 'Forested Area'] <- 'Forested area'
av.meta$forest2[av.meta$forest2 == 'Land-use type'] <- 'Land use type'
unique(av.meta$forest3)
av.meta$forest3[av.meta$forest3 == 'Forested Area'] <- 'Forested area'
av.meta$forest3[av.meta$forest3 == 'Land-Use Type'] <- 'Land use type'
av.meta$forest3[av.meta$forest3 == 'Canopy Cover'] <- 'Canopy cover'
unique(av.meta$forest4)
av.meta$forest4[av.meta$forest4 == 'Canopy Cover'] <- 'Canopy cover'

#rename column in av.meta to match shape files with countries
names(av.meta)[names(av.meta) == "Country.Auth"] <- "COUNTRY"

unique(av.meta$COUNTRY)
av.meta$COUNTRY[av.meta$COUNTRY == 'Republic of Korea'] <- 'South Korea'
av.meta$COUNTRY[av.meta$COUNTRY == 'USA '] <- 'USA'
av.meta$COUNTRY[av.meta$COUNTRY == 'United States'] <- 'USA'
av.meta$COUNTRY[av.meta$COUNTRY == 'Austrailia'] <- 'Australia'
av.meta$COUNTRY[av.meta$COUNTRY == 'USA \nof America'] <- 'USA'
av.meta$COUNTRY[av.meta$COUNTRY == 'Finalnd'] <- 'Finland'
av.meta$COUNTRY[av.meta$COUNTRY == 'Chicago'] <- 'USA'
av.meta$COUNTRY[av.meta$COUNTRY == 'Korea'] <- 'South Korea'
av.meta$COUNTRY[av.meta$COUNTRY == 'Australia '] <- 'Australia'
av.meta$COUNTRY[av.meta$COUNTRY == 'Unites States'] <- 'USA'
av.meta$COUNTRY[av.meta$COUNTRY == 'Japan '] <- 'Japan'

# check for mistakes in spelling etc #
unique(av.meta$Journal)

av.meta$Journal[av.meta$Journal == 'Urban Ecoystems'] <- 'Urban Ecosystems'
av.meta$Journal[av.meta$Journal == 'Journal of \nAnimal Ecology'] <- 'Journal of Animal Ecology'

#PUSH OUT CSV
write.csv(av.meta, "out/av.meta.csv", row.names = FALSE)


# Bubble Map Prep ---------------------------------------------------------

cutoffs <- data.frame(id = 1:1, 
                      lat_1 = c(23.5, -23.5), 
                      lon_1 = c(-170.5, -170.5), 
                      lat_2 = c(23.5, -23.5),
                      lon_2 = c(170.5, 170.5))


latlong<- read.csv("in/countries1.csv")
colnames(latlong)[4] ="COUNTRY"
latlong <- latlong[,c(2:4)]

latlong$COUNTRY[latlong$COUNTRY == 'United States'] <- 'USA'
meta_df1<-  av.meta[!(is.na(av.meta$COUNTRY) | av.meta$COUNTRY==""), ]

unique(meta_df1$COUNTRY)

meta_df1$COUNTRY[meta_df1$COUNTRY == 'Tokyo'] <- 'Japan'
meta_df1$COUNTRY[meta_df1$COUNTRY == 'Scotland'] <- 'United Kingdom'
meta_df1$COUNTRY[meta_df1$COUNTRY == 'United States'] <- 'USA'
meta_df1$COUNTRY[meta_df1$COUNTRY == 'Multiple (Finland, Italy, Spain)'] <- 'Multiple'
meta_df1$COUNTRY[meta_df1$COUNTRY == 'Multiple (Greece, Finland)'] <- 'Multiple'
meta_df1$COUNTRY[meta_df1$COUNTRY == 'Multiple (Czech Republic, Poland, Italy, France)'] <- 'Multiple'
meta_df1$COUNTRY[meta_df1$COUNTRY == 'Multiple (14 countries)'] <- 'Multiple'
meta_df1$COUNTRY[meta_df1$COUNTRY == 'Korea'] <- 'South Korea'
meta_df1$COUNTRY[meta_df1$COUNTRY == 'Prague'] <- 'Czech Republic'
meta_df1$COUNTRY[meta_df1$COUNTRY == 'Bulgaria '] <- 'Bulgaria'
meta_df1$COUNTRY[meta_df1$COUNTRY == 'France '] <- 'France'

unique(meta_df1$COUNTRY)
unique(latlong$COUNTRY)

meta_df1<-meta_df1 %>%
  group_by(COUNTRY) %>%
  dplyr::mutate(count= n())

# merge two data frames by ID
merged_df <- merge(meta_df1,latlong,by="COUNTRY", all.x=TRUE)

merged_df1<-merged_df %>% distinct(COUNTRY, .keep_all = TRUE)

merged_df1<-merged_df1 %>% select(1,22:24)
unique(merged_df1$COUNTRY)

data <- merged_df1<- arrange(merged_df1, count)

#write.csv(data, "map_data.csv")

data$latitude[data$latitude == '20.593684'] <- '24.79368'

data$latitude[data$latitude == '-14.235004'] <- '-33.733'

data$latitude[data$latitude == '18.643501'] <- '15.643501'

data$latitude[data$latitude == '-25.274398'] <- '-28.274398'

data$latitude[data$latitude == '39.399872'] <- '40.39987'

data$latitude[data$latitude == '23.69781'] <- '24.89781'

data$latitude<-as.numeric(data$latitude)
data$longitude<-as.numeric(data$longitude)

#PUSH OUT CSV
write.csv(data, "out/data.csv", row.names = FALSE)


# Journal and Country -----------------------------------------------------

# journal name count by country of first author
country.count.av. <- av.meta %>%
  group_by(COUNTRY,Journal) %>%
  dplyr::mutate(journal.count= n())

#raw ncounts per country
authcountry <- av.meta %>%
  group_by(COUNTRY) %>%
  dplyr::mutate(author.count= n())

#calculate average publication count from 2012 to 2022
authoravg<- authcountry[c("COUNTRY", "author.count")]
authoravg <- authoravg[order(-authoravg$author.count),]
#cut to relevant years
authoravg$percent <- (authoravg$author.count/169)*100
authoravg

country.count.av. <- country.count.av. %>%
  group_by(Journal) %>%
  dplyr::mutate(count= n())

#subset columns needed
journal.df.av <- country.count.av.[,c("COUNTRY", "Journal","count","journal.count")]
#sort column descending order
journal.df.av <- arrange(journal.df.av, -count)
#remove duplicates
journal.df.av <- journal.df.av[!duplicated(journal.df.av), ]

journal.df.av$COUNTRY[journal.df.av$COUNTRY == 'USA/ Germany'] <- 'USA'

#esquisse::esquisser(journal.df)

#select only top ten
journal.df.av <- journal.df.av[1:63,]
journal.df.av$COUNTRY[journal.df.av$journal.count <= 1] <- "Other"

write.csv(journal.df.av, "out/journal.df.av.csv", row.names = FALSE)


# Year --------------------------------------------------------------------

#COUNT TABLE
#number of publications per year
year.count <- av.meta %>%
  group_by(Year) %>%
  dplyr::mutate(Year_count= n())
#remove duplicates
year.count <- year.count[!duplicated(year.count$Year), ]

#sort years for results section
yearscolumn <- select(av.meta, "Year")
yearscolumn <- sort(yearscolumn$Year)

#PUSH OUT CSV
write.csv(year.count, "out/Year.count.csv", row.names = FALSE)

#calculate average publication count from 2012 to 2022
year.average<- year.count[c("Year", "Year_count")]
year.average <- year.average[order(year.average$Year),]
#cut to relevant years
year.average<- year.average[c(28:37),]
mean(year.average$Year_count)

#COUNT TABLE
#journal name count by study country
country.count <- av.meta %>%
  group_by(COUNTRY, Journal) %>%
  dplyr::mutate(journal.count= n())

#PUSH OUT CSV
write.csv(country.count, "out/Country.count.csv", row.names = FALSE)


# Recommendation and Journal ----------------------------------------------

journal.rec<- av.meta %>%
  group_by(Rec., Journal) %>%
  dplyr::mutate(journal.count=n())

#get total count on journals
journal.count<- journal.rec[,c("Journal", "journal.count")]
journal.count<- journal.count %>% distinct(Journal, .keep_all = TRUE)

#PUSH OUT CSV
write.csv(journal.rec, "out/journal.rec.csv", row.names = FALSE)

#get total counts on country of first author
country<- av.meta %>%
  group_by(COUNTRY) %>%
  dplyr::mutate(country.count=n())

#get total count on country of first author
authcountry <- country[,c("COUNTRY", "country.count")]
authcountry <- authcountry %>% distinct(COUNTRY, .keep_all = TRUE)


# Urban Scale -------------------------------------------------------------

#COUNT TABLE
#all bird domain columns grouped by URBAN SCALE
urb.counts <-av.meta %>%
  pivot_longer(cols = c(birdcomp1:birdcomp4)) %>%
  dplyr::count(Urb.scale,value)

urb.counts2<- av.meta %>%
  pivot_longer(cols= c(forest1:forest4)) %>%
  dplyr::count(Urb.scale, value)

#non-categorized urban count
urbanallcount<- av.meta %>%
  dplyr:: count(Urb.scale)

#PUSH OUT CSV
write.csv(urb.counts, "out/birdurb.count.csv", row.names = FALSE)
write.csv(urb.counts2, "out/forestbird.urbcount.csv", row.names = FALSE)

# Comparator --------------------------------------------------------------

#COUNT TABLE
#all bird domain columns grouped by COMPARATOR USED Y/N
comp.counts <-av.meta %>%
  pivot_longer(cols = c(birdcomp1:birdcomp4)) %>%
  dplyr::count(Comparator,value)

comp.counts2 <- av.meta %>%
  pivot_longer(cols = c(forest1:forest4)) %>%
  dplyr:: count(Comparator, value)

comp.counts3 <-av.meta %>%
  dplyr::count(Comparator)

#PUSH OUT CSV
write.csv(comp.counts, "out/birdcomp.counts.csv", row.names = FALSE)
write.csv(comp.counts2, "out/forestbirdcomp.counts.csv", row.names = FALSE)

#format for yes/100 to 100% bar graph
Comp<- comp.counts
Comp <- as.data.frame(Comp)

#remove n/a's created from empty rows
Comp <- Comp %>% drop_na(Comparator)
Comp <- Comp %>% drop_na(value)

#sort alphabetically to create percentages
Comp <- Comp[order(Comp[,"value"]), ]
#remove remaining empty cells 
Comp <- Comp[-c(1,2),]

#create another version for binding later
Comp2<- Comp
sum(Comp$n)

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
compno<- subset(Comp, select = c(value, nopercent, N))
colnames(compno)[2] = "comparator"
colnames(compno)[3] = "yes/no"

Compmeta<- rbind(compyes, compno)

#sort
Compmeta <- Compmeta[order(Compmeta[,1]), ]
Comp2[nrow(Comp2) + 1,] <- c("N", "Foraging", 0)
#resort
Comp2 <- Comp2[order(Comp2[,2], Comp2[,3]), ]
Compmeta$comp <- Comp2$Comparator

#swap Y/N because not sorted properly
Compmeta$comp[Compmeta$comp=="N"]<-"Yes"
Compmeta$comp[Compmeta$comp=="Y"]<-"No"
Birdcompmeta<- Compmeta

#PUSH OUT CSV
write.csv(Birdcompmeta, "out/Comp.meta.csv", row.names = FALSE)

#FOREST
#format for yes/100 to 100% bar graph
Comp2<- comp.counts2
Comp2 <- as.data.frame(Comp2)

#remove n/a's created from empty rows
Comp2 <- Comp2 %>% drop_na(Comparator)
Comp2 <- Comp2 %>% drop_na(value)

#sort alphabetically to create percentages
Comp2 <- Comp2[order(Comp2[,"value"]), ]

#create another version for binding later
Comp3<- Comp2
sum(Comp3$n)

#spread rows so that we can create percent bars that reach 100%
Comp2<- Comp2 %>% spread(Comparator, n)

#replace NA with 0
Comp2[is.na(Comp2)] <- 0
#create column of totals
Comp2$total <- Comp2$Y + Comp2$N
#create percent yes column
Comp2$yespercent <- (Comp2$Y/Comp2$total)*100
Comp2$nopercent <- (Comp2$N/Comp2$total)*100
sum(Comp2$total)

compyes<- subset(Comp2, select = c(value, yespercent, Y))
compyes$comp <- c("y","y","y","y","y","y","y","y","y","y")
colnames(compyes)[2] = "comparator"
colnames(compyes)[3] = "count"
colnames(compyes)[4] = "yes/no"
compno<- subset(Comp2, select = c(value, nopercent, N))
compno$comp <- c("n","n","n","n","n","n","n","n","n","n")
colnames(compno)[2] = "comparator"
colnames(compno)[3] = "count"
colnames(compno)[4] = "yes/no"

forest.compmeta<- rbind(compyes, compno)

#sort
forest.compmeta <- forest.compmeta[order(forest.compmeta[,1]), ]

#PUSH OUT CSV
write.csv(forest.compmeta, "out/ForestComp.meta.csv", row.names = FALSE)

# Data Subsetting and Organizing ------------------------------------------


#subset data for STUDY DURATION 
duration.df<- av.meta[,c("birdcomp1", "birdcomp2", "birdcomp3", "birdcomp4", "Start.year", "End.year")]

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
  pivot_longer(cols = c(birdcomp1:birdcomp4)) %>%
  count(duration_bin,value)

duration.counts2 <- duration.df %>%
  count(duration_bin)

#remove duplicate N/As from additional columns for plotting
duration.counts <- duration.counts %>% drop_na(duration_bin)
duration.counts <- duration.counts %>% drop_na(value)
duration.counts2 <- duration.counts2 %>% drop_na(n)

#PUSH OUT CSV
write.csv(duration.counts, "out/Duration.count.csv", row.names = FALSE)
write.csv(duration.counts2, "out/Duration.count2.csv", row.names = FALSE)

################SUBSET FOR RECOMMENDATIONS ########################################

recdf<- av.meta[,c("birdcomp1","birdcomp2", "birdcomp3", "birdcomp4", "Rec.",
                   "Rec.1", "Rec.2", "Rec.3")]

#clean dataframe of whitespace
recdf <- recdf %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

#replace N/As with not recommendation in Rec1 column to incorporate "NO" recommendations
recdf$Rec.1 <- recdf$Rec.1 %>% replace_na("No Recommendations")
#raw counts for results section
recdf

rec.1count<- recdf %>%
  dplyr:: count(Rec.1)
sum(rec.1count$n)

rec2.count<- recdf %>%
  dplyr:: count(Rec.2)

rec3.count<- recdf %>%
  dplyr:: count(Rec.3)

cbind(rec.1count, rec2.count, rec3.count)

#COUNT TABLE
#including all bird domain columns grouped by RECOMMENDATION TYPE 

#REC 1
rectype.counts <-as.data.frame(recdf %>%
                                 pivot_longer(cols = c(birdcomp1:birdcomp4)) %>%
                                 dplyr::count(Rec.1,value))


#clean and remove N/As
rectype.counts <- rectype.counts %>% drop_na(value)
rectype.counts <- rectype.counts %>% drop_na(Rec.1)

#REC 2
rectype.counts2 <-as.data.frame(recdf %>%
                                  pivot_longer(cols = c(birdcomp1:birdcomp4)) %>%
                                  dplyr::count(Rec.2,value))
#remove empty cells from studies with only 1 recommendation ##CHECK CHECK DOUBLE CHECK
rectype.counts2 <- rectype.counts2 %>% drop_na(Rec.2)
rectype.counts2 <- rectype.counts2 %>% drop_na(value)

#rename column to rec.1 to merge
names(rectype.counts2)[names(rectype.counts2) == "Rec.2"] <- "Rec.1"
sum(rectype.counts2$n)

#REC 3
rectype.counts3 <-as.data.frame(recdf %>%
                                  pivot_longer(cols = c(birdcomp1:birdcomp4)) %>%
                                  dplyr::count(Rec.3,value))
rectype.counts3 <- rectype.counts3 %>% drop_na(Rec.3)
rectype.counts3 <- rectype.counts3 %>% drop_na(value)
names(rectype.counts3)[names(rectype.counts3) == "Rec.3"] <- "Rec.1"
sum(rectype.counts3$n)

#MERGE tables of ALL recommendations
#rectype.all <- bind_rows(rectype.counts, rectype.counts2, rectype.counts3) %>% 
# group_by(value, n) %>% 
# distinct(.keep_all = TRUE)

rectype.all <- bind_rows(rectype.counts, rectype.counts2, rectype.counts3) %>% 
  group_by(Rec.1, n) %>% 
  distinct(.keep_all = TRUE)

#resort by category
rectype.all <- as.data.frame(rectype.all)
rectype.all <- rectype.all[order(rectype.all[,"value"]), ]

unique(rectype.all$Rec.1)

#PUSHOUT
write.csv(rectype.all, "out/Allrecraw.count.csv", row.names = FALSE)

#calculate the percent according to topic and recommendation from TOTAL papers
rectype.all
sum(rectype.all$n)
rectype.all$percent<- ((rectype.all$n/267)*100)
rectype.all

#PUSH OUT CSV
write.csv(rectype.all, "out/Allrec.count.csv", row.names = FALSE)

#create a table to count the number of "NO" recommendations
table(recdf['Rec.'])
norec<- data.frame(
  Rec.1 = "None",
  value= "No Recommendations",
  n = 99)
norec$percent <- ((99/274)*100)
norec

norec2<- data.frame(
  Rec.1 = "None",
  value= "No Recommendations",
  n = 99)
norec2$totals <- (274)
norec2

#PUSH OUT CSV
write.csv(norec, "out/Norec.count.csv", row.names = FALSE)
write.csv(norec2, "out/Norec.count2.csv", row.names = FALSE)

#FOREST METRIC#

recdf<- av.meta[,c("forest1","forest2", "forest3", "forest4", "Rec.",
                   "Rec.1", "Rec.2", "Rec.3")]

#clean dataframe of whitespace
#recdf <- recdf %>% 
 # mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

#replace N/As with not recommendation in Rec1 column to incorporate "NO" recommendations
recdf$Rec.1 <- recdf$Rec.1 %>% replace_na("No Recommendations")
trimws("recdf", which = c("both"))

#COUNT TABLE
#including all bird domain columns grouped by RECOMMENDATION TYPE 

#REC 1
rectype.counts <-as.data.frame(recdf %>%
                                 pivot_longer(cols = c(forest1:forest4)) %>%
                                 dplyr::count(Rec.1,value))


#clean and remove N/As
rectype.counts <- rectype.counts %>% drop_na(value)
rectype.counts <- rectype.counts %>% drop_na(Rec.1)

#REC 2
rectype.counts2 <-as.data.frame(recdf %>%
                                  pivot_longer(cols = c(forest1:forest4)) %>%
                                  dplyr::count(Rec.2,value))
#remove empty cells from studies with only 1 recommendation ##CHECK CHECK DOUBLE CHECK
rectype.counts2 <- rectype.counts2 %>% drop_na(Rec.2)
rectype.counts2 <- rectype.counts2 %>% drop_na(value)

#rename column to rec.1 to merge
names(rectype.counts2)[names(rectype.counts2) == "Rec.2"] <- "Rec.1"
sum(rectype.counts2$n)

#REC 3
rectype.counts3 <-as.data.frame(recdf %>%
                                  pivot_longer(cols = c(forest1:forest4)) %>%
                                  dplyr::count(Rec.3,value))
rectype.counts3 <- rectype.counts3 %>% drop_na(Rec.3)
rectype.counts3 <- rectype.counts3 %>% drop_na(value)
names(rectype.counts3)[names(rectype.counts3) == "Rec.3"] <- "Rec.1"
sum(rectype.counts3$n)

#MERGE tables of ALL recommendations
#rectype.all <- bind_rows(rectype.counts, rectype.counts2, rectype.counts3) %>% 
# group_by(value, n) %>% 
# distinct(.keep_all = TRUE)

rectype.all <- bind_rows(rectype.counts, rectype.counts2, rectype.counts3) %>% 
  group_by(Rec.1, n) %>% 
  distinct(.keep_all = TRUE)

#resort by category
rectype.all <- as.data.frame(rectype.all)
rectype.all <- rectype.all[order(rectype.all[,"value"]), ]

unique(rectype.all$value)

#PUSHOUT
write.csv(rectype.all, "out/forestAllrecraw.count.csv", row.names = FALSE)

# Subsetting for Multiple Indicators Counts -------------------------------


#subset data frame to sort # publications with multiple indicators
Indicators <- av.meta[,c("birdcomp1","birdcomp2", "birdcomp3", "birdcomp4")]

#COUNT TABLE
#create count table including all bird domain columns grouped by Indicator TYPE (first only)
# FIRST INDICATOR
Ind1 <- journal.rec<- Indicators %>%
  group_by(birdcomp1) %>%
  dplyr::mutate(indicator1.count=n())
#remove duplicates in first indicator column
Ind1 <- Ind1[!duplicated(Ind1$birdcomp1),]
#remove N/A columns
Ind1<- Ind1 %>% drop_na(birdcomp1)
#calculate sum
sum(Ind1$indicator1.count)


# SECOND INDICATOR
Ind2 <- journal.rec<- Indicators %>%
  group_by(birdcomp2) %>%
  dplyr::mutate(indicator2.count=n())
Ind2 <- Ind2[!duplicated(Ind2$birdcomp2), ]
Ind2<- Ind2 %>% drop_na(birdcomp2)
#calculate sum
sum(Ind2$indicator2.count)

# THIRD INDICATOR
Ind3 <- journal.rec<- Indicators %>%
  group_by(birdcomp3) %>%
  dplyr::mutate(indicator3.count=n())
Ind3 <- Ind3[!duplicated(Ind3$birdcomp3), ]
Ind3<- Ind3 %>% drop_na(birdcomp3)
#calculate sum
sum(Ind3$indicator3.count)

#FOURTH INDICATOR
Ind4 <- journal.rec<- Indicators %>%
  group_by(birdcomp4) %>%
  dplyr::mutate(indicator4.count=n())
Ind4 <- Ind4[!duplicated(Ind4$birdcomp4), ]
Ind4<- Ind4 %>% drop_na(birdcomp4)
sum(Ind4$indicator4.count)

#create data frame to sort # publications with multiple indicators
#use calculate sums ^above
number_topics <- c("One Indicator", "Two Indicators", "Three Indicators", "Four Indicators")
number_publications <- c("277","61", "12", "2")

#create dataframe
allindicators<- data.frame(number_topics, number_publications)  
#PUSH OUT CSV
write.csv(allindicators, "out/Allind.count.csv", row.names = FALSE)


# Topics Only Count -------------------------------------------------------

######### Categories only count ##############
#remove columns not needed
Comp1<- subset(av.data, select = c("birdcomp1"))
colnames(Comp1) <- c("Component")

Comp2 <- av.data[grep("2", av.data$Bird.domainraw_pieces), ]
#remove columns not needed
Comp2<- subset(Comp2, select = c("birdcomp2"))
colnames(Comp2) <- c("Component")

Comp3 <- av.data[grep("3", av.data$Bird.domainraw_pieces), ]
#remove columns not needed
Comp3<- subset(Comp3, select = c("birdcomp3"))
colnames(Comp3) <- c("Component")

Comp4 <- av.data[grep("4", av.data$Bird.domainraw_pieces), ]
Comp4<- subset(Comp4, select = c("birdcomp4"))
colnames(Comp4) <- c("Component")

##MERGING
dfmerge12 <- rbind(Comp1, Comp2)
dfmerge34 <- rbind(dfmerge12, Comp3)
dfmergefinal <- rbind(dfmerge34,Comp4)

dfmergefinal <- dfmergefinal %>%
  mutate_if(is.character, str_trim)

dfmergefinal$Component <- gsub("foraging", "Foraging", dfmergefinal$Component)
dfmergefinal$Component <- gsub("Demographics/patterns", "Demographics/Patterns", dfmergefinal$Component)
unique(dfmergefinal$Component)

birdcomps <- dfmergefinal %>%
  dplyr:: count(Component)
birdcomps <- as.data.frame(birdcomps)


birdcomps$percent <- ((birdcomps$n/277)*100)
birdcomps

#PUSH OUT CSV
write.csv(birdcomps, "out/bird.component.csv", row.names = FALSE)

# FOREST METRICS Only Count -------------------------------------------------------

######### Categories only count ##############
#remove columns not needed
forest1<- subset(av.meta, select = c("forest1"))
colnames(forest1) <- c("forest")

forest2 <- av.meta[grep("2", av.meta$Forest.comp_pieces), ]
#remove columns not needed
forest2<- subset(forest2, select = c("forest2"))
colnames(forest2) <- c("forest")

forest3 <- av.meta[grep("3", av.meta$Forest.comp_pieces), ]
#remove columns not needed
forest3<- subset(forest3, select = c("forest3"))
colnames(forest3) <- c("forest")

forest4 <- av.meta[grep("4", av.meta$Forest.comp_pieces), ]
#remove columns not needed
forest4<- subset(forest4, select = c("forest4"))
colnames(forest4) <- c("forest")

##MERGING
dfmerge12 <- rbind(forest1, forest2)
dfmerge34 <- rbind(dfmerge12, forest3)
dfmergefinal <- rbind(dfmerge34,forest4)

dfmergefinal <- dfmergefinal %>%
  mutate_if(is.character, str_trim)
unique(dfmergefinal$forest)

#clean and remove N/As
birdforestall <- dfmergefinal %>% drop_na(forest)

birdforestallcount <- birdforestall %>%
  dplyr:: count(forest)
birdforestallcount <- as.data.frame(birdforestallcount)

birdforestallcount$percent <- ((birdforestallcount$n/277)*100)
birdforestallcount

#PUSH OUT CSV
write.csv(birdforestallcount, "out/birdforestall.csv", row.names = FALSE)

#MULTIPLE INDICATORS COUNTS
#create data frame to sort # publications with multiple indicators

Indicators <- av.meta[,c("birdcomp1","birdcomp2", "birdcomp3", "birdcomp4")]

#create count table including all bird domain columns grouped by RECOMMENDATION TYPE (first only)
Ind1 <- journal.rec<- Indicators %>%
  group_by(birdcomp1) %>%
  dplyr::mutate(indicator1.count=n())
Ind1 <- Ind1[!duplicated(Ind1),]
#remove N/A columns
Ind1<- Ind1 %>% drop_na(birdcomp1)
#remove remaining irrelevant rows
Ind1 <- Ind1[-c(6,7,9:30),]
sum(Ind1$indicator1.count)

Ind2 <- journal.rec<- Indicators %>%
  group_by(birdcomp2) %>%
  dplyr::mutate(indicator2.count=n())
Ind2 <- Ind2[!duplicated(Ind2), ]
Ind2<- Ind2 %>% drop_na(birdcomp2)
#remove remaining irrelevant rows
Ind2 <- Ind2[-c(2,6,8:10, 12:24),]
sum(Ind2$indicator2.count)

Ind3 <- journal.rec<- Indicators %>%
  group_by(birdcomp3) %>%
  dplyr::mutate(indicator3.count=n())
Ind3 <- Ind3[!duplicated(Ind3), ]
Ind3<- Ind3 %>% drop_na(birdcomp3)
Ind3 <- Ind3[-c(4,6,8:11),]
sum(Ind3$indicator3.count)

Ind4 <- journal.rec<- Indicators %>%
  group_by(birdcomp4) %>%
  dplyr::mutate(indicator4.count=n())
Ind4 <- Ind4[!duplicated(Ind4), ]
Ind4<- Ind4 %>% drop_na(birdcomp4)
sum(Ind4$indicator4.count)

#create data frame to sort # publications with multiple indicators
number_topics <- c("One Indicator", "Two Indicators", "Three Indicators", "Four Indicators")
number_publications <- c("274","60", "11", "2")

allindicators<- data.frame(number_topics, number_publications)  
write.csv(allindicators, "out/Allind.count.csv", row.names = FALSE)

