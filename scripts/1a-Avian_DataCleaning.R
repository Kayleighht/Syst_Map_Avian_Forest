#packagesneeded
source('scripts/0-packages.R')

#loading avian data and formatting
av.data <- read.csv("in/Metadata_Avian_Component.csv")

#spread bird topic into separate columns
av.data <- separate_wider_delim(av.data, cols = Bird.domainraw, delim = ",", names = c("birdcomp1", "birdcomp2", "birdcomp3", "birdcomp4"),
                                    too_few = "align_start", too_many = "debug")

av.data <- separate_wider_delim(av.data, cols = Rec.type, delim = ",", names = c("Rec.1", "Rec.2", "Rec.3"),
                                too_few = "align_start", too_many = "debug")


# Cleaning and Organizing Data --------------------------------------------


#cut down to only necessary columns for figures (so far..)
av.meta <- av.data[,c("Year", "Journal", "Country.Auth", "Study.country", "Urb.scale", 
                      "Start.year", "End.year", "Comparator", "birdcomp1", "birdcomp2", "birdcomp3", "birdcomp4", "Rec.",
                      "Rec.1", "Rec.2", "Rec.3","Forest.comp")]
#remove whitespace
av.meta<- av.meta %>% 
  mutate(across(where(is.character), str_trim))

#replace empty cells with N/A
av.meta <- replace(av.meta, av.meta=='', NA)
print(av.meta)

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

merged_df1<-merged_df1 %>% select(1,18:20)
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

#non-categorized urban count
urbanallcount<- av.meta %>%
  dplyr:: count(Urb.scale)

#PUSH OUT CSV
write.csv(urb.counts, "out/birdurb.count.csv", row.names = FALSE)


# Comparator --------------------------------------------------------------


#COUNT TABLE
#all bird domain columns grouped by COMPARATOR USED Y/N
comp.counts <-av.meta %>%
  pivot_longer(cols = c(birdcomp1:birdcomp4)) %>%
  dplyr::count(Comparator,value)

comp.counts2 <-av.meta %>%
  dplyr::count(Comparator)

#PUSH OUT CSV
write.csv(comp.counts, "out/Birdcomp.counts.csv", row.names = FALSE)

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

# Subsetting for Multiple Indicators Counts -------------------------------


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



# Topics Only Count -------------------------------------------------------


######### Categories only count ##############
#create dataframes for each indicator/topic and align column names

#remove columns not needed
Comp1<- subset(av.meta, select = c("birdcomp1"))
colnames(Comp1) <- c("Component")

Comp2 <- Birdcomps[grep("2", Birdcomps$Bird.domainraw_pieces), ]
#remove columns not needed
Comp2<- subset(Comp2, select = c("birdcomp2"))
colnames(Comp2) <- c("Component")

Comp3 <- Birdcomps[grep("3", Birdcomps$Bird.domainraw_pieces), ]
#remove columns not needed
Comp3<- subset(Comp3, select = c("birdcomp3"))
colnames(Comp3) <- c("Component")

Comp4 <- Birdcomps[grep("4", Birdcomps$Bird.domainraw_pieces), ]
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

######### Categories of forest management only count ##############
#create dataframes for each indicator/topic and align column names
av.forestmanagement <- separate_wider_delim(av.data, cols = Forest.comp, delim = ",", 
                                names = c("fint1", "fint2", "fint3", "fint4", "fint5"),
                                too_few = "align_start", too_many = "debug")
av.forestmanagement <- av.forestmanagement[,c("fint1", "fint2", "fint3", "fint4", "Forest.comp_pieces")]

#remove columns not needed
Comp1<- subset(av.forestmanagement, select = c("fint1"))
colnames(Comp1) <- c("Component")

Comp2 <- av.forestmanagement[grep("2", av.forestmanagement$Forest.comp_pieces), ]
#remove columns not needed
Comp2<- subset(Comp2, select = c("birdcomp2"))
colnames(Comp2) <- c("Component")

Comp3 <- av.forestmanagement[grep("3", av.forestmanagement$Forest.comp_pieces), ]
#remove columns not needed
Comp3<- subset(Comp3, select = c("birdcomp3"))
colnames(Comp3) <- c("Component")

Comp4 <- av.forestmanagement[grep("4", av.forestmanagement$Forest.comp_pieces), ]
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
write.csv(birdcomps, "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/bird.component.csv", row.names = FALSE)