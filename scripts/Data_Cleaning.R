#packagesneeded
Packages <- c("tidyverse", "ggplot2", "maps", "bibliometrix", "ggthemes", "cartography", "sf")

lapply(Packages, library,character.only= TRUE)

getwd()
setwd("C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/in")

#loading avian data and formatting
av.data <- read.csv("Meta_Avian_Data_Extraction - Master_Data.csv")

#create a count column based on year of publication
av.data <- av.data %>%
  group_by(Year) %>%
  mutate(Year_count= n())

#set x and y labels
lab<- labs(x= "Year", y= "Number of Publications")

#plotting articles over time
byyear_av<- ggplot(av.data, aes(x= Year, y=Year_count))  + 
  geom_line(size= 0.9, colour= "purple") + lab + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 8, angle = 90, 
        colour = "black")) + 
  scale_x_continuous(breaks = 1979:2022, limits = c(1979, 2022)) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24), 
                     limits = c(0,24))
byyear_av

#### GLOBAL PUBLICATION HEAT MAP ####
# here I will create a heat map to show where the majority of studies are being conducted

library(dplyr)
#rename column in av.data to match shape file
names(av.data)[names(av.data) == "Study.country"] <- "COUNTRY"

#adding column in dataframe with count for each country
av.data <- av.data %>%
  group_by(COUNTRY) %>%
  mutate(country.count= n())

#subsetting columns we need first
region.df<- av.data[,c("COUNTRY","country.count")]

#remove duplicates
region.df <- region.df[!duplicated(region.df), ]

#remove columns with multiple (don't specificy region)
#filter rows that contain the string 'Multiple' in the country column
region.df <- region.df %>% filter(!grepl('Multiple', COUNTRY))

#load shape file with global map including all countries
mapdata<- st_read("World_Countries_Generalized.shp")

#merge data with studies countries location from systematic map
mapdata2<- merge(mapdata, region.df, by="COUNTRY")

#upload layer of simple coordinates of global map for bubblemaps 
coordmap<- read.csv("countries.csv")

#merge with country count data
coordmap<- merge(mapdata2, coordmap, by="COUNTRY")

#PLOTTING     
#####WITH MID range 
ggplot(data= mapdata) + geom_sf(color= "white", fill= "lightgrey") +
      geom_sf(data= mapdata2, aes(fill= country.count), color= "white") + 
      geom_point(data= coordmap, aes(x=longitude, y=latitude, size= country.count, alpha= country.count, col= country.count), shape=20, stroke=FALSE) +
      scale_color_viridis_b(option="magma", trans="log", name="Publications (N)" ) +
      scale_fill_viridis_c(option = "rocket")+
      xlab("Longitude")+ ylab("Latitude") +
      ggtitle("Studies by Country") +
      theme(
            legend.position = "bottom",
            text = element_text(color = "black"),
            plot.background = element_rect(fill = "#f5f5f2", color = NA), 
            panel.background = element_rect(fill = "#f5f5f2", color = NA), 
            legend.background = element_rect(fill = "#f5f5f2", color = NA),
            plot.title = element_text(size= 15, hjust=0.1, color = "black", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
            )

##PLOTTING Location vs. Journal

#clean dataframe and create column with count_data
av.data <- av.data %>%
  group_by(COUNTRY, Journal) %>%
  mutate(journal.count= n())

#subset columns needed
journal.df <- av.data[,c("COUNTRY", "Journal", "journal.count")]
#sort column descending order
journal.df <- arrange(journal.df, -journal.count)
#remove duplicates
journal.df <- journal.df[!duplicated(journal.df), ]

#select only top ten
journal.df <- journal.df[1:10,]

journal.plot <- ggplot(journal.df, aes(x= reorder(Journal, -journal.count), y= journal.count, fill = COUNTRY)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(size= 10, angle= 90)) +
  labs(x= "Journal", y= "Number of Publications") +
  theme_hc()
journal.plot

##################################### DESIGN FIGURES ##########################################################

##WAS COMPARATOR USED (design)

##clean data
av.data<- as_tibble(av.data)
#get column names
colnames(av.data)
#rename columns need for bar chart
av.data<- rename(av.data, "comparator" = "Comparator")
av.data<- rename(av.data, bird.domain = Bird.domain)

#create a count table of comparator used based on bird domain
av.data <- av.data %>%
  group_by(Domain.multi, comparator) %>%
  mutate(comparator_count= n())

#subsetting columns we need first
av.comparator<- av.data[,c("Domain.multi","comparator_count", "comparator")]

#remove duplicates
av.comparator <- av.comparator[!duplicated(av.comparator), ]

##remove blank rows (LOOK IN THIS^^^^^)
av.comparator <- av.comparator[-19,]

#remove N/A
av.comparator <- av.comparator[-13,]

#create column that converts counts to percentage 
#first need to convert column to numeric
av.comparator <- av.comparator %>% mutate_at('comparator_count', as.numeric)

#mutate
av.comparator <-av.comparator %>% mutate(Percentage = (comparator_count/270*100))

#plotting bar chart
comparator.plot<- ggplot(av.comparator, aes(fill = comparator, x= reorder(Domain.multi,-Percentage), 
                                            y= Percentage)) +
                  geom_bar(stat= 'identity') + theme(axis.text.x = element_text(size= 10, angle = 90))+
                  theme_hc() + labs(x= "Success Measurement") + scale_y_continuous(limits = c(0,60), 
                                                                                   breaks = c(10, 20, 30, 40, 50, 60)) +
                  coord_flip()
comparator.plot

##### SCALE OF MEASUREMENT

av.data <- av.data %>%
  group_by(Urb.scale, Domain.multi) %>%
  mutate(scale_count= n())

scale.df <- av.data[, c("Urb.scale", "scale_count", "Domain.multi")]
view(scale.df)

#remove duplicates
scale.df <- scale.df[!duplicated(scale.df), ]
#remove N/A (DOUBLE CHECK)
scale.df <- scale.df[-17,]

#######PLOT

scale.plot <- ggplot(scale.df, aes(fill= Urb.scale, x= reorder(Domain.multi, -scale_count), y= scale_count)) +
              geom_bar(stat= 'identity') + theme_hc() + labs(x= "Bird component", y= "Count") +
              theme(axis.text.x = element_text(size = 10, angle = -30)) + coord_flip()
scale.plot    

####### DURATION OF STUDY

#clean and organize data
duration.df<- av.data[,c("Domain.multi", "Start.year", "End.year")]

#remove N/A
duration.df<- duration.df[!grepl('N/A', duration.df$Start.year),]
duration.df<-duration.df[!grepl('N/A', duration.df$End.year),]

#ensure columns are numerics
duration.df <- duration.df %>% mutate_at(c('Start.year', 'End.year'), as.numeric)
duration.df <- as.data.frame(duration.df)

#create new column with duration
duration.df$duration <- (duration.df$End.year-duration.df$Start.year)

##CHECK ROW 201 FORMATTING, remove for now
duration.df<- duration.df[-201,]

#since single year studies are zeros according to calculation, I will transform column by adding one to all values
duration.df$duration2 <-duration.df[,4]+1

#create bins for values based on duration column
duration.df<- duration.df %>% mutate(duration_bin = cut(duration2, breaks=c(0, 1, 5, 10, 100)))

#make count column based on bins for figure
duration.df <- duration.df %>%
  group_by(duration_bin) %>%
  mutate(duration_count= n())

#remove start and end year, duration
duration.df<- duration.df[ , !names(duration.df) %in% 
      c("Start.year","End.year","duration" , "duration2")]
#remove duplicates and N/As
duration.df <- duration.df[!duplicated(duration.df), ]

#remove N/As
duration.df<- duration.df[-c(25,28),]

##PLOTTING###############

duration.plot <- ggplot(duration.df, aes(x= duration_bin , y= duration_count)) +
  geom_bar(stat= 'identity', position = 'dodge') + theme_hc() + labs(x= "Bird component", y= "Study Duration") +
  theme(axis.text.x = element_text(size = 10, angle = -30)) 
duration.plot

############# RECOMMENDATIONS

#cleaning and organizing data
#clean and organize data
rec.df<- av.data[,c("Category.multi", "Rec.", "Rec.type")]

#add count column
rec.df <- rec.df %>%
  group_by(Rec.type, Category.multi) %>%
  mutate(rec_count= n())

#add count column for yes/no recommendation
rec.df <- rec.df %>%
  group_by(Rec.type, Category.multi) %>%
  mutate(rec2_count = n())
rec.df

#subset yes or no
rec1.df<- rec.df[,c("Category.multi", "Rec.", "rec2_count")]
#remove duplicates
rec1.df <- rec.df[!duplicated(rec.df), ]

### PLOT YES/NO

rec1.plot <- ggplot(rec1.df, aes(x= reorder(Category.multi, -rec2_count), y= rec2_count, fill= Rec.)) +
  geom_bar(stat= 'identity', position = 'dodge') + theme_hc() + labs(x= "Bird component category", y= "Recommendations included?") +
  theme(axis.text.x = element_text(size = 10, angle = -30)) + coord_flip()

rec1.plot

### subset recommendation type
rec.df<- rec.df[,c("Category.multi", "Rec.type", "rec_count")]

#remove N/As (no recommendation included)
rec.df<- rec.df[!grepl('N/A', rec.df$Rec.type),]

#remove duplicates
rec.df <- rec.df[!duplicated(rec.df), ]

### PLOT RECOMMENDATION TYPE

rec.plot <- ggplot(rec.df, aes(x= reorder(Category.multi, -rec_count), y= rec_count, fill= Rec.type)) +
  geom_bar(stat= 'identity') + theme_hc() + labs(x= "Bird component category", y= "Recommendations") +
  theme(axis.text.x = element_text(size = 10, angle = -30)) + coord_flip()

rec.plot

