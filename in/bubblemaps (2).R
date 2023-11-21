#### Bird publications map ####

#packagesneeded
Packages <- c("tidyverse", "ggplot2", "maps", "bibliometrix", "ggthemes", "cartography", "sf")
lapply(Packages, library,character.only= TRUE)

getwd()
setwd("C:/Users/rkinnun/Desktop/systematic_map_analysis")

#loading avian data and formatting
av.data <- read.csv("avian_data.csv")

########## CLEANING AND ORGANIZING DATA ################


#### Number of Studies per country (i.e., counts by study country, NOT country of first author) with cutoffs - NOTE this is just in case we want to have this as well ---> counts by first author country come below! ####
#cut down to only necessary columns for figures (so far..)
av.meta <- av.data[,c("Year", "Journal", "Country.Auth", "Study.country", "Urb.scale", 
                      "Start.year", "End.year", "Comparator", "birddomain1", 
                      "birddomain2", "birddomain3", "birddomain4", "Rec.",
                      "Rec.1", "Rec.2", "Rec.3")]

#rename column in av.meta to match shape files with countries
names(av.meta)[names(av.meta) == "Study.country"] <- "COUNTRY"

#replace empty cells with N/A
av.meta <- replace(av.meta, av.meta=='', NA)
av.meta<- read.csv("av.meta.csv")

cutoffs <- data.frame(id = 1:1, 
                      lat_1 = c(23.5, -23.5), 
                      lon_1 = c(-170.5, -170.5), 
                      lat_2 = c(23.5, -23.5),
                      lon_2 = c(170.5, 170.5))


latlong<- read.csv("countries.csv")
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

library(stringr)
library(dplyr)
meta_df1<-meta_df1 %>%
  group_by(COUNTRY) %>%
  dplyr::mutate(count= n())

# merge two data frames by ID
merged_df <- merge(meta_df1,latlong,by="COUNTRY", all.x=TRUE)
library(ggplot2)
merged_df1<-merged_df %>% distinct(COUNTRY, .keep_all = TRUE)

merged_df1<-merged_df1 %>% select(1,17:19)
unique(merged_df1$COUNTRY)

## Map cities and count of papers
library(maps)
world<- map_data("world")

# viridis package for the color palette
library(viridis)

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

# Build the map
  
# Reorder data to show biggest cities on top
data <- data %>%
    arrange(-count)
  
library(ggrepel) 
mybreaks <- c(1,5,15,35,95)  
   
  # Build the map
   
  av.map<- ggplot() +
    geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
    geom_point( data=data, aes(x=longitude, y=latitude, size=count, color=count), shape=20, stroke=FALSE) + 
    geom_text_repel(data=data %>% arrange(count) %>% tail(10), aes(x=longitude, y=latitude, label=COUNTRY),     size=3, box.padding = 0.7, family = "serif", segment.color="grey50", max.overlaps = Inf) +
    scale_size_continuous(range=c(2.5,16), breaks=mybreaks,name="Number of Studies") +
    scale_color_viridis(option="viridis", end = 1, begin = 0.1, breaks=mybreaks, direction=-1, discrete =        FALSE, name="Number of Studies") +
    theme_void() +  
    coord_quickmap() +
    ggtitle("") + 
    theme(legend.position ="bottom",legend.direction = "horizontal",
          text = element_text(color = "#22211d", size=10,family = "serif"),
          plot.background = element_rect(fill = "white", color = NA), 
          panel.background = element_rect(fill = "white", color = NA), 
          legend.background = element_rect(fill = "white", color = NA)) +
          guides(color = guide_legend(nrow=1, byrow=TRUE)) +
    geom_segment(data = cutoffs,aes(x = lon_1, y = lat_1, xend = lon_2, yend = lat_2), color = "grey", alpha           = 0.5, linewidth = 0.6, linetype=2 , lineend = "round") 

  av.map
  
  #### Forest publications map ####
    
  forest_data<- read.csv("forest.data.csv")
  
  #cut down to only necessary columns for figures (so far..)
  for.meta <- forest_data[,c(6,8,9, 13)]
  
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
    merged_for<-merged_for %>% select(1,5:7)
    merged_for<-merged_for %>% distinct(COUNTRY, .keep_all = TRUE)
    
    data1 <- arrange(merged_for, -count)
    write.csv(data1, "map_data_for.csv")
    
    data1$latitude[data1$latitude == "23.82408"] <- "25.562583"
    
    data1$latitude[data1$latitude == '24.24902'] <- '25.24902'	
    
    data1$latitude <- as.numeric(data1$latitude)
    
    
    # Build the map
    
    # Reorder data to show biggest cities on top
    data1 <- data1 %>%
      arrange(count)
    
    mybreaks <- c(1,5,15,25,60)
    
    # Build the map
   for.map<-  ggplot() +
     geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
     geom_point( data=data1, aes(x=longitude, y=latitude, size=count, color=count), shape=20, stroke=FALSE) + 
     geom_text_repel(data=data1 %>% arrange(count) %>% tail(10), aes(x=longitude, y=latitude, label=COUNTRY),     size=3, box.padding = 0.7, family = "serif", segment.color="grey50", max.overlaps = Inf) +
     scale_size_continuous(range=c(2.5,16), breaks=mybreaks,name="Number of Studies") +
     scale_color_viridis(option="turbo", end = 0.8, begin = 0.3, breaks=mybreaks, direction=-1, discrete =        FALSE, name="Number of Studies") +
     theme_void() +  
     coord_quickmap() +
     ggtitle("") + 
     theme(legend.position ="bottom",legend.direction = "horizontal",
           text = element_text(color = "#22211d", size=10,family = "serif"),
           plot.background = element_rect(fill = "white", color = NA), 
           panel.background = element_rect(fill = "white", color = NA), 
           legend.background = element_rect(fill = "white", color = NA)) +
     guides(color = guide_legend(nrow=1, byrow=TRUE)) +
     geom_segment(data = cutoffs,aes(x = lon_1, y = lat_1, xend = lon_2, yend = lat_2), color = "grey",          alpha = 0.5, linewidth = 0.6, linetype=2 , lineend = "round") 
  
    for.map
   
   library(cowplot)
   av_for_maps<- plot_grid(av.map, for.map)
   av_for_maps
    
   ggsave(filename ="av_for_maps1.png", width = 12, height = 6, device='tiff', dpi=300)
  
   

   
 ##### Bubble maps ####################################################################
  
  #### Number of Publications per country by country of first author ####
   ## Avian ##
   
   #loading avian data and formatting
   av.data <- read.csv("avian_data.csv")  
    
   #cut down to only necessary columns for figures (so far..)
   av.meta <- av.data[,c("Year", "Journal", "Country.Auth", "Study.country", "Urb.scale", 
                         "Start.year", "End.year", "Comparator", "birddomain1", 
                         "birddomain2", "birddomain3", "birddomain4", "Rec.",
                         "Rec.1", "Rec.2", "Rec.3")]
   
   #rename column in av.meta to match shape files with countries
   names(av.meta)[names(av.meta) == "Country.Auth"] <- "COUNTRY"
   
   #replace empty cells with N/A
   av.meta <- replace(av.meta, av.meta=='', NA)
   #av.meta<- read.csv("av.meta.csv")

   latlong<- read.csv("countries.csv")    ## https://www.kaggle.com/datasets/paultimothymooney/latitude-and-longitude-for-every-country-and-state #
   colnames(latlong)[4] ="COUNTRY"
   latlong <- latlong[,c(2:4)]
   latlong$COUNTRY[latlong$COUNTRY == 'United States'] <- 'USA'
 
   ## data cleaning
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
   
   unique(av.meta$COUNTRY)
   
   library(stringr)
   library(dplyr)
   av.meta<-av.meta %>%
     group_by(COUNTRY) %>%
     dplyr::mutate(count= n())
   
   # merge two data frames by ID
   merged_df <- merge(av.meta,latlong,by="COUNTRY", all.x=TRUE)
   library(ggplot2)
   merged_df1<-merged_df %>% distinct(COUNTRY, .keep_all = TRUE)
   
   merged_df1<-merged_df1 %>% select(1,17:19)
   unique(merged_df1$COUNTRY)
   
   ## Map cities and count of papers
   library(maps)
   world<- map_data("world")
   
   # viridis package for the color palette
   library(viridis)
   
   data <- merged_df1<- arrange(merged_df1, count)
   data$latitude[data$latitude == '18.643501'] <- '15.643501'
   data$latitude <- as.numeric(data$latitude)
   
   #write.csv(data, "map_data.csv")
   
   # Build the map
   
   # Reorder data to show biggest cities on top
   data <- data %>%
     arrange(-count)
   
   library(ggrepel) 
   mybreaks <- c(1,5,15,35,95)  
   
   # Build the map
   
   av.map<- ggplot() +
     geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
     geom_point( data=data, aes(x=longitude, y=latitude, size=count, color=count), shape=20, stroke=FALSE) + 
     geom_text_repel(data=data %>% arrange(count) %>% tail(10), aes(x=longitude, y=latitude, label=COUNTRY), size=1.5, box.padding = 0.4,min.segment.length = 0, segment.size=0.2, family = "serif", segment.color="grey50", max.overlaps = Inf) +
     scale_size_continuous(range=c(1,6), breaks=mybreaks,name="# Publications") +
     scale_color_viridis(option="viridis", end = 1, begin = 0.1, breaks=mybreaks, direction=-1, discrete =FALSE, name="# Publications") +
     theme_void() +  
     coord_quickmap() +
     ggtitle("") + 
     theme(legend.position ="bottom",legend.direction = "horizontal",
           text = element_text(color = "#22211d", size=6,family = "serif"),
           plot.background = element_rect(fill = "white", color = NA), 
           panel.background = element_rect(fill = "white", color = NA), 
           legend.background = element_rect(fill = "white", color = NA)) +
     guides(color = guide_legend(nrow=1, byrow=TRUE)) 
   
   av.map
   
   #### Forest publications map ####
   
   forest_data<- read.csv("forest.data.csv")
   
   #cut down to only necessary columns for figures (so far..)
   for.meta <- forest_data[,c(6,8,9, 13)]
   
   #rename column in av.meta to match shape files with countries
   names(for.meta)[names(for.meta) == "Country of First Author"] <- "COUNTRY"
  
   #replace empty cells with N/A
   for.meta <- replace(for.meta, for.meta=='', NA) 
   
   unique(for.meta$COUNTRY)
   
   for.meta$COUNTRY[for.meta$COUNTRY == 'United States'] <- 'USA'
   for.meta$COUNTRY[for.meta$COUNTRY == 'United State of America'] <- 'USA'
   for.meta$COUNTRY[for.meta$COUNTRY == 'United States of America'] <- 'USA'
   for.meta$COUNTRY[for.meta$COUNTRY == 'Korea'] <- 'South Korea'
   for.meta$COUNTRY[for.meta$COUNTRY == 'Helsinki'] <- 'Finland'
   for.meta$COUNTRY[for.meta$COUNTRY == 'Republic of Korea'] <- 'South Korea'
   #for.meta$COUNTRY[for.meta$COUNTRY == 'France '] <- 'France'
   
   meta_for<-for.meta %>%
     group_by(COUNTRY) %>%
     dplyr::mutate(count= n())
   
   
   # merge two data frames by ID
   merged_for <- merge(meta_for,latlong,by="COUNTRY", all.x=TRUE)
   merged_for<-merged_for %>% select(1,5:7)
   merged_for<-merged_for %>% distinct(COUNTRY, .keep_all = TRUE)
   
   data1 <- arrange(merged_for, -count)
   #write.csv(data1, "map_data_for.csv")
   
   data1$latitude[data1$latitude == '18.643501'] <- '15.643501'

   
   data1$latitude <- as.numeric(data1$latitude)
   
   
   # Build the map
   
   # Reorder data to show biggest cities on top
   data1 <- data1 %>%
     arrange(-count)
   
   mybreaks <- c(1,5,15,25,59)
   
   # Build the map
   for.map<-  ggplot() +
     geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
     geom_point( data=data1, aes(x=longitude, y=latitude, size=count, color=count), shape=20, stroke=FALSE) + 
     geom_text_repel(data=data1 %>% arrange(count) %>% tail(10), aes(x=longitude, y=latitude, label=COUNTRY),  size=1.5, box.padding = 0.4, min.segment.length = 0,segment.size= 0.2,family = "serif", segment.color="grey50", max.overlaps = Inf) +
     scale_size_continuous(range=c(1,5), breaks=mybreaks,name="# Publications") +
     scale_color_viridis(option="viridis", end = 1, begin = 0.1, breaks=mybreaks, direction=-1, discrete = FALSE, name="# Publications") +
     theme_void() +  
     coord_quickmap() +
     ggtitle("") + 
     theme(legend.position ="bottom",legend.direction = "horizontal",
           text = element_text(color = "#22211d", size=6,family = "serif"),
           plot.background = element_rect(fill = "white", color = NA), 
           panel.background = element_rect(fill = "white", color = NA), 
           legend.background = element_rect(fill = "white", color = NA)) +
     guides(color = guide_legend(nrow=1, byrow=TRUE)) 
   for.map
   
   library(cowplot)
   av_for_maps<- plot_grid(av.map, for.map)
   av_for_maps
   
   ggsave(filename ="av_for_maps_test.tiff", width = 170, units="mm", height = 85 , device='tiff', dpi=300)  
  