#pacagesneeded
Packages <- c("tidyverse", "ggplot2", "maps", "ggthemes", "cartography", "sf", "ggpubr", "plotly", "data.table", "rphylopic", "png")
lapply(Packages, library,character.only= TRUE)


########################## FIGURE 3 #########################################################
############### # Publication according to Top 10 Journals with Country ###########################################

#AVIAN#
av.data <- read.csv("avian_data.csv")  

#cut down to only necessary columns for figures (so far..)
av.meta <- av.data[,c("Year", "Journal", "Country.Auth", "Study.country", "Urb.scale", 
                      "Start.year", "End.year", "Comparator", "birddomain1", 
                      "birddomain2", "birddomain3", "birddomain4", "Rec.",
                      "Rec.1", "Rec.2", "Rec.3")]

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

# journal name count by study country
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

library(dplyr)
library(ggplot2)
library(rphylopic)
library(png)

#read file
bird<-readPNG("bird1.png")

av.plot<- journal.df.av %>%
 filter(!is.na(COUNTRY)) %>%
 ggplot() +
 aes(x = reorder(Journal, count), y = journal.count, fill = COUNTRY) +
 geom_col() + scale_fill_viridis(discrete=TRUE, direction=-1, end = 1, begin = 0.1, name="Country") +
 labs(x= "", y= "Number of Publications") +
 coord_flip() +
 theme_classic() + 
  theme(legend.position ="bottom",
        text = element_text(color = "#22211d",size=4.5,family = "serif"),
        axis.text.y = element_text(face="bold"),
        plot.margin = margin(1, 0.3, 1, 1, "cm"),
        legend.text = element_text(size = 3.3),
        legend.key.size = unit(0.35,"line"),
        legend.background = element_rect(fill = "white", color = NA)) + 
  guides(fill = guide_legend(reverse = TRUE)) +
  add_phylopic(bird, alpha = 1, x = 2, y = 35.4, ysize =0.8)
av.plot
  
#FOREST#

#### Forest publications map ####

forest_data<- read.csv("forest_data.csv")

#cut down to only necessary columns for figures (so far..)
for.meta <- forest_data[,c(6:9)]

#rename column in av.meta to match shape files with countries
names(for.meta)[names(for.meta) == "Country.of.First.Author"] <- "COUNTRY"

#replace empty cells with N/A
for.meta <- replace(for.meta, for.meta=='', NA) 

unique(for.meta$COUNTRY)

for.meta$COUNTRY[for.meta$COUNTRY == 'United States'] <- 'USA'
for.meta$COUNTRY[for.meta$COUNTRY == 'United State of America'] <- 'USA'
for.meta$COUNTRY[for.meta$COUNTRY == 'United States of America'] <- 'USA'
for.meta$COUNTRY[for.meta$COUNTRY == 'Korea'] <- 'South Korea'
for.meta$COUNTRY[for.meta$COUNTRY == 'Helsinki'] <- 'Finland'
for.meta$COUNTRY[for.meta$COUNTRY == 'Republic of Korea'] <- 'South Korea'

unique(for.meta$Journal)
for.meta$Journal[for.meta$Journal == 'Forests Multidisciplinary Digital Publishing Institute'] <- 'Forests'
for.meta$Journal[for.meta$Journal == 'Forests Multidisciplinary Digital Publishing'] <- 'Forests'
for.meta$Journal[for.meta$Journal == 'Multidisciplinary Digital Publishing Institute'] <- 'Forests'
for.meta$Journal[for.meta$Journal == 'Sustainability Multidisciplinary Digital Publishing Institute'] <- 'Sustainability'
for.meta$Journal[for.meta$Journal == 'Susainability'] <- 'Sustainability'
for.meta$Journal[for.meta$Journal == 'Urban Foresty & Urban Greening'] <- 'Urban Forestry & Urban Greening'

country.count <- for.meta %>%
  group_by(COUNTRY, Journal) %>%
  dplyr::mutate(journal.count= n())

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

#read file
tree<-readPNG("tree.png")

for.plot <-journal.df %>%
  filter(!is.na(COUNTRY)) %>%
  ggplot() +
  aes(x = reorder(Journal, count), y = journal.count, fill = COUNTRY) +
  geom_col() + scale_fill_viridis(discrete=TRUE, direction=-1, end = 1, begin = 0.1, name="Country") +
  labs(x= "", y= "Number of Publications") +
  coord_flip() +
  theme_classic() + 
  theme(legend.position ="bottom",
        text = element_text(color = "#22211d",size=4.5,family = "serif"),
        axis.text.y = element_text(face="bold"),
        plot.margin = margin(1, 1.5, 1, 0.3, "cm"),
        legend.text = element_text(size = 3.3),
        legend.key.size = unit(0.35,"line"),
        legend.background = element_rect(fill = "white", color = NA)) + 
  guides(fill = guide_legend(reverse = TRUE)) +
  add_phylopic(tree, alpha = 1, x = 1.8, y = 20.4, ysize =0.8)
 for.plot

 library(cowplot)
 av_for_maps<- plot_grid(av.plot, for.plot)
 av_for_maps
 
 ggsave(filename ="av_for_plots_test.tiff", width = 170, units="mm", height = 85 , device='tiff', dpi=300)  

