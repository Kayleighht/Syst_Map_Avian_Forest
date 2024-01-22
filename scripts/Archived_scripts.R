### SYSTEMATIC MAP #####

# TRACKING R CoDE THAT WILL NOT BE USED IN PUBLICATION

######################################
########### FIGURES ##################
######################################

#plotting articles over time
yyear_av<- ggplot(year.count, aes(x= Year, y=Year_count))  + 
  geom_point(size= 1.0, colour= "#c51b8a") + lab + geom_line(size= 0.7, colour= "#c51b8a") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 7.5, angle = 90, 
                                   colour = "black")) + 
  scale_x_continuous(breaks = c(1980, 1985, 1990, 1995,2000, 2005, 2010, 2015, 2020), limits = c(1979, 2021)) +
  scale_y_continuous(breaks = c(5,10,15,20,25), 
                     limits = c(0,25))
byyear_av + alltheme + geom_point(data= fyear.count, aes(x=Year, y = Year_count), size= 1.0, colour = "#31a354") 
  geom_line(data= fyear.count, aes(x=Year, y = Year_count), size= 0.7, colour = "#31a354") 

  ############################### FIGURE __ #######################################################
  ######################### RECOMMENDATION INCLUDED BY JOURNAL #################################################
  
  #AVIAN#
  Journal.rec<- read.csv("journal.rec.csv")
  #subset only the columns we need
  Journal.rec<- Journal.rec[,c("Journal","Rec.", "journal.count")]
  #remove duplicates
  Journal.rec <- Journal.rec[!duplicated(Journal.rec), ]


  #reorder ascending
  Journal.rec <- Journal.rec[order(Journal.rec$journal.count , decreasing = TRUE),]
  sum(Journal.rec$journal.count)
  Journal.recalpha <- Journal.rec[order(Journal.rec[,1]), ]
  #spread rows so that we can create percent bars that reach 100%
  Journal.recalpha <-  Journal.recalpha %>% spread(Rec., journal.count)
  
  Journal.recalpha[is.na(Journal.recalpha)] <- 0
  #create column of totals
  Journal.rec <- Journal.recalpha
  Journal.rec$total <- Journal.rec$Yes + Journal.rec$No
  #create percent yes column
  Journal.rec$yespercent <- (Journal.rec$Yes/Journal.rec$total)*100
  Journal.rec$nopercent <- (Journal.rec$No/Journal.rec$total)*100
  
  Journal.rec <- Journal.rec[order(-Journal.rec[,4], Journal.rec[,1]), ]
  Journal.rec <- Journal.rec[1:10,]
  
  Journal.rec <- Journal.rec %>% pivot_longer(., cols = c(yespercent, nopercent))
  
  #PLOT
  
  Journal.rec <- ggplot(Journal.rec, aes(fill = name , x= Journal, y= value)) +
    geom_bar(stat= 'identity') + theme(axis.text.x = element_text(size= 11), axis.text.y = element_text(
      size = 12, face = "bold", angle = 10)) +
    theme_hc() + labs(x= "", y= "Percent of Studies") + scale_fill_brewer(palette = "Set2", labels= c("No", "Yes")) +
    coord_flip() + scale_y_continuous(limits = c(0,110), breaks = c(0,20,40,60,80,100))
  
  bird.journalrec <- Journal.rec + alltheme + theme_legend2 + labs(fill= "Recommendations 
included?")
  
  #FOREST#
  Journal.rec<- read.csv("fjournal.rec.csv")
  #subset only the columns we need
  Journal.rec<- Journal.rec[,c("Journal","Rec.included", "journal.count")]
  #remove duplicates
  Journal.rec <- Journal.rec[!duplicated(Journal.rec), ]
  
  #reorder ascending
  Journal.rec <- Journal.rec[order(Journal.rec$journal.count , decreasing = TRUE),]
  sum(Journal.rec$journal.count)
  Journal.recalpha <- Journal.rec[order(Journal.rec[,1]), ]
  #spread rows so that we can create percent bars that reach 100%
  Journal.recalpha <-  Journal.recalpha %>% spread(Rec.included, journal.count)
  
  Journal.recalpha[is.na(Journal.recalpha)] <- 0
  #create column of totals
  Journal.rec <- Journal.recalpha
  Journal.rec$total <- Journal.rec$Y + Journal.rec$N
  #create percent yes column
  Journal.rec$yespercent <- (Journal.rec$Y/Journal.rec$total)*100
  Journal.rec$nopercent <- (Journal.rec$N/Journal.rec$total)*100
  
  Journal.rec <- Journal.rec[order(-Journal.rec[,4], Journal.rec[,1]), ]
  Journal.rec <- Journal.rec[1:10,]
  
  Journal.rec <- Journal.rec %>% pivot_longer(., cols = c(yespercent, nopercent))
  
  #reorder ascending
  #Journal.rec <- Journal.rec[order(Journal.rec$journal.count , decreasing = TRUE),]
  #sum(Journal.rec$journal.count)
  
  #Journal.rec <- Journal.rec[1:11,]
  #create a column with percent
  #Journal.rec$percent <- (Journal.rec$journal.count/170)*100
  
  #PLOT
  
  Journal.rec <- ggplot(Journal.rec, aes(fill = name , x= Journal, y= value)) +
    geom_bar(stat= 'identity') + theme(axis.text.x = element_text(size= 11), axis.text.y = element_text(size= 12, face= "bold", angle= 10)) +
    theme_hc() + labs(x= "", y= "Percent of Studies") + coord_flip() + scale_fill_brewer(palette = "Set2", labels= c("No", "Yes")) +
    scale_y_continuous(limits = c(0,100))  
  
  
  forest.journal<- Journal.rec + alltheme + theme_legend2 + labs(fill= "Recommendations 
included?") 
  
  #ALL#
  journal.recall<- ggarrange(bird.journalrec, forest.journal,
                             labels = c("A", "B"),
                             ncol = 2, nrow = 1,
                             hjust = -1)
  journal.recall
  
  

################################### FIGURE _ #############################################################
############################## BIRD COMPONENT ################################################
  birdcomponent<- read.csv("bird.component.csv")
  
  birdcomponentplot<- ggplot(birdcomponent, aes(x= reorder(birdomain, -total), y= total)) +
    geom_bar(stat= 'identity') + theme(axis.text.y = element_text(size= 13, angle = 15), 
                                       axis.text.x = element_text(size = 13))+
    theme_hc() + labs(x= "", y= "Number of Studies") + scale_y_continuous(limits = c(0,200), 
                                                                          breaks = c(0, 50, 100, 150, 200)) +
    scale_fill_brewer(palette = "Set2", labels= c("Yes", "No")) + coord_flip()
  
  bird.comparatorplot<- birdcomponentplot + alltheme + theme_legend + labs(fill= "Comparator 
used?")
  
  
  ### URBAN PLOT TROUBLESHOOTING
  
  
  #create column of totals
  Urban$total<- rowSums(Urban[ , c(2:7)], na.rm=TRUE)
  #create a new row of sums for each category
  Urban <- Urban %>%
    adorn_totals("row")
  
  #pivot table
  Urban <- Urban %>% pivot_longer(., cols = c(`Behaviour`, `Biodiversity`, Breeding, `Demographics/Patterns`, Foraging, Resources, Survival))
  Urban <- as.data.frame(Urban)
  Urban
  Urban$percent<- (Urban$value/Urban$total)*100
  
  #remove total rows that are now not needed
  Urban<- Urban[-c(29:35),]
  
  ####
  #inserts rows for totals
  Urban <- Urban %>% add_row(Urb.scale='Total1', 
                             name= 'Total1', ##for behaviour
                             value= 29, 
                             .before=5)
  
  Urban <- Urban %>% add_row(Urb.scale='Total2', 
                             name= 'Total2',                 ## for biodiversity
                             value= 189, 
                             .before=10)
  
  Urban <- Urban %>% add_row(Urb.scale='Total3',            ## for breeding
                             name= 'Total3',
                             value= 49, 
                             .before=15)
  
  Urban <- Urban %>% add_row(Urb.scale='Total4',           ## for demographics/patterns 
                             name= 'Total4',
                             value= 46, 
                             .before= 20)
  
  Urban <- Urban %>% add_row(Urb.scale='Total5',           ## for foraging
                             name= 'Total5',
                             value= 1, 
                             .before= 25)
  
  
  Urban <- Urban %>% add_row(Urb.scale='Total6',           ## for resources
                             name= 'Total6',
                             value= 7, 
                             .before= 30)
  
  Urban <- Urban %>% add_row(Urb.scale='Total7',           ## for survival 
                             name= 'Total7',
                             value= 28, 
                             .before= 35)
  
  Urban
  
###################### RECOMMENDATION TYPE (USED + TYPE) ############################################
  
  #read in files
  rec.type<- read.csv("FAllrec.count.csv")
  no.rec<- read.csv("FNorec.count.csv")
  
  rec.meta<- rbind(rec.type, no.rec)
  
  #remove N/As
  rec.meta<- rec.meta[!grepl("N/A", rec.meta$value),]
  rec.meta<- rec.meta[!grepl("N/A", rec.meta$Rec1),]
  
  #create new column with TOTAL percent
  sum(rec.meta$n)
  rec.meta$totalpercent<- (rec.meta$n/332)*100
  
  #### PLOTTING ##############
  
  rec.plot<- ggplot(rec.meta, aes(fill = Rec1 , x= reorder(value,-totalpercent), 
                                  y= totalpercent)) +
    geom_bar(stat= 'identity') + theme(axis.text.x = element_text(size= 12,), axis.text.y = element_text(size= 13, face = "bold"
                                                                                                         , angle = 15)) +
    theme_hc() + labs(x= "", y= "Percent of Studies") + 
    scale_y_continuous(limits = c(0,40), breaks = c(5, 10, 15, 20, 25, 30, 35, 40)) +
    scale_fill_brewer(palette = "Set2")
  
  theme_legend2<- theme(
    legend.title = element_text(colour = "black", size = 12, face= "bold"),
    legend.position = c(0.70, 0.80),
    legend.box.background =  element_rect(colour = "black"), legend.box.margin = margin(5,5,5,5)
  )
  
  forest.recplot<- rec.plot + alltheme + theme_legend2 + labs(fill = "Recommendation Type") + coord_flip()
  forest.recplot
  
  #ALL#
  all.recplot <- ggarrange(bird.recplot, forest.recplot,
                           labels = c("A", "B"),
                           ncol = 2, nrow = 1,
                           hjust = -1)
  all.recplot
  
  
  ########################## FIGURE _ ###################################################
  ################### Study Country Heat Map ##################################
  
  cutoffs <- data.frame(id = 1:1, 
                        lat_1 = c(23.5, -23.5), 
                        lon_1 = c(-170.5, -170.5), 
                        lat_2 = c(23.5, -23.5),
                        lon_2 = c(170.5, 170.5))
  
  ### AVIAN ###
  av.meta <-read.csv("Heatmap.count.csv")
  
  #subsetting columns we need first
  region.df<- av.meta[,c("COUNTRY","country.count")]
  
  #remove duplicates
  region.df <- region.df[!duplicated(region.df), ]
  
  #remove columns with multiple (don't specificy region)
  #filter rows that contain the string 'Multiple' in the country column
  region.df <- region.df %>% filter(!grepl('Multiple', COUNTRY))
  
  #load shape file with global map including all countries
  mapdata<- st_read("World_Countries_Generalized.shp")
  mapdata$COUNTRY[mapdata$COUNTRY=="United States"]<-"USA"
  
  #merge data with studies countries location from systematic map
  mapdata2<- merge(mapdata, region.df, by="COUNTRY")
  
  #upload layer of simple coordinates of global map for bubblemaps 
  coordmap<- read.csv("countries.csv")
  coordmap$COUNTRY[coordmap$COUNTRY=="United States"]<-"USA"
  
  #merge with country count data
  coordmap<- merge(mapdata2, coordmap, by="COUNTRY")
  
  #PLOTTING     
  #####WITH MID range 
  heatmap<- ggplot(data= mapdata) + geom_sf(color= "white", fill= "lightgrey") +
    geom_sf(data= mapdata2, aes(fill= country.count), color= "white") +
    xlab("Longitude")+ ylab("Latitude") +
    ggtitle("Studies by Country Avian") +
    theme(
      legend.position = "bottom",
      text = element_text(color = "black"),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      plot.title = element_text(size= 12, hjust=0.1, color = "black", 
                                margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),) + 
    guides(
      fill = guide_legend("# of Publications")) + geom_segment(data = cutoffs, 
                                                               aes(x = lon_1, y = lat_1, xend = lon_2, yend = lat_2), 
                                                               color = "black", linewidth = 1.3, lineend = "round") +
    scale_colour_continuous(palette = "BuGN")
  
  heatmap
  
  ### FOREST ###
  forest.meta <-read.csv("FHeatmap.count.csv")
  
  #subsetting columns we need first
  region.df<- forest.meta[,c("COUNTRY","country.count")]
  
  #remove duplicates
  region.df <- region.df[!duplicated(region.df), ]
  
  #remove columns with multiple (don't specificy region)
  #filter rows that contain the string 'Multiple' in the country column
  region.df <- region.df %>% filter(!grepl('Multiple', COUNTRY))
  
  #load shape file with global map including all countries
  mapdata<- st_read("World_Countries_Generalized.shp")
  mapdata$COUNTRY[mapdata$COUNTRY=="United States"]<-"USA"
  
  #merge data with studies countries location from systematic map
  mapdata2<- merge(mapdata, region.df, by="COUNTRY")
  
  #upload layer of simple coordinates of global map for bubblemaps 
  coordmap<- read.csv("countries.csv")
  coordmap$COUNTRY[coordmap$COUNTRY=="United States"]<-"USA"
  
  #merge with country count data
  coordmap<- merge(mapdata2, coordmap, by="COUNTRY")
  
  #PLOTTING     
  #####WITH MID range 
  
  heatmap<- ggplot(data= mapdata) + geom_sf(color= "white", fill= "lightgrey") +
    geom_sf(data= mapdata2, aes(fill= country.count), color= "white") +
    xlab("Longitude")+ ylab("Latitude") +
    ggtitle("Studies by Country Forest") +
    
    theme(
      legend.position = "bottom",
      text = element_text(color = "black"),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      plot.title = element_text(size= 12, hjust=0.1, color = "black", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),) +
    guides(
      fill = guide_legend("# of Publications")) + geom_segment(data = cutoffs, 
                                                               aes(x = lon_1, y = lat_1, xend = lon_2, yend = lat_2), 
                                                               color = "black", linewidth = 1.3, lineend = "round") +
    scale_colour_continuous(palette = "BuGN")
  
  
  heatmap
  
  ########################## FIGURE 3 #########################################################
  ############### # Publication according to Top 10 Journals with Country ###########################################
  
  #AVIAN#
  country.count<- read.csv("Country.count.csv")
  
  #subset columns needed
  journal.df <- country.count[,c("COUNTRY", "Journal", "journal.count")]
  #sort column descending order
  journal.df <- arrange(journal.df, -journal.count)
  #remove duplicates
  journal.df <- journal.df[!duplicated(journal.df), ]
  
  #select only top ten
  journal.df <- journal.df[1:13,]
  
  journal.plot <- ggplot(journal.df, aes(x= reorder(Journal, -journal.count), y= journal.count, fill = COUNTRY)) + 
    geom_bar(stat = 'identity') + theme(axis.text.x = element_text(size= 10)) +
    labs(x= "", y= "Number of Publications") + scale_fill_brewer(palette = "Set2") +
    theme_hc() + theme(axis.text.x = element_text(colour = "black", size= 10.5),
                       axis.text.y = element_text(colour = "black", size = 12, face= "bold"))
  
  
  journal.plotbird<- journal.plot + alltheme + labs(fill = "Country") + theme_legend + coord_flip()
  
  #FOREST#
  
  country.count<- read.csv("FCountry.count.csv")
  
  #subset columns needed
  journal.df <- country.count[,c("COUNTRY", "Journal", "journal.count")]
  #sort column descending order
  journal.df <- arrange(journal.df, -journal.count)
  #remove duplicates
  journal.df <- journal.df[!duplicated(journal.df), ]
  
  #select only top ten
  journal.df <- journal.df[1:14,]
  
  journal.plot <- ggplot(journal.df, aes(x= reorder(Journal, -journal.count), y= journal.count, fill = COUNTRY)) + 
    geom_bar(stat = 'identity')  + labs(x= "", y= "Number of Publications") + scale_fill_brewer(palette = "Set2") +
    theme_hc() + theme(axis.text.x = element_text(colour = "black", size= 10.5), 
                       axis.text.y = element_text(colour= "black", size = 12, face = "bold")) + coord_flip() +
    scale_y_continuous(breaks = c(0,5,10,15), lim = c(0,10))
  
  theme_legend<- theme(
    legend.title = element_text(colour = "black", size = 12, face= "bold"),
    legend.position = c(0.7, 0.8),
    legend.box.background =  element_rect(colour = "black"), legend.box.margin = margin(5,5,5,5))
  
  journal.plotforest<- journal.plot + alltheme + labs(fill = "Country") + theme_legend 
  journal.plotforest
  
  journal.allplot<- ggarrange(journal.plotbird, journal.plotforest,
                              labels = c("A", "B"),
                              ncol = 2, nrow = 1,
                              hjust = -2.3)
  journal.allplot
  

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
  

#BIRD
  
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
  
### COMPARATOR CLEANING BIRD
  Comp<- read.csv("Comp.count.csv")
  #remove n/a's created from empty rows
  Comp <-Comp[!grepl('N/A', Comp$Comparator),]
  Comp <-Comp[!grepl('N/A', Comp$value),]
  Comp <- Comp %>% drop_na(Comparator)
  Comp <- Comp %>% drop_na(value)
  
  #sort alphabetically to create percentages
  Comp <- Comp[order(Comp[,2]), ]
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

  
  ###### COUNTRY ###################################################################################################################################
  
  #subset necessary columns
  df<- subset(forest.meta, select = c(COUNTRY, Journal))
  
  #COUNT TABLE
  #journal name count by study country
  forestcountrycount <- df %>%
    group_by(COUNTRY, Journal) %>%
    dplyr::mutate(journal.count= n())
  
  #PUSH OUT CSV
  write.csv(forestcountrycount, 
            "C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/out/FCountry.count.csv", row.names = FALSE)
  
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
  
## FROM figure avian plots
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
  

  
  
  #PLOT 
  
  duration.plot <- ggplot(Duration, aes(x= reorder(duration_bin, -percent) , y= percent)) +
    geom_bar(stat= 'identity', fill = birdcol, width = 0.6) + 
    theme_hc() + labs(x= "Time-scale considered", y= "Percent of Studies") +
    
    theme(axis.text.x = element_text(size = 11, angle = -20), axis.ticks.x = element_blank()) + 
    
    scale_x_discrete(labels = c("[0-1 years]", "[1-5 years]", "[5-10 years]", "[10-100 years]")) +
    scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50), limits = c(0,55))
  
  
  bird.durationplot<- duration.plot + alltheme
  bird.durationplot
  
  #FOREST#
  Duration2<- read.csv("Studydurationall.csv")
  sum(Duration2$n)
  
  #create column for percentage for plotting
  Duration2$percent <- ((Duration2$n/169)*100)
  
  duration.plot <- ggplot(Duration2, aes(x= reorder(duration_bin, -percent) , y= percent)) +
    geom_bar(stat= 'identity', position = 'dodge', fill= forestcol, width = 0.6) + theme_hc() + labs(x= "Time-scale considered", y= "Percent of Studies") +
    
    theme(axis.text.x = element_text(size = 11, angle = -20), axis.ticks.x = element_blank()) + 
    
    scale_x_discrete(labels = c("[0-1 years]", "[1-5 years]", "[5-10 years]", "[10-100 years]")) + 
    scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50), limits = c(0,55))
  
  forest.durationplot<- duration.plot + alltheme 
  forest.durationplot
  
  #COMBINE
  durationall<- ggarrange(bird.durationplot, forest.durationplot,
                          labels = c("A", "B"),
                          ncol = 2, nrow = 1,
                          hjust = -1,
                          common.legend = TRUE)
  durationall
  
  ### INTERVENTION FIGURE
  forestpalette5 <- c("#1F968BFF", "#29AF7FFF", "#55C667FF", "#B8DE29FF", "#FDE725FF")
  
  Indicator.ct<- read.csv("FAllind.count.csv")
  sum(Indicator.ct$number_publications)
  #add column with percents
  Indicator.ct$percent<- (Indicator.ct$number_publications/277)*100
  
  
  indicator.plot <- ggplot(Indicator.ct, aes(x= reorder(number_topics, -percent), y= percent)) +
    geom_bar(stat= 'identity', fill= forestpalette5) + theme(axis.text.x = element_text(size= 10)) +
    theme_hc() + labs(x = "", y=" Percent of Studies") + scale_y_continuous(limits = c(0,80))
  forest.indicatorplot<- indicator.plot + alltheme +coord_flip()
  
  all.indicatorplot<- ggarrange(bird.indicatorplot, forest.indicatorplot,
                                labels = c("Bird", "Forest"),
                                font.label = list(color= "black"),
                                ncol = 2, nrow = 1)
  all.indicatorplot

  
  ######### Categories of forest management only count ##############
  
  #COUNT TABLE
  #including all bird domain columns grouped by RECOMMENDATION TYPE 
  
  #REC 1
  birdforest1 <-as.data.frame(av.meta %>%
                                pivot_longer(cols = c(birdcomp1:birdcomp4)) %>%
                                dplyr::count(forest1, value))
  
  colnames(birdforest1) <- c("forest", "birdcomp", "count")
  
  #clean and remove N/As
  birdforest1 <- birdforest1 %>% drop_na(birdcomp)
  birdforest1 <- birdforest1 %>% drop_na(forest)
  
  #REC 2
  birdforest2 <-as.data.frame(av.meta %>%
                                pivot_longer(cols = c(birdcomp1:birdcomp4)) %>%
                                dplyr::count(forest2, value))
  colnames(birdforest2) <- c("forest", "birdcomp", "count")
  
  #remove empty cells from studies with only 1 recommendation ##CHECK CHECK DOUBLE CHECK
  birdforest2 <- birdforest2 %>% drop_na(birdcomp)
  birdforest2 <- birdforest2 %>% drop_na(forest)
  
  #REC 3
  birdforest3 <-as.data.frame(av.meta %>%
                                pivot_longer(cols = c(birdcomp1:birdcomp4)) %>%
                                dplyr::count(forest3, value))
  colnames(birdforest3) <- c("forest", "birdcomp", "count")
  
  birdforest3 <- birdforest3 %>% drop_na(birdcomp)
  birdforest3 <- birdforest3 %>% drop_na(forest)
  
  #FOREST 4
  birdforest4 <-as.data.frame(av.meta %>%
                                pivot_longer(cols = c(birdcomp1:birdcomp4)) %>%
                                dplyr::count(forest4, value))
  colnames(birdforest4) <- c("forest", "birdcomp", "count")
  
  birdforest4 <- birdforest4 %>% drop_na(birdcomp)
  birdforest4 <- birdforest4 %>% drop_na(forest)
  
  birdforestall <- bind_rows(birdforest1, birdforest2, birdforest3, birdforest4) %>% 
    group_by(forest, count) %>% 
    distinct(.keep_all = TRUE)
  birdforestall<- as.data.frame(birdforestall)
  unique(birdforestall$forest)
  
  #resort by category
  #birdforestall <- as.data.frame(birdforestall)
  birdforestall <- birdforestall[order(birdforestall[,"forest"]), ]
  
  #manually add a column of totals
  totals<- c(106,106,106,106,106,106,106,106,106,106,106,106,106,106,106,206,206,206,206,206,206,206,206,206,206,206,206,206,206,206,206,206,
             26,26,26,26,26,26,26,26,23,23,23,23,23,23,23,24,24,24,24,24,24,24,24,24,159,159,159,159,159,159,159,159,159,159,159,159,159,
             159,159,159,19,19,19,19,19,19,19,19,19,19,22,22,22,22,22,22,22,22,22,22,22,122,122,122,122,122,122,122,122,122,122,122,122,
             122,122,122,122,122,122,27,27,27,27,27,27,27,27,27,27)
  #bind
  bfall<- cbind(birdforestall, totals)
  
  bfall$percent<- (bfall$count/bfall$totals)*100
  
  #PUSHOUT
  write.csv(bfall, "out/birdforestall.csv", row.names = FALSE)
  
  