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
  
