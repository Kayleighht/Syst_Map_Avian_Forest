#packagesneeded
source('scripts/0-packages.R')

birdexcl<- read.csv("in/articles_avian.csv")
birdexcl <- separate_wider_delim(birdexcl, cols = notes , delim = "|", names = c("notes", "decision", "reason"),
                               too_few = "align_start", too_many = "debug")

#remove unwanted columns
birdexcl <- subset(birdexcl, select=-c(notes_ok,notes_pieces))

birdex1<- birdexcl[c(1:67),]

#separate misaligned section of data to clip and then rebind
birdex2<- birdexcl[c(68:1454),]
#remove extra column
birdex2<- subset(birdex2, select = -reason)
colnames(birdex2) <- c("key", "title", "authors", "notes", "decision", "reason")

#last chunk of misalgined dataframe
birdex3<- birdexcl[c(1455:1486),]
birdex3 <- birdex3[-c(12,23),]
birdex4<- birdex3[c(12,23),]

#remove irrelevant column in birdex4
birdex4 <- subset(birdex4, select=-c(notes_remainder, notes))

#recombine
colnames(birdex3) <- c("key", "title", "authors", "journal2", "notes", "decision", "reason")

df_merge12 <- merge(birdex1,birdex2,by=c("key", "title", "authors", "reason"),all.x=TRUE, all.y = TRUE) 
df_merge23 <- merge(df_merge12,birdex3, by=c("key", "title", "authors", "reason"),all.x=TRUE, all.y = TRUE)
df_merge <- merge(df_merge23, birdex4,  by=c("key", "title", "authors", "reason"),all.x=TRUE, all.y = TRUE)

mergedbirdex<- subset(df_merge, select = c("key", "title", "reason"))

#remove character string from reason column before separating
mergedbirdex$reason <- gsub("RAYYAN-EXCLUSION-REASONS: ", "", mergedbirdex$reason)
#search for values that contain relevant review to remove
relevantreviews <- mergedbirdex[grep("relevant review", mergedbirdex$reason), ]
#remove them from dataframe
mergedbirdex<- mergedbirdex[!grepl("relevant review", mergedbirdex$reason), ]
#remove inaccessible/not reviewed articles
notretrieved <- mergedbirdex[grep("inaccessible", mergedbirdex$reason), ]
mergedbirdex<- mergedbirdex[!grepl("inaccessible", mergedbirdex$reason), ]


#now separate by comma and only use first exclusion tag
birdexclean <- separate_wider_delim(mergedbirdex, cols = reason , delim = ",", names = c("reason1", "reason2", "reason3", "reason4"),
                                 too_few = "align_start")

birdreasons<- birdexclean %>%
  dplyr:: count(reason1)
birdreasons

# Forest ------------------------------------------------------------------


forestexcl<- read.csv("in/articles_forest.csv")
#remove uneeded columns
forestexcl<- subset(forestexcl, select = c(title, notes))


forestexcl <- separate_wider_delim(forestexcl, cols = notes , delim = "|", names = c("notes1", "notes2", "notes3", "notes4"),
                                 too_few = "align_start", too_many = "debug")

#search for matching # of notes to subset and then recombine (2,3,4 etc)
forestex1 <- forestexcl[grep("2", forestexcl$notes_pieces), ]
#remove columns not needed
forestex1<- subset(forestex1, select = -c(notes, notes4, notes4, notes_ok, notes_pieces, notes_remainder))
colnames(forestex1) <- c("title", "notes", "decision", "comments")

forestex2 <- forestexcl[grep("3", forestexcl$notes_pieces), ]
#remove columns not needed
forestex2<- subset(forestex2, select = -c(notes, notes2, notes4, notes_ok, notes_pieces, notes_remainder))
colnames(forestex2) <- c("title", "journal", "decision")

forestex3 <- forestexcl[grep("4", forestexcl$notes_pieces), ]
#remove columns not needed
forestex3<- as.data.frame(subset(forestex3, select = -c(notes, notes2, notes3, notes_ok, notes_pieces)))
colnames(forestex3) <- c("title", "journal", "decision")

##MERGING

df_merge12 <- merge(forestex1, forestex2, by=c("title", "decision"),all.x=TRUE, all.y = TRUE) 
df_merge12<- df_merge12[,-4]
forestmergedex <- merge(df_merge12,forestex3, by=c("title", "decision"),all.x=TRUE, all.y = TRUE)
forestmergedex<- subset(forestmergedex, select = c(title, decision))

#remove character string from reason column before separating
forestmergedex$decision <- gsub("| RAYYAN-EXCLUSION-REASONS: ", "", forestmergedex$decision)
#search for values that contain relevant review to remove
relevantreviews <- forestmergedex[grep("relevant review", forestmergedex$decision), ]
#remove them from dataframe
mergedforestex<- forestmergedex[!grepl("relevant review", forestmergedex$decision), ]
#remove inaccessible/not reviewed articles
notretrieved <- forestmergedex[grep("inaccessible", forestmergedex$decision), ]
forestmergedex<- forestmergedex[!grepl("inaccessible", forestmergedex$decision), ]


#now separate by comma and only use first exclusion tag
forestexclean <- separate_wider_delim(forestmergedex, cols = decision , delim = ",", names = c("reason1", "reason2", "reason3", "reason4"),
                                    too_few = "align_start")

forestreasons<- forestexclean %>%
  dplyr:: count(reason1)
forestreasons

###############################################################################################################
####################################### GREY LIT ##############################################################
###############################################################################################################
##############################################################################################################
###############################################################################################################


#BIRD##
bgreyexcl<- read.csv("in/greylitarticles.csv")
bgreyexcl <- separate_wider_delim(bgreyexcl, cols = notes , delim = "|", names = c("1", "2", "label", "reason", "5", "6", "7"),
                                 too_few = "align_start", too_many = "debug")

#choose the right columns
birdgrey <- subset(bgreyexcl, select=c("title","label","reason"))

#remove character string from reason column before separating
birdgrey$label <- gsub("RAYYAN-LABELS:", "", birdgrey$label)
birdgrey$reason <- gsub("RAYYAN-EXCLUSION-REASONS:", "", birdgrey$reason)

#remove inaccessible/not reviewed articles
notretrieved <- birdgrey[grep("inaccessible", birdgrey$reason), ]
birdgrey<- birdgrey[!grepl("inaccessible", birdgrey$reason), ]

#separate by labels
birdgrey1<- birdgrey[grep("bird", birdgrey$label),]
forestgrey<- birdgrey[grep("forest", birdgrey$label),]

#now separate by comma and only use first exclusion tag
birdgreyclean <- separate_wider_delim(birdgrey1, cols = reason , delim = ",", names = c("reason1", "reason2", "reason3", "reason4"),
                                      too_few = "align_start", too_many = "debug")

birdcountgrey<- birdgreyclean %>%
  dplyr:: count(reason1)
birdcountgrey

#now separate by comma and only use first exclusion tag
forestgreyclean <- separate_wider_delim(forestgrey, cols = reason , delim = ",", names = c("reason1", "reason2", "reason3", "reason4"),
                                      too_few = "align_start", too_many = "debug")

forestcountgrey<- forestgreyclean %>%
  dplyr:: count(reason1)
forestcountgrey

#### PHASE 2 GREY LIT #############################################################
#########################################################################################

#BIRD##
phase2grey<- read.csv("in/phase2grey_articles.csv")

phase2grey<- subset(phase2grey, select = c(title, notes))

phase2grey <- separate_wider_delim(phase2grey, cols = notes , delim = "|", names = c("notes1", "notes2", "notes3", "notes4"),
                                   too_few = "align_start", too_many = "debug")


#remove inaccessible/not reviewed articles
notretrieved <- birdgrey[grep("inaccessible", birdgrey$reason), ]
birdgrey<- birdgrey[!grepl("inaccessible", birdgrey$reason), ]

#separate by labels
birdgrey1<- birdgrey[grep("bird", birdgrey$label),]
forestgrey<- birdgrey[grep("forest", birdgrey$label),]
