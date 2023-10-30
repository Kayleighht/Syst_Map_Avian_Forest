#packagesneeded
Packages <- c("tidyverse", "ggplot2", "maps", "bibliometrix", "ggthemes", "cartography", "sf", "stringr")
lapply(Packages, library,character.only= TRUE)

getwd()
setwd("C:/Users/KHUTTTAY/Documents/Systematic_Map_Avian_Forest/Syst_Map_Avian_Forest/in")

birdexcl<- read.csv("articles_avian.csv")
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
birdex4 <- subset(birdex4, select=-c(notes_ok,notes_pieces, notes_remainder, notes))

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
