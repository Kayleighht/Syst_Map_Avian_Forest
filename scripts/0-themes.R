# Theme Creation ----------------------------------------------------------


alltheme <- theme(
  panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.background = element_rect(fill = "white", color = NA),
  plot.background = element_rect(fill = "white", color = NA),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
  text = element_text(family = "serif"),
  axis.title.y = element_text(colour = "black",face = "bold", size = '40', vjust = +1),
  axis.title.x = element_text(colour = "black", face = "bold", size= '40', vjust= 0),
  axis.ticks = element_line(size = 0.7),
  axis.text = element_text(size = 28, colour = "black")
)

bigtheme <- theme(
  panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.background = element_rect(fill = "white", color = NA),
  plot.background = element_rect(fill = "white", color = NA),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
  text = element_text(family = "serif"),
  axis.title.y = element_text(colour = "black",face = "bold", size = '200', vjust = +1),
  axis.title.x = element_text(colour = "black", face = "bold", size= '200', vjust= 0),
  axis.ticks = element_line(size = 0.7),
  axis.text = element_text(size = 170, colour = "black")
)


#LEGEND 1 THEME
theme_legend<- theme(
  legend.title = element_text(colour = "black", size = 12, face= "bold"),
  legend.position = c(0.85, 0.80),
  legend.box.background =  element_rect(colour = "black"), legend.box.margin = margin(5,5,5,5))

#LEGEND 2 THEME
theme_legend2<- theme(
  legend.title = element_text(colour = "black", size = 12, face= "bold"),
  legend.position = c(0.70, 0.80),
  legend.box.background =  element_rect(colour = "black"), legend.box.margin = margin(5,5,5,5))

#LEGEND 3 THEME
theme_legend3<- theme(
  legend.title = element_text(colour = "black", size = 120, face= "bold"),
  legend.text = element_text(colour = "black", size = 110),
  legend.position = c("right"),
  legend.key.size = unit(3.5, "cm"),
  legend.box.background =  element_rect(colour = "black", size = 3), 
  legend.box.margin = margin(5,5,5,5))

#LEGEND 4 THEME
theme_legend4<- theme(
  legend.title = element_text(colour = "black", size = 12, face= "bold"),
  legend.position = c(0.90,0.55),
  legend.box.background =  element_rect(colour = "black"), legend.box.margin = margin(5,5,5,5))

#LEGEND 5 THEME
theme_legend5<- theme(
  legend.key.size = unit(2, 'cm'),
  legend.title = element_text(colour = "black", size = 15, face= "bold"),
  legend.position = c(0.88,0.85), ## (h,v)
  legend.text = element_text(size=15),
  legend.box.background =  element_rect(colour = "black"), legend.box.margin = margin(5,5,5,5))


forestcol<- ("#DCE319FF")
birdcol<- ("#440154FF")

birdpalette2<- c("#481567FF","#404788FF")
forestpalette2<- c("#B8DE29FF", "#55C667FF")
birdpalette4 <- c("#481567FF", "#404788FF", "#33638DFF", "#238A8Dff")
forestpalette4 <- c("#20A387FF", "#55C667FF", "#B8DE29FF", "#FDE725FF")
forestpalette5 <- c("#1F968BFF", "#29AF7FFF", "#55C667FF", "#B8DE29FF", "#FDE725FF")
