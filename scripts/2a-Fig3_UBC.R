#packagesneeded
source('scripts/0-packages.R')

# themes 
source('scripts/0-themes.R')

journal.df.av <- read.csv("out/journal.df.av.csv")
journal.df.av$percent <- (journal.df.av$journal.count/277*100)

# Figure 3 ----------------------------------------------------------------
# Study Country Heat Map

#read file
bird<-readPNG("graphics/bird.1.png")

av.plot<- journal.df.av %>%
  filter(!is.na(COUNTRY)) %>%
  ggplot() +
  aes(x = reorder(Journal, count), y = percent, fill = COUNTRY) +
  geom_col() + 
  scale_fill_viridis_d(direction=-1, end = 1, begin = 0.1, name="Country") +
  labs(x= "", y= "") +
  coord_flip() + alltheme + theme_legend4 + 
  theme(axis.text = element_text(size = 18, colour = "black")) +
  add_phylopic(bird, alpha = 1, x = 2, y = 35.4, ysize =0.8) + 
  scale_y_continuous(breaks = c(0, 5, 10, 15), limits = c(0,15))
av.plot

#read file
journal.df <- read.csv("out/journal.df.csv")
journal.df$percent <- (journal.df$journal.count/169*100)
tree<-readPNG("graphics/tree.1.png")

for.plot <-journal.df %>%
  filter(!is.na(COUNTRY)) %>%
  ggplot() +
  aes(x = reorder(Journal, count), y = percent, fill = COUNTRY) +
  geom_col() + scale_fill_viridis_d(direction=-1, end = 1, begin = 0.1, name="Country") +
  labs(x= "", y= "Percent of Publications") +
  coord_flip() + alltheme + theme_legend4 + theme(axis.text = element_text(size = 18, colour = "black")) +
  add_phylopic(tree, alpha = 1, x = 1.8, y = 20.4, ysize =0.8) + 
  scale_y_continuous(breaks = c(0, 5, 10, 15), limits = c(0,15))

for.plot

#combined plot
av_for_maps<- plot_grid(av.plot, for.plot, align = "v", nrow = 2, ncol= 1)
av_for_maps

ggsave(filename ="graphics/Figure3.png", width = 350, units="mm", height = 260 , device='tiff', dpi=300)  
