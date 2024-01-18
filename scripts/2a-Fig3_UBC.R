#packagesneeded
source('scripts/0-packages.R')

# themes
source('scripts/0-themes.R')

journal.df.av <- read.csv("out/journal.df.av.csv")
journal.df.av$percent <- (journal.df.av$journal.count/277*100)

# Figure 3 ----------------------------------------------------------------
# Study Country Heat Map

#read file
bird<-readPNG("graphics/bird.1.png", native = T)

av.plot<- journal.df.av %>%
  filter(!is.na(COUNTRY)) %>%
  ggplot() +
  aes(x = reorder(Journal, count), y = percent, fill = COUNTRY) +
  geom_col() +
  scale_fill_viridis_d(direction=-1, end = 1, begin = 0.1, name="Country") +
  labs(x= "", y= "") +
  coord_flip(clip = 'off') + alltheme + theme_legend4 +
  theme(axis.text = element_text(size = 18, colour = "black")) +
  annotation_custom(grid::rasterGrob(bird,
                                     width=ggplot2::unit(1,"npc"),
                                     height=ggplot2::unit(1,"npc")),
                    ymin = -7.6, ymax = -9.1, xmin = 12.5, xmax = 13.5) +
  scale_y_continuous(breaks = c(0, 5, 10, 15), limits = c(0,15))
av.plot

#read file
journal.df <- read.csv("out/journal.df.csv")
journal.df$percent <- (journal.df$journal.count/169*100)
tree<-readPNG("graphics/tree.1.png", native = T)

for.plot <-journal.df %>%
  filter(!is.na(COUNTRY)) %>%
  ggplot() +
  aes(x = reorder(Journal, count), y = percent, fill = COUNTRY) +
  geom_col() + scale_fill_viridis_d(direction=-1, end = 1, begin = 0.1, name="Country") +
  labs(x= "", y= "Percent of Publications") +
  coord_flip(clip = 'off') +
  alltheme +
  theme_legend4 +
  theme(axis.text = element_text(size = 18, colour = "black")) +
  annotation_custom(grid::rasterGrob(tree,
                                     width=ggplot2::unit(1,"npc"),
                                     height=ggplot2::unit(1,"npc")),
                    ymin = -7.7, ymax = -8.7, xmin = 12.0, xmax = 13.5) +
  scale_y_continuous(breaks = c(0, 5, 10, 15), limits = c(0,15))

for.plot


#combined plot
av_for_maps<- plot_grid(av.plot, for.plot, align = "v", nrow = 2, ncol= 1)
av_for_maps

ggsave(filename ="graphics/Figure3.png", width = 350, units="mm", height = 260 , device='tiff', dpi=300)  


