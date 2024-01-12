#packagesneeded
source('scripts/0-packages.R')

# themes 
source('scripts/0-themes.R')

# Figure 3 ----------------------------------------------------------------
# Publications according to Top 10 Journals with Country


world<- map_data("world")

cutoffs <- data.frame(id = 1:1, 
                      lat_1 = c(23.5, -23.5), 
                      lon_1 = c(-170.5, -170.5), 
                      lat_2 = c(23.5, -23.5),
                      lon_2 = c(170.5, 170.5))

data <- read.csv("out/data.csv")
# Reorder data to show biggest cities on top
data <- data %>%
  arrange(-count)
mybreaks <- c(1,5,15,35,95) 

# Build the map

av.map<- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=longitude, y=latitude, size=count, color=count), shape=20, stroke=FALSE) + 
  geom_text_repel(data=data %>% arrange(count) %>% tail(10), aes(x=longitude, y=latitude, label=COUNTRY),     size=5, box.padding = 0.7, family = "serif", segment.color="grey50", max.overlaps = Inf) +
  scale_size_continuous(range=c(2.5,16), breaks=mybreaks,name="Number of Studies") +
  scale_color_viridis_c(option="viridis", end = 1, begin = 0.1, breaks=mybreaks, direction=-1, name="Number of Studies") +
  theme_void() +  
  coord_quickmap() +
  ggtitle("") + 
  theme(legend.position ="bottom",legend.direction = "horizontal",
        text = element_text(color = "black", size=15, family = "serif"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.background = element_rect(fill = "white", color = NA), 
        legend.background = element_rect(fill = "white", color = NA)) +
  guides(color = guide_legend(nrow=1, byrow=TRUE)) +
  geom_segment(data = cutoffs,aes(x = lon_1, y = lat_1, xend = lon_2, yend = lat_2), color = "grey", alpha           = 0.5, linewidth = 0.6, linetype=2 , lineend = "round") 

av.map

# Reorder data to show biggest cities on top

data1<- read.csv("out/data1.csv")

data1 <- data1 %>%
  arrange(count)

mybreaks <- c(1,5,15,25,60)

# Build the map
for.map<-  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data1, aes(x=longitude, y=latitude, size=count, color=count), shape=20, stroke=FALSE) + 
  geom_text_repel(data=data1 %>% arrange(count) %>% tail(10), aes(x=longitude, y=latitude, label=COUNTRY),     size=5, box.padding = 0.7, family = "serif", segment.color="grey50", max.overlaps = Inf) +
  scale_size_continuous(range=c(2.5,16), breaks=mybreaks,name="Number of Studies") +
  scale_color_viridis_c(option="viridis", end = 0.8, begin = 0.3, breaks=mybreaks, direction=-1, name="Number of Studies") +
  theme_void() +  
  coord_quickmap() +
  ggtitle("") + 
  theme(legend.position ="bottom",legend.direction = "horizontal",
        text = element_text(color = "black", size=15,family = "serif"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.background = element_rect(fill = "white", color = NA), 
        legend.background = element_rect(fill = "white", color = NA)) +
  guides(color = guide_legend(nrow=1, byrow=TRUE)) +
  geom_segment(data = cutoffs,aes(x = lon_1, y = lat_1, xend = lon_2, yend = lat_2), color = "grey",          alpha = 0.5, linewidth = 0.6, linetype=2 , lineend = "round") 

for.map

av_for_maps<- plot_grid(av.map, for.map)
av_for_maps

ggsave(filename ="graphics/Figure5.png", width = 290, units="mm", height = 90 , dpi=300)  

