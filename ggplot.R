# R workshop - Intro to ggplot
# Authors: Juliana Balluffi-Fry, Lionel Leston, and Simran Bains

# Install packages and load libraries
install.packages("ggplot2")
install.packages("tidyverse")
library(ggplot2)
library(tidyverse)

# read in data and view the data
biomass <- read.csv("input/biomass.csv") # File can be found on Juliana's GitHub Teaching R workshop repository
head(biomass) #OR
View(biomass)


# explore data ------------------------------------------------------------
# 3 main things you need to use ggplot
# 1) Data
# 2) Aesthetic mapping: what is being mapped to x and y?
# 3) Using layers
# Let's start by taking a look at the relationship between shrub biomass and canopy cover using a scatterplot
ggplot(data = biomass, mapping = aes(x = canopy_cover, y = shrub_biomass)) + # Layers are added using the +
  geom_point()

#shortened way:
ggplot(biomass, aes(x = canopy_cover, y = shrub_biomass)) + 
  geom_point()

# other plots:
ggplot(data = biomass, aes(x = SA, y = shrub_biomass)) + 
  geom_boxplot() #geom_violin

# change the point size:
ggplot(biomass, aes(x = canopy_cover, y = shrub_biomass)) +
  geom_point(cex = 2)

# change the shape to diamonds
ggplot(biomass, aes(x = canopy_cover, y = shrub_biomass)) +
  geom_point(cex = 2, shape = 5)

# Change points to a different colour:
#change point colour
ggplot(data = biomass, aes(x = canopy_cover, y = shrub_biomass)) + 
  geom_point(colour = "blue")

# Change colour based on SA
ggplot(biomass, aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_point()

# NOTE: aes() inside the ggplot() will apply to all layers 
# Let’s demonstrate by adding a best-fit line in a new layer
ggplot(biomass, aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_point() +
  geom_smooth(method = "lm")

# If the aesthetic is within a layer, it applies to the one layer only
# What if I want a best fit line that goes through all the points
ggplot(biomass, aes(x = canopy_cover, y = shrub_biomass)) +
  geom_point(aes(colour = SA)) +
  geom_smooth(method = "lm")

# If you wnat the geom_smooth() to know to provide a different line for each class
ggplot(biomass, aes(x = canopy_cover, y = shrub_biomass)) +
  geom_point(aes(colour = SA)) +
  geom_smooth(aes(group = SA), method = "lm") # geom_smooth(aes(colour = SA), method = "lm")

# NOTE: Order of layers matter!
ggplot(biomass, aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_smooth(method = "lm") +
  geom_point()

# Using themes ---------------------------------------
# If you want a white background
ggplot(biomass, aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() # theme_dark(), theme_minimal(), theme_classic()

# making your own theme:
?theme
ggplot(biomass, aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw(base_family = "Times New Roman") +
  theme(panel.grid = element_blank(), # remove grid lines
        axis.title = element_text(size = 15), # axis title
        axis.text = element_text(size = 13), # axis text size
        panel.border = element_rect(size = 2), # darker border
        legend.position = c(0.83, 0.83) # change legend position
  )

# Save custom theme as object (can save time if you have to make multiple graphs in the same style)
theme.custom <- theme_bw(base_family = "Times New Roman") +
  theme(panel.grid = element_blank(), # remove grid lines
        axis.title = element_text(size = 15), # axis title
        axis.text = element_text(size = 13), # axis text size
        panel.border = element_rect(size = 2), # darker border
        legend.position = c(0.83, 0.83) # change legend position
  )

# remake plot with saved theme:
ggplot(biomass, aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  theme.custom

# Changing axis titles and legend title:
ggplot(biomass, aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  theme.custom + 
  xlab("Canopy cover (%)") +
  ylab("Shrub biomass (kg/ha)") + 
  labs(color = "Study area")

# OR
ggplot(biomass, aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  theme.custom + 
  scale_x_continuous("Canopy cover (%)") +
  scale_y_continuous("Shrub biomass (kg/ha)", labels = scales::comma) +
  labs(color = "Study area")


# Exporting graphs --------------------------------------------------------
final.plot <- ggplot(biomass, aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  theme.custom + 
  labs(color = "Study area") +
  scale_x_continuous("Canopy cover (%)") +
  scale_y_continuous("Shrub biomass (kg/ha)", labels = scales::comma) #add comma to thousands separator

ggsave(filename = "final_plot1.jpeg", 
       final.plot,
       width = 7,
       height = 4.5,
       units = "in"
)


# Multi-panel plotting ----------------------------------------------------
ggplot(biomass, aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~SA) +
  theme.custom +
  theme(strip.background = element_blank()) # if you want to remove the grey strip on top of the graphs


# Making maps -------------------------------------------------------------
# Install more packages:
install.packages("tmap")
install.packages("sf")
install.packages("ggspatial")
install.packages("terra")
library(sf)
library(tmap)
library(ggspatial)
library(terra)

# To make map, you need to decide your map projection (how you represent 3D structure of the planet in 2D)
# Projection where going with is Lambert conformal conical:
lcc_crs <- "+proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

# Import shapefiles of NA and project to LCC
NAmerica<-st_read("input/boundaries/BCR_Terrestrial_master.shp")%>%
  st_transform(lcc_crs)%>%
  group_by(COUNTRY)%>%
  summarize()#dissolve polygons within countries
plot(NAmerica["COUNTRY"])

provs <-  st_read("input/boundaries/BCR_Terrestrial_master.shp")%>%
  filter(COUNTRY == "CANADA")%>%  #limit to Canada
  st_transform(lcc_crs)%>%  #reproject as Lambert Conformal Conic
  group_by(PROVINCE_S)%>%
  summarize()#dissolve provinces within Canadian provinces
plot(provs["PROVINCE_S"])

alberta<-provs%>%
  filter(PROVINCE_S == "ALBERTA")  #limit to Alberta 
#already dissolved polygons within provinces, so no need to dissolve them here
plot(alberta)


#add city
City <- data.frame(ID = c("Edmonton","Hinton", "Sundre", "Lac La Biche"), 
                   lat = c(53.5461, 53.4111, 51.7972, 54.7702), 
                   lon = c(-113.4937, -117.5627, -114.6405, -111.9791))
city.sf <- st_as_sf(x = City,                         
                    coords = c("lon", "lat"),
                    crs = "+proj=longlat +ellps=WGS84
+datum=WGS84 +no_defs +towgs84=0,0,0")
city.sf<-city.sf%>%
  st_transform(lcc_crs)
str(city.sf)

#Alberta label
Alberta.Pt<- data.frame(ID = "ALBERTA" , lat = 57, lon = -113.4937)
Alberta.sf <- st_as_sf(x = Alberta.Pt,                         
                       coords = c("lon", "lat"),
                       crs = "+proj=longlat +ellps=WGS84 
+datum=WGS84 +no_defs +towgs84=0,0,0")%>%
  st_transform(lcc_crs)

# Using ggplot to make a nice map
Albertamap <- ggplot() + 
  geom_sf(data = NAmerica, size = 0.5, color = "black", fill = "white") + 
  geom_sf(data = provs, size = 0.5, color = "black", fill = "white") + 
  geom_sf(data = alberta, size = 0.25, color = "black", fill = "lightyellow") + 
  ggtitle("Map of Alberta") +
  guides(fill="none")+
  geom_sf(data = city.sf, 
          shape=22, #square symbol instead of solid circle(default)
          size = 1, 
          color = "black", 
          fill = "black") +
  geom_sf_text(data=city.sf, aes(label = ID), size=3, nudge_y = 50000, nudge_x = 50000)+
  geom_sf(data = Alberta.sf, 
          color = "lightyellow", 
          fill = "lightyellow")+ #we want Alberta point on map but invisible
  geom_sf_text(data=Alberta.sf, aes(label = ID), size=3)+
  labs(y="Latitude", x="Longitude") +
  xlim(-1585420.5, -806054.1) + ylim(6713525.8, 8052465.1) + 
  #crop map to Alberta's bounding box
  annotation_scale()+ #add scale bar
  annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"))+ #add a north arrow
  theme_bw()

# Save your map!
ggsave("AlbertaMap.png", Albertamap, width=6, height=8, units="in", dpi=300)







