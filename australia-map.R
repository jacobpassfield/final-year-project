# Loading necessary libraires
library(tidyverse)
library(ggmap)
# Ensure to cite ggmap "citation("ggmap")"

# Load data
load(file = "data/data.RData")

# Geogroup consists of several surveys with different latitude and longitude
# coordinates. Find the average longitude and latitude cordinates for each geogroup.
lonlat_data <- data %>%
  group_by(Geogroup) %>%
  summarise(Latitude = mean(SiteLat), Longitude = mean(SiteLong), records = n())

# Find minimum and maximum longitude and latitude coordinates to aid the 
# creation of a bounding box.
which.max(lonlat_data$Latitude)
lonlat_data[284,]$Latitude # -9.527617
which.min(lonlat_data$Latitude)
lonlat_data[2,]$Latitude # -43.61616
which.max(lonlat_data$Longitude)
lonlat_data[136,]$Longitude # 167.9488
which.min(lonlat_data$Longitude)
lonlat_data[266,]$Longitude # 105.6571

# Creating a bounding box
bbox <- c(103, -45, 170, -5)

# Retrieve map of Australia from Stamen Maps
map <- get_map(location = bbox, source = "stamen", maptype = "toner-lite")
# Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL.

# Check the map
(ausMap <- ggmap(map))

# Match data frame with bounding box
lonlatAus <- lonlat_data %>% 
  filter(bbox[1] <= Longitude, Longitude <= bbox[3], bbox[2] <= Latitude, Latitude <= bbox[4])
# Overlaying geogroups onto map
ausMap +
  geom_point(aes(x = Longitude, y = Latitude, size = records), shape = 20, colour = "red", data = lonlatAus) +
  scale_size(range = c(1, 15), name="Number of records") +
  theme(legend.position="bottom")


# SAVE MAP

# pdf to call the plot
pdf(file = "Figure1.pdf")

# Create the plot
ausMap +
  geom_point(aes(x = Longitude, y = Latitude, size = records), shape = 20, colour = "red", data = lonlatAus) +
  scale_size(range = c(1, 15), name="Number of records") +
  theme(legend.position="bottom")

# Create the file
dev.off()


