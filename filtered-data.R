# Loading necessary libraries
library(tidyverse)

# Loading main data
load(file = "data/fish_data.RData")

selected_data <- main_data %>%
  rename("TaxonomicName" = "TAXONOMIC_NAME", "SpeciesEpithet" = "SPECIES_EPITHET",
         "Genus" = "GENUS", "Family" = "FAMILY", "Order" = "ORDER", "Day" = "day",
         "Month" = "month", "Year" = "year", "Geogroup" = "geogroup", 
         "SpeciesMedianSST" = "midpoint", "MeanSST" = "meansst") %>%
  select(Geogroup, SiteLat, SiteLong, SiteCode, Location, Day, Month, Year, Diver,
         SurveyID, TaxonomicName, SpeciesEpithet,Genus, Family, Order, SizeClass, 
         MaxSizeObs, MeanSST, SpeciesMedianSST) %>%
  arrange(Geogroup, SiteCode, Day, Month, Year)

# Renaming selected data as data
data <- selected_data

# Confirm there are 335 species
length(unique(data$TaxonomicName)) # 335

# Saving data
save(data, file = "data/data.RData")

# How many...
# Geogroups?
length(unique(data$Geogroup)) # 284
# Surveys?
length(unique(data$SurveyID)) # 21140

# Values of size class?
sort(unique(data$SizeClass))
# 2.5 5.0 7.5 10.0 12.5 15.0 20.0 25.0 30.0 35.0 37.5 40.0 50.0 62.5 75.0 87.5 90.0
# 100.0 112.5 125.0 137.5 150.0 162.5 175.0 187.5

# Smallest MeanSST?
min(data$MeanSST) # 12.98137
# Largest?
max(data$MeanSST) # 29.3511

# Range of Year
sort(unique(data$Year))
# 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010
# 2011 2012 2013 2014 2015 2016 2017 2018

# WHY WHITEFIN SURGEONFISH?

speciesCount <- data %>%
  group_by(TaxonomicName) %>%
  summarise(count = n()) %>%
  arrange(count)

# Low number of observations that make process time in R quicker
# Signifcant correlation (found later in the report)

# Checking to see if data was filtered the way described
WS_data <- data %>% filter(TaxonomicName %in% "Acanthurus albipectoralis")
dim(WS_data) # 1090 19
# More than 1000 observations...
length(unique(WS_data$Geogroup)) # 30
# ...in at least 10 geographic cells...
length(unique(WS_data$Year)) # 10
# ...over at least 5 years.
length(unique(WS_data$SurveyID)) # 87
