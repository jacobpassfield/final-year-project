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

# WHY PEARLY WRASSE?

# Find species with maximum observations
which.max(speciesCount$n) # 329
speciesCount[329,] # Trachurus novaezelandiae 914584

# Find species with minimum observations
which.min(speciesCount$n) # 142
speciesCount[142,] # Halichoeres margaritaceus  1008

# Smallest number observations makes it easier for analysis.
