# Loading necessary libraries
library(tidyverse)
library(ggplot2)
library(lme4)
library(ggeffects)

# Load data
load(file = "data/data.RData")

# THREESPOT DASCYLLUS (Domino Damsel)

TSD_data <- data %>% filter(TaxonomicName %in% "Dascyllus trimaculatus")

TSD_data <- TSD_data %>% mutate(ScaledMeanSST = scale(MeanSST, center = T, scale = T))

TSD_data <- TSD_data %>% 
  mutate(SurveyDate = as.Date(paste(Year, Month, Day, sep = "-")),
         Survey = paste(Geogroup, SurveyDate, sep = "/")) %>%
  arrange(Geogroup, Survey) %>%
  mutate(GeoIndex = as.integer(factor(Geogroup))) %>%
  mutate(YearIndex = as.integer(factor(Year))) %>%
  mutate(SurveyIndex = as.integer(factor(Survey)))

TSD_data$GeoIndex <- factor(TSD_data$GeoIndex)
TSD_data$SurveyIndex <- factor(TSD_data$SurveyIndex)
TSD_data$YearIndex <- factor(TSD_data$YearIndex)

TSD.mm <- lmer(SizeClass ~ ScaledMeanSST + (1|GeoIndex/SurveyIndex), data = TSD_data, REML = T)

TSD.pred.mm <- ggpredict(TSD.mm, terms = c("ScaledMeanSST"))  # this gives overall predictions for the model

ggplot(TSD.pred.mm) + 
  geom_line(aes(x = x, y = predicted)) + # slope
  geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
              fill = "lightgrey", alpha = 0.5) +  # error band
  geom_point(data = TSD_data,  # adding the raw data (scaled values)
             aes(x = ScaledMeanSST, y = SizeClass), alpha = 0.1) + 
  labs(y="Size class (cm)", x="Scaled Mean SST (°C)", 
       title = "How temperature affects the body size of Domino Damsel") + 
  theme_minimal() + 
  theme(legend.position="none")
# Increase!

# Homogeneity.
plot(TSD.mm, which=1)
# Should be flat.
E <- resid(TSD.mm)
# Normality.
hist(E, xlab = "Residuals", main = "")
# Independence.
plot(TSD_data$ScaledMeanSST, E, xlab = "ScaledMeanSST", ylab= "Residuals")

summary(TSD.mm)
