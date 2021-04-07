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

TSD_data$Geogroup <- factor(TSD_data$Geogroup)
TSD_data$SurveyID <- factor(TSD_data$SurveyID)

TSD.mm <- lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup/SurveyID), REML = T, data = TSD_data)

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
# Slight increase!

# Homogeneity.
plot(TSD.mm, which=1)
# Should be flat.
E <- resid(TSD.mm)
# Normality.
hist(E, xlab = "Residuals", main = "")
# Independence.
plot(TSD_data$ScaledMeanSST, E, xlab = "ScaledMeanSST", ylab= "Residuals")

summary(TSD.mm)
