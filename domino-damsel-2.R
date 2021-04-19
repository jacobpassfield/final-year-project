library(tidyverse)
library(lme4)
library(patchwork)
library(ggplot2)
library(ggeffects)

load(file = "data/data.RData")

# THREESPOT DASCYLLUS (Domino Damsel)

TSD_data <- data %>% filter(TaxonomicName %in% "Dascyllus trimaculatus")

TSD_data <- TSD_data %>% mutate(ScaledMeanSST = scale(MeanSST, center = T, scale = T))

TSD_data$Geogroup <- factor(TSD_data$Geogroup)
TSD_data$SurveyID <- factor(TSD_data$SurveyID)

TSD.mm <- lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup/SurveyID), REML = T, data = TSD_data)

TSD.pred.mm <- ggpredict(TSD.mm, terms = c("ScaledMeanSST"))  # this gives overall predictions for the model

TSDmmPlot <- ggplot(TSD.pred.mm) + 
  geom_line(aes(x = x, y = predicted), size = 2, colour = "blue") + # slope
  geom_point(data = TSD_data,  # adding the raw data (scaled values)
             aes(x = ScaledMeanSST, y = SizeClass), alpha = 0.1, size = 3) + 
  labs(y="Size class (cm)", x="Scaled Mean SST (°C)", 
       title = "How temperature affects the body size of Domino Damsel") + 
  theme_minimal() + 
  theme(legend.position="none")
# Slight increase!

# Validation

# Homegeneity.
homoTSD <- ggplot(TSD.mm, aes(x= .fitted, y= .resid)) + 
  geom_point(shape=1, size=2) +
  geom_hline(yintercept=0, col="red", linetype="dashed") +
  geom_smooth(method = "loess", se=F) +
  labs(title = "Residuals versus fitted values", y="Residuals", x="Fitted values") +
  theme_classic()

# Normality.
normTSD <- ggplot(TSD.mm, aes(x = .resid)) +
  geom_histogram(bins = 10, fill = "white", colour = "black") +
  labs(title = "Histogram of residuals", x = "Residuals", y = "Frequency") +
  theme_classic()

# Independence.
indTSD <- ggplot(TSD_data, aes(x = ScaledMeanSST, y = resid(TSD.mm))) +
  geom_point(shape = 1, size = 2) + 
  labs(title = "Explanatory variable vs residuals", x = "Scaled Mean SST (°C)", y = "Residuals") +
  theme_classic()

pdf(file = "figures/NotNeeded.pdf")
TSDmmPlot + (homoTSD / normTSD / indTSD) + 
  plot_annotation(tag_levels = c("A", "B", "C")) &
  theme(plot.tag = element_text(face = 2, size = 15)) # & operator applies tag style to all plots
dev.off()
