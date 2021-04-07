# https://viz-ggplot2.rsquaredacademy.com/textann.html
# https://ggplot2-book.org/layers.html

# Loading necessary libraries
library(tidyverse)
library(ggplot2)
library(broom)
library(nlme)
library(lme4)
library(ggeffects)

# Load data
load(file = "data/data.RData")

# PEARLY WRASSE (HALICHOERES MARGARITACEUS)

# Create data frame containing only observations from halichoeres margaritaceus
PW_data <- data %>% filter(TaxonomicName %in% "Halichoeres margaritaceus")

# Checking to see if data was filtered the way described
dim(PW_data) # 1008 19
# More than 1000 observations...
length(unique(PW_data$Geogroup)) # 37
# ...in at least 10 geographic cells...
length(unique(PW_data$Year)) # 11
# ...over at least 5 years.

# SIMPLE LINEAR REGRESSION

# Inspecting explanatory variable
PW_data <- PW_data %>% mutate(ScaledMeanSST = scale(MeanSST, center = T, scale = T))
# Good practise to scale indepednet variable.

# First model
PW.lm1 <- lm(SizeClass ~ ScaledMeanSST, data = PW_data)

# Plot model
ggplot(PW_data, aes(x=ScaledMeanSST, y=SizeClass)) +
  labs(y="Log-transformed size class (cm)", x="Scaled Mean SST (°C)") +
  geom_point(alpha = 0.1) + 
  geom_smooth(method="lm") +
  theme_classic()
# alpha = 0.1 shows how many observations have the same size class at the same temperature.
# The darker the point, the more observations have the same statistics.
# Initially, high temperature, small size. How can we trust the model?

# Model validation
# Homogeneity.
plot(PW.lm1, which=1) + text(8.4, 9.7, "A", font = 2, cex = 1.5, col = "red")
# Should be flat.
E1 <- resid(PW.lm1)
# Normality.
# https://stats.stackexchange.com/questions/60410/normality-of-dependent-variable-normality-of-residuals
# PW_size <- PW_data %>% group_by(SizeClass) %>% summarise(count=n())
# ggplot(PW_size, aes(x=factor(SizeClass), y=count)) + 
#  geom_point() + 
#  geom_line(group=1) +
#  scale_y_continuous(labels = scales::comma) + 
#  theme_classic()
hist(E1, xlab = "Residuals", main = "") + text(-3.5, 250, "B", cex = 1.5, font = 2, col = "red")
plot(PW.lm1, which=2) + text(-2.5, 3.5, "C", cex = 1.5, font = 2, col = "red")
# Independence.
plot(PW_data$MeanSST, E1, xlab = "MeanSST", ylab= "Residuals") + text(21, 8.5, "D", cex = 1.5, font = 2, col = "red")

# Where to go from here?
# Inspect the spread of data for geogroup, surveyID and yearas included in the reference article.
# The specific values of them do not matter in this analysis, only how they group the data.
# Index them.

# Nomial variables so factor.
PW_data$Geogroup <- factor(PW_data$Geogroup)
PW_data$SurveyID <- factor(PW_data$SurveyID)
PW_data$Year <- factor(PW_data$Year)

str(PW_data)

# Boxplot
# Use ggplot2 so we can swap axis labels
# GEOGROUP
ggplot(PW_data, aes(x=Geogroup, y=SizeClass)) +
  labs(y="Size class (cm)", x="Geogroup") +
  geom_boxplot() + 
  theme_classic() +
  coord_flip()
# SURVEY
ggplot(PW_data, aes(x=SurveyID, y=SizeClass)) +
  labs(y="Size class (cm)", x="SurveyID") +
  geom_boxplot() + 
  theme_classic() +
  coord_flip()
# YEAR
ggplot(PW_data, aes(x=Year, y=SizeClass)) +
  labs(y="Size class (cm)", x="Year") +
  geom_boxplot() + 
  theme_classic() +
  coord_flip()
# Not the same spread. 
# Some only indlude one observation.
# Year Index is slightly better than the others but not perfect.

# To illustrate further, we run sepearte analyses.
# GEOGROUP
ggplot(PW_data, aes(x=ScaledMeanSST, y=SizeClass, colour=Geogroup)) +
  labs(y="Size class (cm)", x="Scaled mean SST (°C)") +
  geom_point() + 
  annotate(geom = "text", x = -3, y = 14, label = "A", fontface = 2, colour = "red", size = 6) +
  theme_classic() + 
  theme(legend.position="none")
# Pink dots fall to the right on the horixontal axis.
# Orange dots fall to the left.
# SURVEY
ggplot(PW_data, aes(x=ScaledMeanSST, y=SizeClass, colour=SurveyID)) +
  labs(y="Size class (cm)", x="Scaled mean SST (°C)") +
  geom_point() + 
  annotate(geom = "text", x = -3, y = 14, label = "B", fontface = 2, colour = "red", size = 6) +
  theme_classic() + 
  theme(legend.position="none")
# YEAR
ggplot(PW_data, aes(x=ScaledMeanSST, y=SizeClass, colour=Year)) +
  labs(y="Size class (cm)", x="Scaled mean SST (°C)") +
  geom_point() + 
  annotate(geom = "text", x = -3, y = 14, label = "C", fontface = 2, colour = "red", size = 6) +
  theme_classic() + 
  theme(legend.position="none")
# We'd have to fun 37 seperate anslyses, that's 74 (111) and adding the effects of SurveyIndex,
# even year would drastically increase number of estimated parameters.
# Some geogroups only a few observations! Not important analysis.
# Leads us astray from our original question.

# MIXED EFFECTS MODELLING

# Using nlme
# f1 <- formula(SizeClass ~ ScaledMeanSST)
# lm <- gls(f1, method = "REML", data = PW_data) # use gls in order to compare models
# mm <- lme(f1, random = ~1 | GeoIndex / SurveyIndex, data = PW_data, method = "REML")
# anova(PW.lm1, PW.mm1)
#        Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# PW.lm1     1  3 4748.695 4763.436 -2371.347                        
# PW.mm1     2  5 4360.609 4385.178 -2175.305 1 vs 2 392.0856  <.0001
# Validation
# Homogeneity.
# plot(PW.mm1, which=1)
# Should be flat.
# E2 <- resid(PW.mm1)
# Normality.
# hist(E2, xlab = "Residuals", main = "")
# plot(PW.mm1, which=2)
# Independence.
# plot(PW_data$ScaledMeanSST, E2, xlab = "ScaledMeanSST", ylab= "Residuals")

mm1  <- lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup), REML = T, data = PW_data)
mm2  <- lmer(SizeClass ~ ScaledMeanSST + (1|Year), REML = T, data = PW_data)
mm3  <- lmer(SizeClass ~ ScaledMeanSST + (1|SurveyID), REML = T, data = PW_data)

mm4  <- lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup) + (1|Year) + (1|SurveyID), REML = T, data = PW_data)
mm5  <- lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup) + (1|SurveyID), REML = T, data = PW_data)
mm6  <- lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup) + (1|Year), REML = T, data = PW_data)
mm7  <- lmer(SizeClass ~ ScaledMeanSST + (1|Year) + (1|SurveyID), REML = T, data = PW_data)

mm8  <- lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup/Year/SurveyID), REML = T, data = PW_data)
# Failed to converge
mm9 <- lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup/SurveyID), REML = T, data = PW_data)
mm10 <- lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup/Year), REML = T, data = PW_data)
mm11 <- lmer(SizeClass ~ ScaledMeanSST + (1|Year/SurveyID), REML = T, data = PW_data)

BIC(mm1, mm2, mm3, mm4, mm5, mm6, mm7, mm9, mm10, mm11)

#      df      BIC
# mm1   4 4416.070
# mm2   4 4747.702
# mm3   4 4375.641
# mm4   6 4372.073
# mm5   5 4369.391 <- 
# mm6   5 4405.636
# mm7   5 4380.878
# mm9   5 4369.391 <-
# mm10  5 4394.367
# mm11  5 4380.878

summary(mm5)
summary(mm9)

# mm5, mm9
# It is nested. So mm9 
# Makes sense YearIndex is not used as spread wasn't drastically different.
# Interesting about MM3 < MM1 Geogroup too but survey index encapsulates day and geogroup

PW.mm <- mm9
PW.pred.mm <- ggpredict(PW.mm, terms = c("ScaledMeanSST"))  
# this gives overall predictions for the model

# Plot the predictions 
ggplot(PW.pred.mm) + 
  geom_line(aes(x = x, y = predicted)) + # slope
  geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
              fill = "lightgrey", alpha = 0.5) +  # error band
  geom_point(data = PW_data,  # adding the raw data (scaled values)
             aes(x = ScaledMeanSST, y = SizeClass), alpha = 0.1) + 
  labs(y="Size class (cm)", x="Scaled Mean SST (°C)", 
       title = "How temperature affects the body size of Pearly Wrasse") + 
  theme_minimal() + 
  theme(legend.position="none")
# Decrease!

# Homogeneity.
plot(PW.mm, which=1)
text(-4.5, 175, label = "B", font = 2, cex = 1.5, col = "red")
# Should be flat.
E3 <- resid(PW.mm)
# Normality.
hist(E3, xlab = "Residuals", main = "") + text(-4.5, 175, label = "B", font = 2, cex = 1.5, col = "red")
# Independence.
plot(PW_data$ScaledMeanSST, E3, xlab = "ScaledMeanSST", ylab= "Residuals") + text(-2.7, 6, "C", font = 2, cex = 1.5, col = "red")

summary(PW.mm)




