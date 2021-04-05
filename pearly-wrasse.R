# Loading necessary libraries
library(tidyverse)
library(ggplot2)
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
plot(PW.lm1, which=1)
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
hist(E1, xlab = "Residuals", main = "")
plot(PW.lm1, which=2)
# Independence.
plot(PW_data$MeanSST, E1, xlab = "MeanSST", ylab= "Residuals")

# Where to go from here?
# Inspect the spread of data for geogroup, surveyID and yearas included in the reference article.
# The specific values of them do not matter in this analysis, only how they group the data.
# Index them.

PW_data <- PW_data %>% 
  mutate(SurveyDate = as.Date(paste(Year, Month, Day, sep = "-")),
         Survey = paste(Geogroup, SurveyDate, sep = "/")) %>%
  arrange(Geogroup, Survey) %>%
  mutate(GeoIndex = as.integer(factor(Geogroup))) %>%
  mutate(YearIndex = as.integer(factor(Year))) %>%
  mutate(SurveyIndex = as.integer(factor(Survey)))

# Nomial variables so factor.
PW_data$GeoIndex <- factor(PW_data$GeoIndex)
PW_data$SurveyIndex <- factor(PW_data$SurveyIndex)
PW_data$YearIndex <- factor(PW_data$YearIndex)

# Boxplot
# Use ggplot2 so we can swap axis labels
# GEOGROUP
ggplot(PW_data, aes(x=GeoIndex, y=SizeClass)) +
  labs(y="Size class (cm)", x="Geogroup") +
  geom_boxplot() + 
  theme_classic() +
  coord_flip()
# SURVEY
ggplot(PW_data, aes(x=SurveyIndex, y=SizeClass)) +
  labs(y="Size class (cm)", x="Geogroup") +
  geom_boxplot() + 
  theme_classic() +
  coord_flip()
# YEAR
ggplot(PW_data, aes(x=YearIndex, y=SizeClass)) +
  labs(y="Size class (cm)", x="Geogroup") +
  geom_boxplot() + 
  theme_classic() +
  coord_flip()
# Not the same spread. 
# Some only indlude one observation.
# Year Index is slightly better than the others but not perfect.

# To illustrate further, we run sepearte analyses.
# GEOGROUP
ggplot(PW_data, aes(x=ScaledMeanSST, y=SizeClass, colour=GeoIndex)) +
  labs(y="Log-transformed size class (cm)", x="Scaled mean SST (°C)") +
  geom_point() + 
  theme_classic() + 
  theme(legend.position="none")
# Pink dots fall to the right on the horixontal axis.
# Orange dots fall to the left.
# SURVEY
ggplot(PW_data, aes(x=ScaledMeanSST, y=SizeClass, colour=SurveyIndex)) +
  labs(y="Log-transformed size class (cm)", x="Scaled mean SST (°C)") +
  geom_point() + 
  theme_classic() + 
  theme(legend.position="none")
# YEAR
ggplot(PW_data, aes(x=ScaledMeanSST, y=SizeClass, colour=YearIndex)) +
  labs(y="Log-transformed size class (cm)", x="Scaled mean SST (°C)") +
  geom_point() + 
  theme_classic() + 
  theme(legend.position="none")
# We'd have to fun 37 seperate anslyses, that's 74 (111) and adding the effects of SurveyIndex,
# even year would drastically increase number of estimated parameters.
# Some geogroups only a few observations! Not important analysis.
# Leads us astray from our original question.

# MIXED EFFECTS MODELLING

# Using nlme
f1 <- formula(SizeClass ~ ScaledMeanSST)
PW.lm1 <- gls(f1, method = "REML", data = PW_data) # use gls in order to compare models
PW.mm1 <- lme(f1, random = ~1 | GeoIndex / SurveyIndex,
          data = PW_data, method = "REML")
anova(PW.lm1, PW.mm1)
#        Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# PW.lm1     1  3 4748.695 4763.436 -2371.347                        
# PW.mm1     2  5 4360.609 4385.178 -2175.305 1 vs 2 392.0856  <.0001
# Validation
# Homogeneity.
plot(PW.mm1, which=1)
# Should be flat.
E2 <- resid(PW.mm1)
# Normality.
hist(E2, xlab = "Residuals", main = "")
plot(PW.mm1, which=2)
# Independence.
plot(PW_data$ScaledMeanSST, E2, xlab = "ScaledMeanSST", ylab= "Residuals")

PW.mm2  <- lmer(SizeClass ~ ScaledMeanSST + (1|GeoIndex), data = PW_data, REML = T)
PW.mm3  <- lmer(SizeClass ~ ScaledMeanSST + (1|YearIndex), data = PW_data, REML = T)
PW.mm4  <- lmer(SizeClass ~ ScaledMeanSST + (1|SurveyIndex), data = PW_data, REML = T)
 
PW.mm5  <- lmer(SizeClass ~ ScaledMeanSST + (1|GeoIndex) + (1|YearIndex) + (1|SurveyIndex), data = PW_data, REML = T)
PW.mm6  <- lmer(SizeClass ~ ScaledMeanSST + (1|GeoIndex) + (1|SurveyIndex), data = PW_data, REML = T)
PW.mm7  <- lmer(SizeClass ~ ScaledMeanSST + (1|GeoIndex) + (1|YearIndex), data = PW_data, REML = T)
PW.mm8  <- lmer(SizeClass ~ ScaledMeanSST + (1|YearIndex) + (1|SurveyIndex), data = PW_data, REML = T)

PW.mm9  <- lmer(SizeClass ~ ScaledMeanSST + (1|GeoIndex/YearIndex/SurveyIndex), data = PW_data, REML = T)
PW.mm10 <- lmer(SizeClass ~ ScaledMeanSST + (1|GeoIndex/SurveyIndex), data = PW_data, REML = T)
PW.mm11 <- lmer(SizeClass ~ ScaledMeanSST + (1|GeoIndex/YearIndex), data = PW_data, REML = T)
PW.mm12 <- lmer(SizeClass ~ ScaledMeanSST + (1|YearIndex/SurveyIndex), data = PW_data, REML = T)

BIC(PW.mm2, PW.mm3, PW.mm4, PW.mm5, PW.mm6, PW.mm7, PW.mm8, PW.mm9, PW.mm10, PW.mm11, PW.mm12)

#         df      BIC
# PW.mm2   4 4416.070
# PW.mm3   4 4747.702
# PW.mm4   4 4382.043
# PW.mm5   6 4391.323
# PW.mm6   5 4385.188
# PW.mm7   5 4405.636
# PW.mm8   5 4388.283
# PW.mm9   6 4387.722
# PW.mm10  5 4385.188
# PW.mm11  5 4394.367
# PW.mm12  5 4388.283

# PW.mm6, PW.mm10
# It is nested. So PW.mm10. 
# Makes sense YearIndex is not used as spread wasn't drastically different.

PW.mm <- PW.mm10
PW.pred.mm <- ggpredict(PW.mm10, terms = c("ScaledMeanSST"))  
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
# Should be flat.
E3 <- resid(PW.mm)
# Normality.
hist(E3, xlab = "Residuals", main = "")
# Independence.
plot(PW_data$ScaledMeanSST, E3, xlab = "ScaledMeanSST", ylab= "Residuals")

summary(PW.mm)




