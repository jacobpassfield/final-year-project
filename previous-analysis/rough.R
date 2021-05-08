load(file = "data/data.RData")
PW_data <- data %>% filter(TaxonomicName %in% "Halichoeres margaritaceus")
PW_data <- PW_data %>% mutate(ScaledMeanSST = scale(MeanSST, center = T, scale = T))

PW_data1 <- PW_data %>% 
  mutate(SurveyDate = as.Date(paste(Year, Month, Day, sep = "-")),
         Survey = paste(Geogroup, SurveyDate, sep = "/")) %>%
  arrange(Geogroup, Survey) %>%
  mutate(GeoIndex = as.integer(factor(Geogroup))) %>%
  mutate(YearIndex = as.integer(factor(Year))) %>%
  mutate(SurveyIndex = as.integer(factor(Survey)))

PW_data1$GeoIndex <- factor(PW_data1$GeoIndex)
PW_data1$SurveyIndex <- factor(PW_data1$SurveyIndex)
PW_data1$YearIndex <- factor(PW_data1$YearIndex)

PW_data2 <- PW_data %>% 
  mutate(SurveyDate = as.Date(paste(Year, Month, Day, sep = "-")),
         Survey = paste(SurveyDate, Geogroup, sep = "/")) %>%
  arrange(Geogroup, Survey) %>%
  mutate(SurveyIndex = as.integer(factor(Survey)))

PW_data2$Geogroup <- factor(PW_data2$Geogroup)
PW_data2$SurveyIndex <- factor(PW_data2$SurveyIndex)
PW_data2$Year <- factor(PW_data2$Year)

PW_data3 <- PW_data

PW_data3$Geogroup <- factor(PW_data3$Geogroup)
PW_data3$SurveyID <- factor(PW_data3$SurveyID)
PW_data3$Year <- factor(PW_data3$Year)

# PW_data1 for SurveyIndex. PW_data2 for SurveyIndex. PW_data3 for SurveyID.

mm1  <- lmer(SizeClass ~ ScaledMeanSST + (1|SurveyIndex), data = PW_data1, REML = T)
mm2  <- lmer(SizeClass ~ ScaledMeanSST + (1|SurveyIndex), data = PW_data2, REML = T)
mm3  <- lmer(SizeClass ~ ScaledMeanSST + (1|SurveyID), data = PW_data3, REML = T)

BIC(mm1, mm2, mm3)

levels(PW_data3$SurveyID)
levels(PW_data1$SurveyIndex)

str(PW_data3)
str(PW_data1)

# PW_data1 for GeoIndex and SurveyIndex PW_data2 for Geogroup and Survey Index.

mm4  <- lmer(SizeClass ~ ScaledMeanSST + (1|GeoIndex/SurveyIndex), data = PW_data1, REML = T)
mm5  <- lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup/SurveyIndex), data = PW_data2, REML = T)
mm6  <- lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup/SurveyID), data = PW_data3, REML = T)

BIC(mm4, mm5, mm6)

# PW_data3 for Geogroup and SurveyID.

# https://drsimonj.svbtle.com/running-a-model-on-separate-groups

library(tidyverse)
library(broom)
library(lme4)

load(file = "data/data.RData")

# Adding indexes and scaled explanatory variable.
data <- data %>% 
  mutate(ScaledMeanSST = scale(MeanSST, center = T, scale = T)) %>% 
  mutate(SurveyDate = as.Date(paste(Year, Month, Day, sep = "-")),
         Survey = paste(Geogroup, SurveyDate, sep = "/")) %>%
  arrange(Geogroup, Survey) %>%
  mutate(GeoIndex = as.integer(factor(Geogroup))) %>%
  mutate(SurveyIndex = as.integer(factor(Survey)))

data$GeoIndex <- factor(data$GeoIndex)
data$SurveyIndex <- factor(data$SurveyIndex)

save(data, file = "data/all_data.RData")

load(file = "data/all_data.RData")

data %>% nest(-TaxonomicName)

d <- data %>%
  nest(-TaxonomicName) %>%
  mutate(fit = map(data, ~ lmer(f1 + (1|GeoIndex/SurveyIndex), REML=T, .)))

# NormalisedSST to stop convergence failing
# Increase number of interations

tidy(d)

library(broom.mixed)


library(nlme)
f1 <- formula(SizeClass ~ ScaledMeanSST)
mm <- lme(f1, random = ~1 | GeoIndex / SurveyIndex,
          data, method = "REML")



# How does body size change with temperature for each species?

library(modelr)
library(tidyverse)
library(lme4)
library(broom.mixed)

load(file = "data/all_data.RData") 

by_species <- data %>%
  group_by(TaxonomicName) %>%
  nest()

dim(by_species)
head(by_species)

species_model <- function(df) {
  lmer(SizeClass ~ ScaledMeanSST + (1|SurveyIndex), REML=T, data = df)
}

by_species <- by_species %>%
  mutate(model = map(data, species_model))

head(by_species)

tidy <- by_species %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy, .drop = T)

# Check with PW_data and DD_data

tidy2 <- by_species %>%
  mutate(tidy = map(model, broom.mixed::tidy)) %>%
  unnest(tidy, .drop = T)

head(tidy)
head(tidy2)

# Using nlme
# f1 <- formula(SizeClass ~ ScaledMeanSST)
# lm <- gls(f1, method = "REML", data = PW_data) # use gls in order to compare models
# mm <- lme(f1, random = ~1 | Geogroup / SurveyID, data = PW_data, method = "REML")
# anova(lm, mm)
#       Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# PW.lm1     1  3 4748.695 4763.436 -2371.347                        
# PW.mm1     2  5 4344.812 4369.381 -2167.406 1 vs 2 407.8828  <.0001


# https://stats.stackexchange.com/questions/60410/normality-of-dependent-variable-normality-of-residuals
# PW_size <- PW_data %>% group_by(SizeClass) %>% summarise(count=n())
# ggplot(PW_size, aes(x=factor(SizeClass), y=count)) + 
#  geom_point() + 
#  geom_line(group=1) +
#  scale_y_continuous(labels = scales::comma) + 
#  theme_classic()






