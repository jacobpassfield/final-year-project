# https://r4ds.had.co.nz/many-models.html#unnesting
# https://cran.r-project.org/web/packages/broom.mixed/vignettes/broom_mixed_intro.html

# Instead of checking residuals for normality, can check size disribution.

library(tidyverse)
library(broom.mixed)
library(lme4)

load(file = "data/data.RData")
data <- data %>% mutate(ScaledMeanSST = scale(MeanSST, center = T, scale = T))
data$SurveyID <- factor(data$SurveyID)

by_species <- data %>%
  group_by(TaxonomicName) %>%
  nest()

dim(by_species)
head(by_species)

species_model <- function(df) {
  lmer(SizeClass ~ ScaledMeanSST + (1|SurveyID), REML=T, data = df)
}

by_species <- by_species %>%
  mutate(model = map(data, species_model)) #byspecies$data 

tidy <- by_species %>%
  mutate(tidy = map(model, broom.mixed::tidy)) %>%
  unnest(tidy, .drop = T)

head(tidy)

# Checking if summary() and broom.mixed() are the same
PW <- filter(data, TaxonomicName %in% "Halichoeres margaritaceus")
PW_mod <- lmer(SizeClass ~ ScaledMeanSST + (1|SurveyID), REML=T, data = PW)
broom.mixed::tidy(PW_mod)
summary(PW_mod)
# The same.

SST_est <- tidy %>%
  select(TaxonomicName, effect, term, estimate) %>%
  filter(term %in% c("ScaledMeanSST"))

head(SST_est)
dim(SST_est)

index <- c(1:335)
SST_est %>%
  ggplot(aes(index, estimate)) + geom_point() + geom_hline(yintercept = 0, colour = "red")

DN <- filter(data, TaxonomicName %in% "Dactylophora nigricans")
DN_mod <- lmer(SizeClass ~ ScaledMeanSST + (1|SurveyID), REML=T, data = DN)
broom.mixed::tidy(DN_mod)
summary(DN_mod) # Highhhhhh

# Check residuals of those two? Remove?

