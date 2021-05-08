# https://r4ds.had.co.nz/many-models.html#unnesting
# https://cran.r-project.org/web/packages/broom.mixed/vignettes/broom_mixed_intro.html

# Instead of checking residuals for normality, can check size disribution.

library(tidyverse)
library(broom.mixed)
library(lme4)

load(file = "data/data.RData")
data <- data %>% mutate(ScaledMeanSST = scale(MeanSST, center = T, scale = T))
data$Geogroup <- factor(data$Geogroup)
data$SurveyID <- factor(data$SurveyID)

str(data)

by_species <- data %>%
  group_by(TaxonomicName) %>%
  nest()

dim(by_species)
head(by_species)

# Geogroup/SurveyID

species_model <- function(df) {
  lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup/SurveyID), REML=T, data = df)
}

by_species1 <- by_species %>%
  mutate(model = map(data, species_model)) #byspecies$data 
# 
# boundary (singular) fit: see ?isSingular
# boundary (singular) fit: see ?isSingular
# boundary (singular) fit: see ?isSingular
# boundary (singular) fit: see ?isSingular
# boundary (singular) fit: see ?isSingular
# boundary (singular) fit: see ?isSingular
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#    Model failed to converge with max|grad| = 0.00339692 (tol = 0.002, component 1)
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#    Model failed to converge with max|grad| = 0.00494532 (tol = 0.002, component 1)
# 3: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#    Model failed to converge with max|grad| = 0.00273887 (tol = 0.002, component 1)
# 4: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#    Model failed to converge with max|grad| = 0.00289741 (tol = 0.002, component 1)

# Geogroup + SurveyID

species_model2 <- function(df) {
  lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup) + (1|SurveyID), REML=T, data = df)
}

by_species2 <- by_species %>%
  mutate(model = map(data, species_model2)) #byspecies$data 

# boundary (singular) fit: see ?isSingular
# boundary (singular) fit: see ?isSingular
# boundary (singular) fit: see ?isSingular
# boundary (singular) fit: see ?isSingular
# boundary (singular) fit: see ?isSingular
# boundary (singular) fit: see ?isSingular
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#    Model failed to converge with max|grad| = 0.00339692 (tol = 0.002, component 1)
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#    Model failed to converge with max|grad| = 0.00494532 (tol = 0.002, component 1)
# 3: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#    Model failed to converge with max|grad| = 0.00273887 (tol = 0.002, component 1)
# 4: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#    Model failed to converge with max|grad| = 0.00289741 (tol = 0.002, component 1)

# SurveyID

species_model3 <- function(df) {
  lmer(SizeClass ~ ScaledMeanSST + (1|SurveyID), REML=T, data = df)
}

by_species3 <- by_species %>%
  mutate(model = map(data, species_model3)) #byspecies$data 

tidy <- by_species3 %>%
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

# What are the two points with an estimate greater than 15?

# Shown on plot
SST_est %>%
  ggplot(aes(index, estimate, label=TaxonomicName)) + 
  geom_point() + 
  geom_hline(yintercept = 0, colour = "red") +
  geom_text(aes(label = ifelse(estimate > 15, TaxonomicName, '')), size = 3, hjust= -0.1) +
  theme_classic()

# Checking their summaries 
DN_data <- filter(data, TaxonomicName %in% "Dactylophora nigricans")
DN.mm <- lmer(SizeClass ~ ScaledMeanSST + (1|SurveyID), REML=T, data = DN_data)

# Homogeneity.
plot(DN.mm, which=1)
# Should be flat.
E <- resid(DN.mm)
# Normality.
hist(E, xlab = "Residuals", main = "")
# Independence.
plot(DN_data$ScaledMeanSST, E, xlab = "ScaledMeanSST", ylab= "Residuals")

summary(DN_mod)

AG_data <- filter(data, TaxonomicName %in% "Achoerodus gouldii")
AG.mm <- lmer(SizeClass ~ ScaledMeanSST + (1|SurveyID), REML=T, data = AG_data)

# Homogeneity.
plot(AG.mm, which=1)
# Should be flat.
E <- resid(AG.mm)
# Normality.
hist(E, xlab = "Residuals", main = "")
# Independence.
plot(AG_data$ScaledMeanSST, E, xlab = "ScaledMeanSST", ylab= "Residuals")

# Model validated. Just large estimates. Do not remove.
# Research these fish?

# Round
# Create two data frames
# Dim
# Statitsics

# ROUNDED ESTIMATES TO A WHOLE NUMBER

SST_est$round_est <- round(SST_est$estimate, digits =  0)

# Temperature doesn't have a large range so the slope won't drastically change the 
# body size if slope isn't drastic.

nrow(subset(SST_est, round_est > 0)) # 105
nrow(subset(SST_est, round_est == 0)) # 59
nrow(subset(SST_est, round_est < 0)) # 171

# INCREASE
(105/335)*100 # 31.34328
# NEITHER INCREASE NOR DECREASE
(59/335)*100 # 17.61194
# DECREASE
(171/335)*100 # 51.04478

# ROUNDED ESTIMATES TO THE NEAREST TENTH

SST_est$round_est <- round(SST_est$estimate, digits =  0.5)

nrow(subset(SST_est, round_est > 0)) # 129
nrow(subset(SST_est, round_est == 0)) # 7
nrow(subset(SST_est, round_est < 0)) # 199

# INCREASE
(129/335)*100 # 38.50746
# NEITHER INCREASE NOR DECREASE
(7/335)*100 # 2.089552
# DECREASE
(199/335)*100 # 59.40299

# ESTIMATES LEFT ALONE

nrow(subset(SST_est, estimate > 0)) # 132
nrow(subset(SST_est, estimate == 0)) # 0
nrow(subset(SST_est, estimate < 0)) # 203

# INCREASE
(132/335)*100 # 39.40299
# NEITHER INCREASE NOR DECREASE
(0/335)*100 # 0
# DECREASE
(203/335)*100 # 60.59701



