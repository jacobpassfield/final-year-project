library(tidyverse)
library(ggplot2)
library(patchwork)
library(lmerTest)
library(ggeffects)
library(ggpubr)
library(broom.mixed)

# Create plot and title axis parameters
theme_parameters <- theme(plot.title = element_text(size = 15),
                          plot.subtitle = element_text(size = 12),
                          axis.title = element_text(size = 15),
                          axis.text = element_text(size = 15),
                          strip.text = element_text(size = 15))

# Loading data
load(file = "data/data.RData")

# Scaling and centering mean SST
data <- data %>% mutate(ScaledMeanSST = scale(MeanSST, center = T, scale = T))

# Factoring cell and survey columns
data$Geogroup <- factor(data$Geogroup)
data$SurveyID <- factor(data$SurveyID)

# Nesting data by taxonomic name
by_species <- data %>%
  group_by(TaxonomicName) %>%
  nest()
# Checking first size rows
head(by_species)

# Geogroup/SurveyID
# Creating model
species_model <- function(df) {
  lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup/SurveyID), REML=T, data = df)
}
# Applying model to data
by_species1 <- by_species %>%
  mutate(model = map(data, species_model)) #byspecies$data 
# Next BIC value for WS data is model using Year/SurveyID 

# Year/SurveyID
# Creating model
species_model2 <- function(df) {
  lmer(SizeClass ~ ScaledMeanSST + (1|Year/SurveyID), REML=T, data = df)
}
# Applying model to data
by_species2 <- by_species %>%
  mutate(model = map(data, species_model2)) #byspecies$data 
# Next BIC value for WS data is model using SurveyID 

# SurveyID
# Creating model
species_model3 <- function(df) {
  lmer(SizeClass ~ ScaledMeanSST + (1|SurveyID), REML=T, data = df)
}
# Applying model to data
by_species3 <- by_species %>%
  mutate(model = map(data, species_model3)) #byspecies$data 
head(by_species3)
# Use SurveyID

# Assumptions still met for whitefin surgeonfish?
# Filter data and fit model
WS_data <- data %>% filter(TaxonomicName %in% "Acanthurus albipectoralis")
WS_data <- WS_data %>% mutate(ScaledMeanSST = scale(MeanSST, center = T, scale = T))
WS_data$SurveyID <- factor(WS_data$SurveyID)
WS.mm2 <- lmer(SizeClass ~ ScaledMeanSST + (1|SurveyID), REML = T, data = WS_data)
# Overall predictions for the model
WS.pred.mm2 <- ggpredict(WS.mm2, terms = c("ScaledMeanSST"))
# Plot the predictions 
WSmmPlot <- ggplot(WS.pred.mm2) + 
  geom_line(aes(x = x, y = predicted), colour = "blue", size = 1) + # slope
  geom_point(data = WS_data,  # adding the raw data (scaled values)
             aes(x = ScaledMeanSST, y = SizeClass), alpha = 0.1, size = 3) + 
  labs(y="Size class (cm)", x="Scaled Mean SST (°C)", 
       title = "SST vs size of WS data",
       subtitle = "Linear mixed-effects model") + 
  theme_minimal() +
  theme_parameters +
  theme(legend.position="none")
# Model validation graphs
# Homoscedasticity
homo.WS <- ggplot(WS.mm2, aes(x = .fitted, y = .resid)) + 
  geom_jitter(shape = 1, size = 2, width = 0.1, height = 0.5) +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = F) +
  theme_classic() +
  theme_parameters +
  labs(title = "Residuals vs fitted values", y = "Residuals", x = "Fitted values")
# Normality of residuals
norm.WS <- ggplot(WS.mm2, aes(x = .resid)) +
  geom_histogram(bins = 10, fill = "white", colour = "black") +
  theme_classic() +
  theme_parameters +
  labs(title = "Histogram of the residuals", x = "Residuals", y = "Frequency")
# Normality of random effects
# SurveyID
re.WS <- ranef(WS.mm2)$SurveyID$`(Intercept)`
qq.WS <- ggqqplot(re.WS, conf.int.level = 0, shape = 19) +
  theme_parameters +
  labs(title = "Normal Q-Q plot", subtitle = "Random effect - survey", x = "Theoretical quantiles", y = "Sample quantiles")
# Obtaining estimates
summary(WS.mm2)
# alpha: 22.4730, beta: 3.1061, residual variance: 2.424^2, 
# survey variance: 5.89^2, p-value: 1.03e-09
# The resulting p-value for beta is 1.03e-09 < 0.05 and so beta is significant.

# Save
pdf(file = "figures/figure-8.pdf")
WSmmPlot + (homo.WS / norm.WS / qq.WS) + 
  plot_annotation(tag_levels = c("A", "B", "C", "D")) &
    theme(plot.tag = element_text(face = 2, size = 15))
dev.off()

# Obtaining model results for each species
tidy <- by_species3 %>%
  mutate(tidy = map(model, broom.mixed::tidy)) %>%
  unnest(tidy, .drop = T)
head(tidy)

# Extracting estimate of fixed effect (beta) and the correpsonding p-value
# Create data frame including species name and the estimate of the slope.
estimates <- tidy %>%
  select(TaxonomicName, effect, term, estimate, p.value) %>%
  filter(term %in% c("ScaledMeanSST"))

# Extract estimates that are significant
sig_est <- estimates %>%
  filter(p.value < 0.05) %>%
  arrange(TaxonomicName)

# Extract estimates that are not significant
notsig_est <- estimates %>%
  filter(p.value > 0.05) %>%
  arrange(TaxonomicName)

# Plot significant values with estimates that appear extreme labelled
dim(sig_est) # 221  5
index = c(1:221)
sigestPlot <- ggplot(sig_est, aes(x = index, y = estimate)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  geom_text(aes(label = ifelse(estimate > 15, TaxonomicName, '')), size = 4, hjust= -0.1) +
  theme_parameters +
  theme_classic() +
  labs(y = "Significant estimated coefficients for sea surface temperature", x = "Index")

# Save
pdf(file = "figures/figure-9.pdf")
sigestPlot
dev.off()

# Investigating the estimates that appear extreme
# Creating two individual data frames 
AG_data <- filter(data, TaxonomicName %in% "Achoerodus gouldii")
DN_data <- filter(data, TaxonomicName %in% "Dactylophora nigricans")
# Creating models
AG.mm <- lmer(SizeClass ~ ScaledMeanSST + (1|SurveyID), REML=T, data = AG_data)
DN.mm <- lmer(SizeClass ~ ScaledMeanSST + (1|SurveyID), REML=T, data = DN_data)
# Model validation graphs
# Homoscedasticity
homo.AG <- ggplot(AG.mm, aes(x = .fitted, y = .resid)) + 
  geom_jitter(shape = 1, size = 2, width = 0.1, height = 0.5) +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = F) +
  theme_classic() +
  theme_parameters +
  labs(title = "Residuals vs fitted values", y = "Residuals", x = "Fitted values")
homo.DN <- ggplot(DN.mm, aes(x = .fitted, y = .resid)) + 
  geom_jitter(shape = 1, size = 2, width = 0.1, height = 0.5) +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = F) +
  theme_classic() +
  theme_parameters +
  labs(title = "Residuals vs fitted values", y = "Residuals", x = "Fitted values")
# Normality of residuals
norm.AG <- ggplot(AG.mm, aes(x = .resid)) +
  geom_histogram(bins = 10, fill = "white", colour = "black") +
  theme_classic() +
  theme_parameters +
  labs(title = "Histogram of the residuals", x = "Residuals", y = "Frequency")
norm.DN <- ggplot(DN.mm, aes(x = .resid)) +
  geom_histogram(bins = 10, fill = "white", colour = "black") +
  theme_classic() +
  theme_parameters +
  labs(title = "Histogram of the residuals", x = "Residuals", y = "Frequency")
# Normality of random effects
# Survey
re.AG <- ranef(AG.mm)$SurveyID$`(Intercept)`
qq.AG <- ggqqplot(re.AG, conf.int.level = 0, shape = 19) +
  theme_parameters +
  labs(title = "Normal Q-Q plot", subtitle = "Random effect - survey", x = "Theoretical quantiles", y = "Sample quantiles")
re.DN <- ranef(DN.mm)$SurveyID$`(Intercept)`
qq.DN <- ggqqplot(re.DN, conf.int.level = 0, shape = 19) +
  theme_parameters +
  labs(title = "Normal Q-Q plot", subtitle = "Random effect - survey", x = "Theoretical quantiles", y = "Sample quantiles")
# Shapiro-Test for normality
shapiro.test(re.AG) # p-value < 2.2e-16
shapiro.test(re.DN) # p-value < 2.2e-16
# Can assume normality.

pdf(file = "figures/figure-10.pdf")
(homo.AG + homo.DN) / (norm.AG + norm.DN) / (qq.AG + qq.DN) &
  plot_annotation(tag_levels = c("A", "B", "C", "D", "E", "F")) &
   theme(plot.tag = element_text(face = 2, size = 15)) # & operator applies tag style to all plots
dev.off()

# Generate percentages
nrow(subset(sig_est, estimate > 0)) # 70
nrow(subset(sig_est, estimate == 0)) # 0
nrow(subset(sig_est, estimate < 0)) # 151
# Positive correlation
(70/335)*100 # 20.89552
# No correlation
(dim(notsig_est)[1]/335)*100 # 34.02985
# Negative correlation
(151/335)*100 # 45.07463
