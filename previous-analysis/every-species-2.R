library(tidyverse)
library(broom.mixed)
library(lme4)
library(patchwork)
library(ggeffects)

load(file = "data/data.RData")
data <- data %>% mutate(ScaledMeanSST = scale(MeanSST, center = T, scale = T))
data$Geogroup <- factor(data$Geogroup)
data$SurveyID <- factor(data$SurveyID)

by_species <- data %>%
  group_by(TaxonomicName) %>%
  nest()

dim(by_species)
head(by_species)

# Geogroup / SurveyID

species_model <- function(df) {
  lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup/SurveyID), REML=T, data = df)
}

by_species1 <- by_species %>%
  mutate(model = map(data, species_model)) #byspecies$data 

# SurveyID

species_model2 <- function(df) {
  lmer(SizeClass ~ ScaledMeanSST + (1|SurveyID), REML=T, data = df)
}

by_species2 <- by_species %>%
  mutate(model = map(data, species_model2)) #byspecies$data 

head(by_species2)

# Use only SurveyID

# Are assumptions still met for PW?

PW_data <- data %>% filter(TaxonomicName %in% "Halichoeres margaritaceus")
PW_data <- PW_data %>% mutate(ScaledMeanSST = scale(MeanSST, center = T, scale = T))
PW_data$Geogroup <- factor(PW_data$Geogroup)
PW_data$SurveyID <- factor(PW_data$SurveyID)
PW.mm2 <- lmer(SizeClass ~ ScaledMeanSST + (1|SurveyID), REML = T, data = PW_data)
PW.pred.mm2 <- ggpredict(PW.mm2, terms = c("ScaledMeanSST"))  # this gives overall predictions for the model
PWmmPlot2 <- ggplot(PW.pred.mm2) + 
  geom_line(aes(x = x, y = predicted), size = 2, colour = "blue") + # slope
  geom_point(data = PW_data,  # adding the raw data (scaled values)
             aes(x = ScaledMeanSST, y = SizeClass), alpha = 0.1, size = 3) + 
  labs(y="Size class (cm)", x="Scaled Mean SST (°C)", 
       title = "Temperature versus size class",
       subtitle = "Using a linear mixed-effects model") + 
  theme_minimal() + 
  theme(legend.position="none")

# Homegeneity.
homoPW <- ggplot(PW.mm2, aes(x= .fitted, y= .resid)) + 
  geom_point(shape=1, size=2) +
  geom_hline(yintercept=0, col="red", linetype="dashed") +
  geom_smooth(method = "loess", se=F) +
  labs(title = "Residuals versus fitted values", y="Residuals", x="Fitted values") +
  theme_classic()

# Normality.
normPW <- ggplot(PW.mm2, aes(x = .resid)) +
  geom_histogram(bins = 10, fill = "white", colour = "black") +
  labs(title = "Histogram of residuals", x = "Residuals", y = "Frequency") +
  theme_classic()

# Independence.
indPW <- ggplot(PW_data, aes(x = ScaledMeanSST, y = resid(PW.mm2))) +
  geom_point(shape = 1, size = 2) + 
  labs(title = "Explanatory variable vs residuals", x = "Scaled Mean SST (°C)", y = "Residuals") +
  theme_classic()

summary(PW.mm2)

pdf(file = "figures/Figure9.pdf")
PWmmPlot2 + (homoPW / normPW / indPW) + 
  plot_annotation(tag_levels = c("A", "B", "C")) &
  theme(plot.tag = element_text(face = 2, size = 15)) # & operator applies tag style to all plots
dev.off()

# Phew.

tidy <- by_species2 %>%
  mutate(tidy = map(model, broom.mixed::tidy)) %>%
  unnest(tidy, .drop = T)

head(tidy)

# Create data frame including species name and the estimate of the slope.

SST_est <- tidy %>%
  select(TaxonomicName, effect, term, estimate) %>%
  filter(term %in% c("ScaledMeanSST"))

head(SST_est)
dim(SST_est)
# Red numbers are in red.

# Show the value of the estimate.

index <- c(1:335) # Create index
# SSTestPlot <- SST_est %>%
#   ggplot(aes(index, estimate)) + geom_point() + geom_hline(yintercept = 0, colour = "red") +
#   labs(y = "Estimated coefficient for the sea surface temperature", x = "Index") +
#   theme_classic()
# 
# pdf(file = "figures/Figure11.pdf")
# SSTestPlot
# dev.off()

# What are the two points with an estimate greater than 15?

# Shown on plot
grt15 <- SST_est %>%
  ggplot(aes(index, estimate, label=TaxonomicName)) + 
  geom_point() + 
  geom_hline(yintercept = 0, colour = "red") +
  geom_text(aes(label = ifelse(estimate > 15, TaxonomicName, '')), size = 3, hjust= -0.1) +
  labs(y = "Estimated coefficient for sea surface temperature", x = "Index") +
  theme_classic()

pdf(file = "figures/Figure10.pdf")
grt15
dev.off()

# Checking their summaries 
DN_data <- filter(data, TaxonomicName %in% "Dactylophora nigricans")
AG_data <- filter(data, TaxonomicName %in% "Achoerodus gouldii")

DN.mm <- lmer(SizeClass ~ ScaledMeanSST + (1|SurveyID), REML=T, data = DN_data)
AG.mm <- lmer(SizeClass ~ ScaledMeanSST + (1|SurveyID), REML=T, data = AG_data)

# Homegeneity.
homoDN <- ggplot(DN.mm, aes(x= .fitted, y= .resid)) + 
  geom_point(shape=1, size=2) +
  geom_hline(yintercept=0, col="red", linetype="dashed") +
  geom_smooth(method = "loess", span=100000, se=F) +
  labs(title = "Residuals versus fitted values", y="Residuals", x="Fitted values") +
  theme_classic()

homoAG <- ggplot(AG.mm, aes(x= .fitted, y= .resid)) + 
  geom_point(shape=1, size=2) +
  geom_hline(yintercept=0, col="red", linetype="dashed") +
  geom_smooth(method = "loess", se=F) +
  labs(title = "Residuals versus fitted values", y="Residuals", x="Fitted values") +
  theme_classic()

# Normality.
normDN <- ggplot(DN.mm, aes(x = .resid)) +
  geom_histogram(bins = 10, fill = "white", colour = "black") +
  labs(title = "Histogram of residuals", x = "Residuals", y = "Frequency") +
  theme_classic()

normAG <- ggplot(AG.mm, aes(x = .resid)) +
  geom_histogram(bins = 10, fill = "white", colour = "black") +
  labs(title = "Histogram of residuals", x = "Residuals", y = "Frequency") +
  theme_classic()

# Independence.
indDN <- ggplot(DN_data, aes(x = ScaledMeanSST, y = resid(DN.mm))) +
  geom_point(shape = 1, size = 2) + 
  labs(title = "Explanatory variable vs residuals", x = "Scaled Mean SST (°C)", y = "Residuals") +
  theme_classic()

indAG <- ggplot(AG_data, aes(x = ScaledMeanSST, y = resid(AG.mm))) +
  geom_point(shape = 1, size = 2) + 
  labs(title = "Explanatory variable vs residuals", x = "Scaled Mean SST (°C)", y = "Residuals") +
  theme_classic()

pdf(file = "figures/Figure11.pdf")
(homoDN + homoAG) / (normDN + normAG) / (indDN + indAG) &
  plot_annotation(tag_levels = c("A", "B", "C", "D", "E", "F")) &
  theme(plot.tag = element_text(face = 2, size = 15)) # & operator applies tag style to all plots
dev.off()

# Keep.
# Cautious to remove as observations with extreme values in ecology is interesting.
# Normality and independent assumptions are good.

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
