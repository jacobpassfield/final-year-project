library(tidyverse)
library(ggplot2)
library(patchwork)
library(lmerTest)
library(ggeffects)
library(ggpubr)

# Create plot and title axis parameters
theme_parameters <- theme(plot.title = element_text(size = 15),
                          plot.subtitle = element_text(size = 12),
                          axis.title = element_text(size = 15),
                          axis.text = element_text(size = 15),
                          strip.text = element_text(size = 15))

# Loading data
load(file = "data/data.RData")

# Creating data fram with Pearly Wrasse only
PW_data <- data %>% filter(TaxonomicName %in% "Halichoeres margaritaceus")

# Centering and scaling mean SST
PW_data <- PW_data %>% mutate(ScaledMeanSST = scale(MeanSST, center = T, scale = T))

# # # SIMPLE LINEAR REGRESSION # # #

# Fitting simple linear regression model
PW.lm <- lm(SizeClass ~ ScaledMeanSST, data = PW_data)

# Obtaining estimates
summary(PW.lm)
# alpha: 6.04911, beta: -0.88658, residual variance: 2.538^2

# Creating plot of the model
lmPlot <- ggplot(PW_data, aes(x=ScaledMeanSST, y=SizeClass)) +
  geom_point(alpha = 0.1, size = 3) + 
  geom_smooth(method = "lm", size = 1, se = F) + 
  theme_classic() +
  theme_parameters +
  labs(title = "How temperature affects the body size of Pearly Wrasse",
       subtitle = "Using a simple linear regression model",
       y = "Size class (cm)", x = "Scaled Mean SST (°C)") 

# Save plot
# pdf(file = "figures/Figure?.pdf")
# lmPlot
# dev.off()

# Model validation graphs with help from https://rpubs.com/therimalaya/43190
# Homoscedasticity
homo.lm <- ggplot(PW.lm, aes(x = .fitted, y = .resid)) + 
  geom_jitter(shape = 1, size = 2, width = 0.1, height = 0.5) +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = F) +
  theme_classic() +
  theme_parameters +
  labs(title = "Residuals versus fitted values", y = "Residuals", x = "Fitted values")
# Normality
norm.lm <- ggplot(PW.lm, aes(x = .resid)) +
  geom_histogram(bins = 10, fill = "white", colour = "black") +
  theme_classic() +
  theme_parameters +
  labs(title = "Histogram of the residuals", x = "Residuals", y = "Frequency")

# Save
# pdf(file = "figures/Figure?.pdf")
# (homo.lm / norm.lm) + 
#   plot_annotation(tag_levels = c("A", "B", "C")) &
#     theme(plot.tag = element_text(face = 2, size = 15)) # & operator applies tag style to all plots
# dev.off()

# Ensure geogroup, year and surveyID are factors
PW_data$Geogroup <- factor(PW_data$Geogroup)
PW_data$SurveyID <- factor(PW_data$SurveyID)
PW_data$Year <- factor(PW_data$Year)

# Geogroup against residuals as a boxplot since geogroup is a factor
geo.res <- ggplot(PW_data, aes(x = Geogroup, y = resid(PW.lm))) + 
  geom_boxplot() +
  theme_classic() +
  theme_parameters +
  labs(title = "Residuals versus geographic cell", y = "Residuals", x = "Cell") +
  coord_flip()
# The spread of the residuals is not the same in all cells.
# Various cells have no spread, cells 2733 and 4500 for example.
# Then there is less spread in 3384 than 3457.

# Save
# pdf(file = "figures/Figure?.pdf")
# geo.res
# dev.off()

# Running seperate analyses using cell to demonstare
geolmPlot <- ggplot(PW_data, aes(x=ScaledMeanSST, y=SizeClass)) +
  geom_point() +
  facet_wrap(~Geogroup) +
  theme_light() +
  theme_parameters +
  theme(legend.position = "none") +
  labs(y="Size class (cm)", x="Scaled mean SST (°C)")

# pdf(file = "figures/Figure?.pdf")
# geolmPlot
# dev.off()

# # # MIXED EFFECTS MODELLING # # #

# Creating different models
mm1  <- lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup), REML = T, data = PW_data)
mm2  <- lmer(SizeClass ~ ScaledMeanSST + (1|Year), REML = T, data = PW_data)
mm3  <- lmer(SizeClass ~ ScaledMeanSST + (1|SurveyID), REML = T, data = PW_data)

# https://yury-zablotski.netlify.app/post/mixed-effects-models-2/

mm4 <- lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup) + (1|Year), REML = T, data = PW_data)
mm5 <- lmer(SizeClass ~ ScaledMeanSST + (1|SurveyID) + (1|Year), REML = T, data = PW_data)

mm6 <- lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup/SurveyID) + (1|Year), REML = T, data = PW_data)
mm7 <- lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup/SurveyID), REML = T, data = PW_data)

# Finding BIC value
BIC(mm1, mm2, mm3, mm4, mm5, mm6, mm7)
#     df      BIC
# mm1  4 4416.070
# mm2  4 4747.702
# mm3  4 4375.641
# mm4  5 4405.636
# mm5  5 4380.878
# mm6  6 4372.073
# mm7  5 4369.391 < < <

# mm7 lowest BIC and so best model
PW.mm <- mm7

# Model validation graphs
# Homoscedasticity
homo.mm <- ggplot(PW.mm, aes(x = .fitted, y = .resid)) + 
  geom_jitter(shape = 1, size = 2, width = 0.1, height = 0.5) +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = F) +
  theme_classic() +
  theme_parameters +
  labs(title = "Residuals versus fitted values", y = "Residuals", x = "Fitted values")
# Normality of residuals
norm.mm <- ggplot(PW.mm, aes(x = .resid)) +
  geom_histogram(bins = 10, fill = "white", colour = "black") +
  theme_classic() +
  theme_parameters +
  labs(title = "Histogram of the residuals", x = "Residuals", y = "Frequency")
# Normality of random effects
# http://www.sthda.com/english/wiki/normality-test-in-r
# From the output, the p-value > 0.05 implying that the distribution of the data are not 
# significantly different from normal distribution. In other words, we can assume the normality.
# Geogroup
re.geo <- ranef(PW.mm)$Geogroup$`(Intercept)`
qq1.mm <- ggqqplot(re.geo, conf.int.level = 0, shape = 19) +
  theme_parameters +
  labs(title = "Normal Q-Q plot", subtitle = "Random effect - cell", x = "Theoretical quantiles", y = "Sample quantiles")
# SurveyID:Geogroup
re.surgeo <- ranef(PW.mm)$`SurveyID:Geogroup`$`(Intercept)`
qq2.mm <- ggqqplot(re.surgeo, conf.int.level = 0, shape = 19) +
  theme_parameters +
  labs(title = "Normal Q-Q plot", subtitle = "Random effect - cell/survey", x = "Theoretical quantiles", y = "Sample quantiles")
# Shapiro-Test for normality
shapiro.test(re.geo) # p-value = 0.4109
shapiro.test(re.surgeo) # p-value = 0.06982
# Can assume normality.

# Save
# pdf(file = "figures/Figure?.pdf")
# (homo.mm + norm.mm) / (qq1.mm + qq2.mm) + 
#   plot_annotation(tag_levels = c("A", "B", "C", "D")) &
#     theme(plot.tag = element_text(face = 2, size = 15))
# dev.off()

# Overall predictions for the model
PW.pred.mm <- ggpredict(PW.mm, terms = c("ScaledMeanSST"))

# Plot the predictions 
mmPlot <- ggplot(PW.pred.mm) + 
  geom_line(aes(x = x, y = predicted), colour = "blue", size = 1) + # slope
  geom_point(data = PW_data,  # adding the raw data (scaled values)
             aes(x = ScaledMeanSST, y = SizeClass), alpha = 0.1, size = 3) + 
  labs(y="Size class (cm)", x="Scaled Mean SST (°C)", 
       title = "How temperature affects the body size of Pearly Wrasse",
       subtitle = "Using a linear mixed-effects model") + 
  theme_minimal() +
  theme_parameters +
  theme(legend.position="none")
# Decrease!

# Save
#pdf(file = "figures/Figure?.pdf")
#mmPlot
#dev.off()

# Obtaining estimates
summary(PW.mm)
# The resulting p-value for beta is 0.313 > 0.05 and so beta is significant.

