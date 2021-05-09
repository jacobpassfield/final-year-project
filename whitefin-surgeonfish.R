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
WS_data <- data %>% filter(TaxonomicName %in% "Acanthurus albipectoralis")

# Centering and scaling mean SST
WS_data <- WS_data %>% mutate(ScaledMeanSST = scale(MeanSST, center = T, scale = T))

# # # SIMPLE LINEAR REGRESSION # # #

# Fitting simple linear regression model
WS.lm <- lm(SizeClass ~ ScaledMeanSST, data = WS_data)

# Obtaining estimates
summary(WS.lm)
# alpha: 20.7867, beta: 0.9553, residual variance: 5.837^2, p-value: 8.13e-08

# Creating plot of the model
lmPlot <- ggplot(WS_data, aes(x=ScaledMeanSST, y=SizeClass)) +
  geom_point(alpha = 0.1, size = 3) + 
  geom_smooth(method = "lm", size = 1, se = F) + 
  theme_classic() +
  theme_parameters +
  labs(title = "SST versus size of whitefin surgeonfish",
       subtitle = "Using a simple linear regression model",
       y = "Size class (cm)", x = "Scaled Mean SST (°C)") 

# Save
pdf(file = "figures/figure-2.pdf")
lmPlot
dev.off()

# Model validation graphs with help from https://rpubs.com/therimalaya/43190
# Homoscedasticity
homo.lm <- ggplot(WS.lm, aes(x = .fitted, y = .resid)) + 
  geom_jitter(shape = 1, size = 2, width = 0.1, height = 0.5) +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = F) +
  theme_classic() +
  theme_parameters +
  labs(title = "Residuals versus fitted values", y = "Residuals", x = "Fitted values")
# Normality
norm.lm <- ggplot(WS.lm, aes(x = .resid)) +
  geom_histogram(bins = 10, fill = "white", colour = "black") +
  theme_classic() +
  theme_parameters +
  labs(title = "Histogram of the residuals", x = "Residuals", y = "Frequency")

# Save
pdf(file = "figures/figure-3.pdf")
(homo.lm / norm.lm) + 
  plot_annotation(tag_levels = c("A", "B", "C")) &
    theme(plot.tag = element_text(face = 2, size = 15)) # & operator applies tag style to all plots
dev.off()

# Ensure geogroup, year and surveyID are factors
WS_data$Geogroup <- factor(WS_data$Geogroup)
WS_data$SurveyID <- factor(WS_data$SurveyID)
WS_data$Year <- factor(WS_data$Year)

# Geogroup against residuals as a boxplot since geogroup is a factor
geo.res <- ggplot(WS_data, aes(x = Geogroup, y = resid(WS.lm))) + 
  geom_boxplot() +
  theme_classic() +
  theme_parameters +
  labs(title = "Residuals versus geographic cell", y = "Residuals", x = "Cell") +
  coord_flip()
# The spread of the residuals is not the same in all cells
# Various cells have no spread, cells 3005 and 4434 for example
# Then there is less spread in 1546 than 3533

# Save
pdf(file = "figures/figure-4.pdf")
geo.res
dev.off()

# Running seperate analyses using cell to demonstare
geolmPlot <- ggplot(WS_data, aes(x=ScaledMeanSST, y=SizeClass)) +
  geom_point(alpha = 0.1) +
  facet_wrap(~Geogroup) +
  theme_light() +
  theme(axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 15),
        strip.text = element_text(size = 15)) +
  theme(legend.position = "none") +
  labs(y="Size class (cm)", x="Scaled mean SST (°C)")

# Save
pdf(file = "figures/figure-5.pdf")
geolmPlot
dev.off()

# # # MIXED EFFECTS MODELLING # # #

# Creating different models
mm1  <- lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup), REML = T, data = WS_data)
mm2  <- lmer(SizeClass ~ ScaledMeanSST + (1|Year), REML = T, data = WS_data)
mm3  <- lmer(SizeClass ~ ScaledMeanSST + (1|SurveyID), REML = T, data = WS_data)

mm4 <- lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup/Year/SurveyID), REML = T, data = WS_data) # boundary is singular

mm5 <- lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup/Year), REML = T, data = WS_data)
mm6 <- lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup/SurveyID), REML = T, data = WS_data)
mm7 <- lmer(SizeClass ~ ScaledMeanSST + (1|Year/SurveyID), REML = T, data = WS_data)

# Finding BIC value
BIC(mm1, mm2, mm3, mm4, mm5, mm6, mm7)
#     df      BIC
# mm1  4 6458.854
# mm2  4 6646.322
# mm3  4 5343.703
# mm4  N/A
# mm5  5 6370.123
# mm6  5 5341.405 < < <
# mm7  5 5341.479

# mm7 lowest BIC and so best model
WS.mm <- mm6

# Model validation graphs
# Homoscedasticity
homo.mm <- ggplot(WS.mm, aes(x = .fitted, y = .resid)) + 
  geom_jitter(shape = 1, size = 1, width = 0.1, height = 0.5) +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = F) +
  theme_classic() +
  theme_parameters +
  labs(title = "Residuals vs fitted vals", y = "Residuals", x = "Fitted values")
# Normality of residuals
norm.mm <- ggplot(WS.mm, aes(x = .resid)) +
  geom_histogram(bins = 10, fill = "white", colour = "black") +
  theme_classic() +
  theme_parameters +
  labs(title = "Histogram of residuals", x = "Residuals", y = "Frequency")
# Normality of random effects
# http://www.sthda.com/english/wiki/normality-test-in-r
# "From the output, the p-value > 0.05 implying that the distribution of the data are not 
# significantly different from normal distribution. In other words, we can assume the normality."
# Geogroup
re.geo <- ranef(WS.mm)$Geogroup$`(Intercept)`
qq1.mm <- ggqqplot(re.geo, conf.int.level = 0, shape = 19) +
  theme_parameters +
  labs(title = "Normal Q-Q plot", subtitle = "Random effect - cell", x = "Theoretical quantiles", y = "Sample quantiles")
# SurveyID:Geogroup
re.surgeo <- ranef(WS.mm)$`SurveyID:Geogroup`$`(Intercept)`
qq2.mm <- ggqqplot(re.surgeo, conf.int.level = 0, shape = 19) +
  theme_parameters +
  labs(title = "Normal Q-Q plot", subtitle = "Random effect - cell/survey", x = "Theoretical quantiles", y = "Sample quantiles")

# Save
pdf(file = "figures/figure-6.pdf")
(homo.mm + norm.mm) / (qq1.mm + qq2.mm) + 
  plot_annotation(tag_levels = c("A", "B", "C", "D")) &
    theme(plot.tag = element_text(face = 2, size = 15))
dev.off()

# Overall predictions for the model
WS.pred.mm <- ggpredict(WS.mm, terms = c("ScaledMeanSST"))

# Plot the predictions 
mmPlot <- ggplot(WS.pred.mm) + 
  geom_line(aes(x = x, y = predicted), colour = "blue", size = 1) + # slope
  geom_point(data = WS_data,  # adding the raw data (scaled values)
             aes(x = ScaledMeanSST, y = SizeClass), alpha = 0.1, size = 3) + 
  labs(y="Size class (cm)", x="Scaled Mean SST (°C)", 
       title = "SST versus size of whitefin surgeonfish",
       subtitle = "Using a linear mixed-effects model") + 
  theme_minimal() +
  theme_parameters +
  theme(legend.position="none")
# Decrease!

# Save
pdf(file = "figures/figure-7.pdf")
mmPlot
dev.off()

# Obtaining estimates
summary(WS.mm)
# alpha: 22.7507, beta: 3.0789, residual variance: 2.423^2, 
# cell variance: 3.256^2, cell/survey variance: 4.615^2, p-value: 8.86e-06
# The resulting p-value for beta is 8.86e-06 < 0.05 and so beta is significant.




