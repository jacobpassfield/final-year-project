load(file = "data/data.RData")

library(tidyverse)

PW_data <- data %>% filter(TaxonomicName %in% "Halichoeres margaritaceus")

PW_data <- PW_data %>% mutate(ScaledMeanSST = scale(MeanSST, center = T, scale = T))

PW.lm <- lm(SizeClass ~ ScaledMeanSST, data = PW_data)

summary(PW.lm)

library(ggplot2)

lmPlot <- ggplot(PW_data, aes(x=ScaledMeanSST, y=SizeClass)) +
  labs(y="Size class (cm)", x="Scaled Mean SST (°C)", 
       title = "How temperature affects the body size of Pearly Wrasse",
       subtitle = "Using a simple linear regression model") +
  geom_point(alpha = 0.1, size=3) + 
  geom_smooth(method="lm", size=2, se = F) +
  theme_classic()

pdf(file = "figures/Figure2.pdf")
lmPlot
dev.off()

library(broom)

#https://rpubs.com/therimalaya/43190

# Homegeneity.
homo1 <- ggplot(PW.lm, aes(x= .fitted, y= .resid)) + 
  geom_point(shape=1, size=2) +
  geom_hline(yintercept=0, col="red", linetype="dashed") +
  geom_smooth(method = "loess", se=F) +
  labs(title = "Residuals versus fitted values", y="Residuals", x="Fitted values") +
  theme_classic()

# Homegeneity.
# homo1 <- ggplot(PW.lm, aes(x= .fitted, y= sqrt(abs(.stdresid)))) + 
#  geom_point(shape=1, size=2) +
#  geom_smooth(method = "loess", span=1, se=F) +
#  labs(title= "Scale-Location", y=expression(sqrt("|Standardised residuals|")), x="Fitted values") +
#  theme_classic()

# Normality.
norm1 <- ggplot(PW.lm, aes(x = .resid)) +
  geom_histogram(bins = 10, fill = "white", colour = "black") +
  labs(title = "Histogram of residuals", x = "Residuals", y = "Frequency") +
  theme_classic()

# Independence.
ind1 <- ggplot(PW_data, aes(x = ScaledMeanSST, y = resid(PW.lm))) +
  geom_point(shape = 1, size = 2) + 
  labs(title = "Explanatory variable versus residuals", x = "Scaled Mean SST (°C)", y = "Residuals") +
  theme_classic()

library(patchwork)
pdf(file = "figures/Figure3.pdf")
(homo1 + norm1) / ind1 + 
  plot_annotation(tag_levels = c("A", "B", "C")) &
  theme(plot.tag = element_text(face = 2, size = 15)) # & operator applies tag style to all plots
dev.off()

# library(ggpubr)
# http://www.sthda.com/english/articles/32-r-graphics-essentials/126-combine-multiple-ggplots-in-one-graph/
# pdf(file = "figures/Figure3.pdf")
# ggarrange(rf1, homo1, norm1, ind1,
#           labels = c("A", "B", "C", "D"),
#           ncol = 2, nrow = 2)
# dev.off()

PW_data$Geogroup <- factor(PW_data$Geogroup)

PW_data$SurveyID <- factor(PW_data$SurveyID)

PW_data$Year <- factor(PW_data$Year)

# GEOGROUP
#  Scatterplot
geoScp <- ggplot(PW_data, aes(x=ScaledMeanSST, y=SizeClass, colour=Geogroup)) +
  labs(title = "Coloured by Geogroup", y="Size class (cm)", x="Scaled mean SST (°C)") +
  geom_point(size = 3) + 
  theme_classic() + 
  theme(legend.position="none")
#  Boxplot
geoBxp <- ggplot(PW_data, aes(x=SizeClass, y=Geogroup)) +
  labs(y="Size class (cm)", x="Geogroup") +
  geom_boxplot() + 
  theme_classic()
## SURVEYID
##  Scatterplot
#svyScp <- ggplot(PW_data, aes(x=ScaledMeanSST, y=SizeClass, colour=SurveyID)) +
#  labs(title = "Coloured by SurveyID", y="Size class (cm)", x="Scaled mean SST (°C)") +
#  geom_point() + 
#  theme_classic() + 
#  theme(legend.position="none")
##  Boxplot
#svyBxp <- ggplot(PW_data, aes(x=SizeClass, y=SurveyID)) +
#  labs(x="Size class (cm)", y="SurveyID") +
#  geom_boxplot() + 
#  theme_classic(base_size = 8)
## YEAR
##  Scatterplot
#yerScp <- ggplot(PW_data, aes(x=ScaledMeanSST, y=SizeClass, colour=Year)) +
#  labs(title = "Coloured by Year", y="Size class (cm)", x="Scaled mean SST (°C)") +
#  geom_point() + 
#  theme_classic() + 
#  theme(legend.position="none")
##  Boxplot
#yerBxp <- ggplot(PW_data, aes(x=SizeClass, y=Year)) +
#  labs(y="Size class (cm)", x="Year") +
#  geom_boxplot() + 
#  theme_classic(base_size = 8)

pdf(file = "figures/Figure4.pdf")
geoScp
dev.off()

pdf(file = "figures/Figure5.pdf")
geoBxp
dev.off()

# pdf(file = "figures/Figure4.pdf")
# ggarrange(geoScp, yerScp, svyScp,
#           labels = c("A", "B", "C"),
#           nrow = 3)
# dev.off()

# library(patchwork)

# pdf(file = "figures/Figure5.pdf")
# (geoBxp / yerBxp | svyBxp) + 
# plot_annotation(tag_levels = c("A", "B", "C")) &
# theme(plot.tag = element_text(face = 2, size = 15)) # & operator applies tag style to all plots
# dev.off()

# Run seperate analyses
# Demonstrate with geogroup.
PWgeo <- ggplot(PW_data, aes(x=ScaledMeanSST, y=SizeClass)) +
  labs(y="Size class (cm)", x="Scaled mean SST (°C)") +
  geom_point() + 
  facet_wrap(~Geogroup) +
  theme_light() + 
  theme(legend.position="none")

pdf(file = "figures/Figure6.pdf")
PWgeo
dev.off()

### MIXED EFFECTS MODELLING ###

# Deciding how to model the equation
library(lme4)

mm1  <- lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup), REML = T, data = PW_data)
mm2  <- lmer(SizeClass ~ ScaledMeanSST + (1|Year), REML = T, data = PW_data)
mm3  <- lmer(SizeClass ~ ScaledMeanSST + (1|SurveyID), REML = T, data = PW_data)

BIC(mm1, mm2, mm3)

#     df      BIC
# mm1  4 4416.070
# mm2  4 4747.702
# mm3  4 4375.641

mm4 <- lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup/Year/SurveyID), REML = T, data = PW_data) # failed to converge
mm5 <- lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup/Year), REML = T, data = PW_data)
mm6 <- lmer(SizeClass ~ ScaledMeanSST + (1|Geogroup/SurveyID), REML = T, data = PW_data)
mm7 <- lmer(SizeClass ~ ScaledMeanSST + (1|Year/SurveyID), REML = T, data = PW_data)

BIC(mm5, mm6, mm7)

#     df      BIC
# mm5  5 4394.367
# mm6  5 4369.391
# mm7  5 4380.878

# summary("mm[i]")

# Makes sense YearIndex is not used as spread wasn't drastically different.
# Interesting about MM3 < MM1 Geogroup too but survey index encapsulates day and geogroup.

# Using nlme
# f1 <- formula(SizeClass ~ ScaledMeanSST)
# lm <- gls(f1, method = "REML", data = PW_data) # use gls in order to compare models
# mm <- lme(f1, random = ~1 | Geogroup / SurveyID, data = PW_data, method = "REML")
# anova(lm, mm)
#       Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# PW.lm1     1  3 4748.695 4763.436 -2371.347                        
# PW.mm1     2  5 4344.812 4369.381 -2167.406 1 vs 2 407.8828  <.0001

PW.mm <- mm5

library(ggeffects)

PW.pred.mm <- ggpredict(PW.mm, terms = c("ScaledMeanSST")) # this gives overall predictions for the model

# Validation

# Homegeneity.
homo2 <- ggplot(PW.mm, aes(x= .fitted, y= .resid)) + 
  geom_point(shape=1, size=2) +
  geom_hline(yintercept=0, col="red", linetype="dashed") +
  geom_smooth(method = "loess", se=F) +
  labs(title = "Residuals versus fitted values", y="Residuals", x="Fitted values") +
  theme_classic()

# Normality.
norm2 <- ggplot(PW.mm, aes(x = .resid)) +
  geom_histogram(bins = 10, fill = "white", colour = "black") +
  labs(title = "Histogram of residuals", x = "Residuals", y = "Frequency") +
  theme_classic()

# Independence.
ind2 <- ggplot(PW_data, aes(x = ScaledMeanSST, y = resid(PW.mm))) +
  geom_point(shape = 1, size = 2) + 
  labs(title = "Explanatory variable versus residuals", x = "Scaled Mean SST (°C)", y = "Residuals") +
  theme_classic()

pdf(file = "figures/Figure7.pdf")
(homo2 + norm2) / ind2 + 
  plot_annotation(tag_levels = c("A", "B", "C")) &
  theme(plot.tag = element_text(face = 2, size = 15)) # & operator applies tag style to all plots
dev.off()

# Plot the predictions 
mmPlot <- ggplot(PW.pred.mm) + 
  geom_line(aes(x = x, y = predicted), colour = "blue", size = 2) + # slope
  geom_point(data = PW_data,  # adding the raw data (scaled values)
             aes(x = ScaledMeanSST, y = SizeClass), alpha = 0.1, size = 3) + 
  labs(y="Size class (cm)", x="Scaled Mean SST (°C)", 
       title = "How temperature affects the body size of Pearly Wrasse",
       subtitle = "Using a linear mixed-effects model") + 
  theme_minimal() + 
  theme(legend.position="none")
# Decrease!

pdf(file = "figures/Figure8.pdf")
mmPlot
dev.off()

