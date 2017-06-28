## Instagram Analysis
setwd("~/Desktop/design-of-experiments/Final/")
data <- read.csv(file = "instagram-factorial.csv", header = T)
Time <- data$Time
Prevalence <- factor(data$Prevalence)
Type <- factor(data$Type, levels = c(0,1), labels = c("Photo", "Video"))

## Graphical Summaries of the data
library(gplots)
par(mfrow = c(1,2))
boxplot(Time ~ Prevalence, main = "Boxplot of Engagement Time by Ad Prevalence", xlab = "Ad Prevalence", ylab = "Time Engaged (s)")
plotmeans(Time ~ Prevalence, main = "Main Effect Plot of Engagement Time by Ad Prevalence", xlab = "Ad Prevalence", ylab = "Time Engaged (s)")
boxplot(Time ~ Type, main = "Boxplot of Engagement Time by Ad Type", xlab = "Ad Type", ylab = "Time Engaged (s)")
plotmeans(Time ~ Type, main = "Main Effect Plot of Engagement Time by Ad Type", xlab = "Ad Type", ylab = "Time Engaged (s)")
par(mfrow = c(1,1))
interaction.plot(Prevalence, Type, Time, main = "Interaction Between Ad Prevalence and Ad Type", ylab = "Mean Engagement Time (s)", xlab = "Ad Prevalence")

## Notes:
# Prevalence seems to be have a large and significant main effect
# Type seems to have a small and maybe significant main effect
# Interaction between Prevalence and Type is existent but minimal
# Thus, the main effects seem to drive variation in the response (engagement time)

## Formal Analysis of Variance
model <- lm(Time ~ Prevalence * Type)
anova(model)

## Notes:
# Our intuition based on the plots was correct: the interaction is minimal but significant, and 
# both main effects are, with "Prevalence" explaining most of the variation in engagement time.

## Summary of the model
summary(model)

## Notes:
# Prevalence=0 and type=Video have the highest response, but this combination does not make sense

# Residual Analysis
par(mfrow = c(2,2))
hist(model$residuals, main = "Histogram of Residuals", xlab = "")
qqnorm(model$residuals, main = "QQ-Plot of Residuals")
qqline(model$residuals, col = "red")
plot(model$residuals, main = "Residuals vs. Order", ylab = "Residuals")
abline(h = 0, col = "red")
plot(model$fitted, model$residuals, main = "Residuals vs. Fitted Values", ylab = "Residuals", xlab = "Fitted Values")
abline(h = 0, col = "red")
