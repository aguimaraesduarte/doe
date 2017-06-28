## Instagram Analysis
setwd("/users/ntstevens/Dropbox/Teaching/MSAN_631/2017/Lectures/")
data <- read.csv(file = "instagram-factorial.csv", header = T)
Time <- data$Time
Prevalence <- factor(data$Prevalence)
Type <- factor(data$Type, levels = c(0,1), labels = c("Photo", "Video"))

## Numerical summaries of the data
summary(data.frame(Time, Prevalence, Type))
sd(Time)

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
summary(model)
anova(model)

## Notes:
 # Our intuition based on the plots was correct: the interaction is minimal but significant, and 
 # both main effects are, with "Prevalence" explaining most of the variation in engagement time.

## Quantifcation of "Prevalence" main effect
timeBYprev <- data.frame(Prev0 = data$Time[Prevalence==0], Prev0.1 = data$Time[Prevalence==0.1], Prev0.2 = data$Time[Prevalence==0.2], Prev0.5 = data$Time[Prevalence==0.5])
timeBYprev <- data.matrix(timeBYprev)
apply(timeBYprev, 2, mean) # average engagement time by ad prevalence category

effect.mat <- matrix(0, nrow = 4, ncol = 4)
pvalue.mat <- matrix(0, nrow = 4, ncol = 4)
prev <- c(0, 0.1, 0.2, 0.5)
for (i in 1:4){
  for (j in 1:4){
    ttest <- t.test(data$Time[Prevalence==prev[i]], data$Time[Prevalence==prev[j]])
    effect.mat[i,j] <- as.numeric(diff(ttest$estimate))
    pvalue.mat[i,j] <- round(ttest$p.value, digits = 4)
  }
}
# The following matrix shows the expected change in engagement time when going from one level of prevalence to another
# The rows and columns correspond to levels 0, 0.1, 0.2, 0.5
effect.mat
pvalue.mat

## Quantification of "Type" main effect
mean(data$Time[data$Type == 1] - data$Time[data$Type == 0])
t.test(data$Time[data$Type == 1], data$Time[data$Type == 0], alternative = "two.sided")
t.test(data$Time[data$Type == 1], data$Time[data$Type == 0], alternative = "greater")

## Notes:
# On average the engagement time is 4.86 seconds longer when ads are videos vs. photos. The 
# 95% CI for this difference is (1.17, 8.55). This represents a significant increase in 
# engagement time, at a 1% significance level.

#######################################################################################
## But main effects don't tell the whole story when significant interaction is present. 
## We need to incorporate this into our understanding of what an "effect" is.
#######################################################################################

## The "effect" of Prevalence when ad type is a photo
timeBYprev0 <- data.frame(Prev0 = data$Time[data$Prevalence==0 & data$Type==0], Prev0.1 = data$Time[data$Prevalence==0.1 & data$Type==0], Prev0.2 = data$Time[data$Prevalence==0.2 & data$Type==0], Prev0.5 = data$Time[data$Prevalence==0.5 & data$Type==0])
timeBYprev0 <- data.matrix(timeBYprev0)
apply(timeBYprev0, 2, mean) # average engagement time by ad prevalence category

effect.mat0 <- matrix(0, nrow = 4, ncol = 4)
prev <- c(0, 0.1, 0.2, 0.5)
for (i in 1:4){
  for (j in 1:4){
    ttest <- t.test(data$Time[data$Prevalence==prev[i] & data$Type==0], data$Time[data$Prevalence==prev[j] & data$Type==0])
    effect.mat0[i,j] <- as.numeric(diff(ttest$estimate))
  }
}
# The following matrix shows the expected change in engagement time when going from one level of prevalence to another
# The rows and columns correspond to levels 0, 0.1, 0.2, 0.5
effect.mat0

## The "effect" of Prevalence when ad Type is a video
timeBYprev1 <- data.frame(Prev0 = data$Time[data$Prevalence==0 & data$Type==1], Prev0.1 = data$Time[data$Prevalence==0.1 & data$Type==1], Prev0.2 = data$Time[data$Prevalence==0.2 & data$Type==1], Prev0.5 = data$Time[data$Prevalence==0.5 & data$Type==1])
timeBYprev1 <- data.matrix(timeBYprev1)
apply(timeBYprev1, 2, mean) # average engagement time by ad prevalence category

effect.mat1 <- matrix(0, nrow = 4, ncol = 4)
prev <- c(0, 0.1, 0.2, 0.5)
for (i in 1:4){
  for (j in 1:4){
    ttest <- t.test(data$Time[data$Prevalence==prev[i] & data$Type==1], data$Time[data$Prevalence==prev[j] & data$Type==1])
    effect.mat1[i,j] <- as.numeric(diff(ttest$estimate))
  }
}
# The following matrix shows the expected change in engagement time when going from one level of prevalence to another
# The rows and columns correspond to levels 0, 0.1, 0.2, 0.5
effect.mat1

## The "effect" of ad Type when Prevalence is 0
mean(data$Time[data$Type == 1 & data$Prevalence==0] - data$Time[data$Type == 0 & data$Prevalence==0])
t.test(data$Time[data$Type == 1 & data$Prevalence==0], data$Time[data$Type == 0 & data$Prevalence==0], alternative = "two.sided")

## The "effect" of ad Type when Prevalence is 0.1
mean(data$Time[data$Type == 1 & data$Prevalence==0.1] - data$Time[data$Type == 0 & data$Prevalence==0.1])
t.test(data$Time[data$Type == 1 & data$Prevalence==0.1], data$Time[data$Type == 0 & data$Prevalence==0.1], alternative = "two.sided")

## The "effect" of ad Type when Prevalence is 0.2
mean(data$Time[data$Type == 1 & data$Prevalence==0.2] - data$Time[data$Type == 0 & data$Prevalence==0.2])
t.test(data$Time[data$Type == 1 & data$Prevalence==0.2], data$Time[data$Type == 0 & data$Prevalence==0.2], alternative = "two.sided")

## The "effect" of ad Type when Prevalence is 0.5
mean(data$Time[data$Type == 1 & data$Prevalence==0.5] - data$Time[data$Type == 0 & data$Prevalence==0.5])
t.test(data$Time[data$Type == 1 & data$Prevalence==0.5], data$Time[data$Type == 0 & data$Prevalence==0.5], alternative = "two.sided")


# Residual Analysis
par(mfrow = c(2,2))
hist(model$residuals, main = "Histogram of Residuals", xlab = "")
qqnorm(model$residuals, main = "QQ-Plot of Residuals")
qqline(model$residuals, col = "red")
plot(model$residuals, main = "Residuals vs. Order", ylab = "Residuals")
abline(h = 0, col = "red")
plot(model$fitted, model$residuals, main = "Residuals vs. Fitted Values", ylab = "Residuals", xlab = "Fitted Values")
abline(h = 0, col = "red")
