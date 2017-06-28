setwd("~/Desktop/design-of-experiments/Lab4/")

results <- read.table('response.txt', sep=' ', header=TRUE)
lyft <- read.table('design.txt', sep='\t', header=TRUE)
lyft$y <- results$y
colnames(lyft) <- c('A', 'B', 'C', 'D', 'y')
lyft$AB <- lyft$A*lyft$B
lyft$AC <- lyft$A*lyft$C
lyft$AD <- lyft$A*lyft$D
lyft$BC <- lyft$B*lyft$C
lyft$BD <- lyft$B*lyft$D
lyft$CD <- lyft$C*lyft$D
#lyft$ABC <- lyft$AB*lyft$C
#lyft$ABD <- lyft$AB*lyft$D
#lyft$ACD <- lyft$AC*lyft$D
#lyft$BCD <- lyft$BC*lyft$D
#lyft$ABCD <- lyft$AB*lyft$CD

m <- lm(y ~ ., data=lyft)
summary(m)
anova(m)
confint(m)

# QQ-plot of effects
par(mfrow=c(1,1))
effects <- 2*m$coefficients[2:length(m$coefficients)]
q <- qqnorm(effects, main = "QQ-Plot of Effects")
qqline(effects)
abs(q$y)[order(-abs(q$y))] #figure out which effects are the ones lying away from the line

## Main effects plots
library(gplots)
par(mfrow=c(2,2), oma = c(0,0,2,0))
plotmeans(formula = y~A, ylab = "Booking Rate", xlab = "Discount amount (A)", data=lyft)
plotmeans(formula = y~B, ylab = "Booking Rate", xlab = "Discount duration (B)", data=lyft)
plotmeans(formula = y~C, ylab = "Booking Rate", xlab = "Method of dissemination (C)", data=lyft)
plotmeans(formula = y~D, ylab = "Booking Rate", xlab = "Ride type (D)", data=lyft)
mtext("Main Effect Plots", outer = TRUE, cex = 1.5)

## Interaction plots
par(mfrow=c(2,3), oma = c(0,0,2,0))
interaction.plot(lyft$A, lyft$B, lyft$y, ylab = "Mean Booking Rate", xlab = "Discount amount (A)", main = "A:B Interaction", legend = TRUE)
interaction.plot(lyft$A, lyft$C, lyft$y, ylab = "Mean Booking Rate", xlab = "Discount amount (A)", main = "A:C Interaction", legend = TRUE)
interaction.plot(lyft$A, lyft$D, lyft$y, ylab = "Mean Booking Rate", xlab = "Discount amount (A)", main = "A:D Interaction", legend = TRUE)
interaction.plot(lyft$B, lyft$C, lyft$y, ylab = "Mean Booking Rate", xlab = "Discount duration (B)", main = "B:C Interaction", legend = TRUE)
interaction.plot(lyft$B, lyft$D, lyft$y, ylab = "Mean Booking Rate", xlab = "Discount duration (B)", main = "B:D Interaction", legend = TRUE)
interaction.plot(lyft$C, lyft$D, lyft$y, ylab = "Mean Booking Rate", xlab = "Method of dissemination (C)", main = "C:D Interaction", legend = TRUE)

## Fitted model
resp.surf <- function(x1, x2, x3, x4, model){
  beta <- as.numeric(model$coefficients)
  y.hat <- beta[1] + beta[2]*x1 + beta[3]*x2 + beta[4]*x3 + beta[5]*x4 +
    beta[6]*x1*x2 + beta[7]*x1*x3 + beta[8]*x1*x4 + beta[9]*x2*x3 + beta[10]*x2*x4 +
    beta[11]*x3*x4
  return(y.hat)
}

## Since the factors are qualitative, we can easily find the optimal combinations of levels that 
## maximize the response by iterating through all possible combinations of levels
fitted <- matrix(0, nrow = 16, ncol = 1)
for(i in 1:16){
  x1 <- lyft[i, 1]
  x2 <- lyft[i, 2]
  x3 <- lyft[i, 3]
  x4 <- lyft[i, 4]
  fitted[i] <- resp.surf(x1, x2, x3, x4, m)
}

d <- data.frame(lyft[,1:4], fitted)
d[order(-d$fitted),][1,]

# Residual Analysis
par(mfrow = c(2,2))
hist(m$residuals, main = "Histogram of Residuals", xlab = "")
qqnorm(m$residuals, main = "QQ-Plot of Residuals")
qqline(m$residuals, col = "red")
plot(m$residuals, main = "Residuals vs. Order", ylab = "Residuals")
abline(h = 0, col = "red")
plot(m$fitted, m2$residuals, main = "Residuals vs. Fitted Values", ylab = "Residuals", xlab = "Fitted Values")
abline(h = 0, col = "red")
