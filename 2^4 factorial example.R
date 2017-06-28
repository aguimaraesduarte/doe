## Example 6.6 in Montgomery (8th ed.)

## Create the data
A <- kronecker(rep(1,8), c(-1,1))
B <- rep(kronecker(c(-1,1), rep(1,2)), 4)
C <- rep(kronecker(c(-1,1), rep(1,4)), 2)
D <- kronecker(c(-1,1), rep(1,8))
y <- c(2.45, 3.36, 2.16, 2.29, 2.49, 3.39, 2.32, 2.44, 1.84, 2.24, 1.69, 1.87, 2.29, 2.92, 2.04, 2.03)
data <- data.frame(A, B, C, D, y)
data

## Fit a first order regression model to these data with all interaction terms
model.full <- lm(y ~ A * B * C * D)
summary(model.full)
anova(model.full)
## Notes:
 # This is an over-saturated model in which we cannot separately estimate the error
 # variance. We must result to some heuristic techniques to gauge factor significance 
 # and refit a reduced model.

## QQ-plot of effects
par(mfrow=c(1,1))
effects <- 2*model.full$coefficients[2:length(model.full$coefficients)]
q <- qqnorm(effects, main = "QQ-Plot of Effects")
qqline(effects)
abs(q$y)[order(-abs(q$y))] #figure out which effects are the ones lying away from the line
## Notes:
 # Main effects A, B, C, D, and the 2-way interactions A:B and seem to be most significant
 # Let's try to fit a full model with everything except the 3 and 4-way interaction terms
 # and see if we arrive at the same conclusion
model <- lm(y ~ . + .^2 +.^3, data = data)
summary(model)
model <- lm(y ~ . + .^2, data = data)
summary(model)

## Notes:
 # We do see the same thing. Let's now fit a reduced model with just main effects and the 
 # AB interaction term
model.red <- lm(y ~ A+B+C+D+A:B)
summary(model.red)
anova(model.red)

## Main Effects plots
library(gplots)
par(mfrow=c(2,2), oma = c(0,0,2,0)) 
plotmeans(formula = y~A, ylab = "Response Rate", xlab = "Annual Fee (A)")
plotmeans(formula = y~B, ylab = "Response Rate", xlab = "Account-opening Fee (B)")
plotmeans(formula = y~C, ylab = "Response Rate", xlab = "Initial Interest Rate (C)")
plotmeans(formula = y~D, ylab = "Response Rate", xlab = "Long-term Interest Rate (D)")
mtext("Main Effect Plots", outer = TRUE, cex = 1.5)

## Interaction Plots
par(mfrow=c(1,1))
interaction.plot(A, B, y, ylab = "Mean Response Rate", xlab = "Annual Fee (A)", main = "A:B Interaction")

## Fitted model
resp.surf <- function(x1, x2, x3, x4, model){
  beta <- as.numeric(model$coefficients)
  y.hat <- beta[1] + beta[2]*x1 + beta[3]*x2 + beta[4]*x3 + beta[5]*x4 + beta[6]*x1*x2
  return(y.hat)
}

## Since the factors are qualitative, we can easily find the optimal combinations of levels that 
## maximize the response by iterating through all possible combinations of levels
fitted <- matrix(0, nrow = 16, ncol = 1)
for(i in 1:16){
  x1 <- data[i, 1]
  x2 <- data[i, 2]
  x3 <- data[i, 3]
  x4 <- data[i, 4]
  fitted[i] <- resp.surf(x1, x2, x3, x4, model.red)
}

d <- data.frame(data[,1:4], fitted)
d[order(-d$fitted),][1,]
# Best settings are A+, B-, C+, D-
# This corresponds to lower annual fee, no opening fee, low initial interest rate, low long-term interest rate

# Residual Analysis
par(mfrow = c(2,2))
hist(model.red$residuals, main = "Histogram of Residuals", xlab = "")
qqnorm(model.red$residuals, main = "QQ-Plot of Residuals")
qqline(model.red$residuals, col = "red")
plot(model.red$residuals, main = "Residuals vs. Order", ylab = "Residuals")
abline(h = 0, col = "red")
plot(model.red$fitted, model.red$residuals, main = "Residuals vs. Fitted Values", ylab = "Residuals", xlab = "Fitted Values")
abline(h = 0, col = "red")

