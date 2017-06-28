# Generate some data
set.seed(10)
grp1 <- rnorm(n = 100, mean = 0, sd = 10)
grp2 <- rnorm(n = 100, mean = 10, sd = 10)
grp3 <- rnorm(n = 100, mean = 50, sd = 10)
grp4 <- rnorm(n = 100, mean = 10, sd = 10)

# Visualize the data
par(mfrow = c(4,1))
hist(grp1, xlim = c(-50, 100), main = "Group 1 Observations")
abline(v = mean(grp1), col = "red", lwd = 2)
hist(grp2, xlim = c(-50, 100), main = "Group 2 Observations")
abline(v = mean(grp2), col = "red", lwd = 2)
hist(grp3, xlim = c(-50, 100), main = "Group 3 Observations")
abline(v = mean(grp3), col = "red", lwd = 2)
hist(grp4, xlim = c(-50, 100), main = "Group 4 Observations")
abline(v = mean(grp4), col = "red", lwd = 2)

# Organize the data
d <- data.frame(y = c(grp1, grp2, grp3, grp4), group = c(rep(1,100), rep(2,100), rep(3,100), rep(4,100)))

# Test Ho: mu1 = mu2 = mu3 = mu4 (we reject)
m <- lm(y ~ as.factor(group), data = d)
summary(m)
anova(m)



