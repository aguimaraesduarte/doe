setwd(dir = "~/Desktop/design-of-experiments/Final")

data <- read.table(file = "candycrush.txt", header = T, sep = "\t")

grp1 <- subset(data, booster == 1)$time
grp2 <- subset(data, booster == 2)$time
grp3 <- subset(data, booster == 3)$time

# Visualize the data
par(mfrow = c(3, 1))
hist(grp1, main = "Group 1 Observations")
abline(v = mean(grp1), col = "red", lwd = 2)
hist(grp2, main = "Group 2 Observations")
abline(v = mean(grp2), col = "red", lwd = 2)
hist(grp3, main = "Group 3 Observations")
abline(v = mean(grp3), col = "red", lwd = 2)

# Test Ho: mu1 = mu2 = mu3 (we reject)
m <- lm(time ~ as.factor(booster), data = data)
summary(m)
anova(m)

k <- 3
alpha <- 0.05
alpha.bonferroni <- alpha/k

## mu1 < mu2
t.test(x = grp1,
       y = grp2,
       alternative = "less",
       mu = 0,
       paired = F,
       var.equal = T,
       conf.level = 1-alpha.bonferroni)

## mu1 < mu3
t.test(x = grp1,
       y = grp3,
       alternative = "less",
       mu = 0,
       paired = F,
       var.equal = T,
       conf.level = 1-alpha.bonferroni)

## mu2 < mu3
t.test(x = grp2,
       y = grp3,
       alternative = "less",
       mu = 0,
       paired = F,
       var.equal = T,
       conf.level = 1-alpha.bonferroni)
