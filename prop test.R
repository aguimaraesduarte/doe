setwd(dir = "/Users/ntstevens/Dropbox/Teaching/MSAN_631/2017/Lectures/")

data <- read.csv(file = "buttoncolor.csv", header = T)
A <- data$A
B <- data$B
n1 <- length(A)
n2 <- length(B)

par(mfrow=c(2,1))
xmin <- min(min(A), min(B))
xmax <- max(max(A), max(B))
hist(A, xlim = c(xmin, xmax), main = "Conversion", xlab = "Variant A")
abline(v = mean(A), col = "red", lwd = 2)
hist(B, xlim = c(xmin, xmax), main = "", xlab = "Variant B")
abline(v = mean(B), col = "red", lwd = 2)

## Two-sided test Ha: piA != piB
prop.test(x = c(sum(A), sum(B)), n = c(n1, n2), alternative = "two.sided", conf.level = 0.95)

## One-sided test Ha: piA > piB
prop.test(x = c(sum(A), sum(B)), n = c(n1, n2), alternative = "greater", conf.level = 0.95)

## One-sided test Ha: piA < piB
prop.test(x = c(sum(A), sum(B)), n = c(n1, n2), alternative = "less", conf.level = 0.95)
