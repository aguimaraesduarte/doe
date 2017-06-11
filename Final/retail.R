setwd(dir = "~/Desktop/design-of-experiments/Final")

data <- read.table(file = "retail.txt", header = T, sep = " ")
A <- data$A
B <- data$B

par(mfrow=c(2,1))
xmin <- min(min(A), min(B))
xmax <- max(max(A), max(B))
hist(A, xlim = c(xmin, xmax), main = "Purchase prices", xlab = "Variant A")
abline(v = mean(A), col = "red", lwd = 2)
hist(B, xlim = c(xmin, xmax), main = "", xlab = "Variant B")
abline(v = mean(B), col = "red", lwd = 2)

# Do standard deviations seem similar?
sd(A)
sd(B)

## One-sided test Ha: muA > muB
t.test(x = A, y = B, alternative = "greater", mu = 0, paired = F, var.equal = T, conf.level = 0.95)


