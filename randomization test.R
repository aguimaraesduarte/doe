setwd(dir = "/Users/ntstevens/Dropbox/Teaching/MSAN_631/2017/Lectures/")

data <- read.csv(file = "tripeaks.csv", header = T)
A <- data$A
B <- data$B
C <- data$C
n <- length(A)
par(mfrow=c(3,1))
xmin <- min(min(A), min(B), min(C))
xmax <- max(max(A), max(B), max(C))
hist(A, xlim = c(xmin, xmax), main = "Dollars Spent on In-app Purchases", xlab = "Variant A")
abline(v = mean(A), col = "red", lwd = 2)
hist(B, xlim = c(xmin, xmax), main = "", xlab = "Variant B")
abline(v = mean(B), col = "red", lwd = 2)
hist(C, xlim = c(xmin, xmax), main = "", xlab = "Variant C")
abline(v = mean(C), col = "red", lwd = 2)


## Compare Variants A and B
comb <- c(A,B)
t.dist <- rep(0, n)
Nsim <- 1000
for(i in 1:Nsim){
  resamp <- sample(comb, size = length(comb), replace = FALSE)
  t.dist[i] <- mean(resamp[1:n]) - mean(resamp[(n+1):(2*n)])
}
par(mfrow = c(1,1))
hist(t.dist, main = "Sampling Distribution of Test Statistic", xlab = "t")
t <- mean(A)-mean(B)
abline(v = t, col = "red", lwd = 2)
p.value = sum(t.dist <= t) / Nsim
p.value

## Compare Variants A and C
comb <- c(A,C)
t.dist <- rep(0, n)
Nsim <- 1000
for(i in 1:Nsim){
  resamp <- sample(comb, size = length(comb), replace = FALSE)
  t.dist[i] <- mean(resamp[1:n]) - mean(resamp[(n+1):(2*n)])
}
par(mfrow = c(1,1))
hist(t.dist, main = "Sampling Distribution of Test Statistic", xlab = "t")
t <- mean(A)-mean(C)
abline(v = t, col = "red", lwd = 2)
p.value = sum(t.dist <= t) / Nsim
p.value
