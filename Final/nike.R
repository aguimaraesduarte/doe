setwd(dir = "~/Desktop/design-of-experiments/Final")

data <- read.table(file = "nike.txt", header = T, sep = " ")
NE <- subset(data, Region == "NE")
MW <- subset(data, Region == "MW")
S <- subset(data, Region == "S")
W <- subset(data, Region == "W")

NE.1 <- subset(NE, Ad == 1)$Y
NE.2 <- subset(NE, Ad == 2)$Y
NE.3 <- subset(NE, Ad == 3)$Y
NE.4 <- subset(NE, Ad == 4)$Y
NE.5 <- subset(NE, Ad == 5)$Y

MW.1 <- subset(MW, Ad == 1)$Y
MW.2 <- subset(MW, Ad == 2)$Y
MW.3 <- subset(MW, Ad == 3)$Y
MW.4 <- subset(MW, Ad == 4)$Y
MW.5 <- subset(MW, Ad == 5)$Y

S.1 <- subset(S, Ad == 1)$Y
S.2 <- subset(S, Ad == 2)$Y
S.3 <- subset(S, Ad == 3)$Y
S.4 <- subset(S, Ad == 4)$Y
S.5 <- subset(S, Ad == 5)$Y

W.1 <- subset(W, Ad == 1)$Y
W.2 <- subset(W, Ad == 2)$Y
W.3 <- subset(W, Ad == 3)$Y
W.4 <- subset(W, Ad == 4)$Y
W.5 <- subset(W, Ad == 5)$Y

prop.test(c(sum(NE.1), sum(NE.2), sum(NE.3), sum(NE.4), sum(NE.5)),
          rep(5000, 5),
          alternative = "two.sided")

# NE.1 > NE.2 Accept
prop.test(c(sum(NE.1), sum(NE.2)), rep(5000, 2), alternative = "less")
# NE.1 > NE.3 Accept
prop.test(c(sum(NE.1), sum(NE.3)), rep(5000, 2), alternative = "less")
# NE.1 > NE.4 Accept
prop.test(c(sum(NE.1), sum(NE.4)), rep(5000, 2), alternative = "less")
# NE.1 > NE.5 Accept
prop.test(c(sum(NE.1), sum(NE.5)), rep(5000, 2), alternative = "less")
# NE.2 > NE.3 Accept
prop.test(c(sum(NE.2), sum(NE.3)), rep(5000, 2), alternative = "less")
# NE.2 > NE.4 Reject
prop.test(c(sum(NE.2), sum(NE.4)), rep(5000, 2), alternative = "less")
# NE.2 > NE.5 Accept
prop.test(c(sum(NE.2), sum(NE.5)), rep(5000, 2), alternative = "less")
# NE.3 > NE.4 Reject
prop.test(c(sum(NE.3), sum(NE.4)), rep(5000, 2), alternative = "less")
# NE.3 > NE.5 Reject
prop.test(c(sum(NE.3), sum(NE.5)), rep(5000, 2), alternative = "less")
# NE.4 > NE.5 Accept
prop.test(c(sum(NE.4), sum(NE.5)), rep(5000, 2), alternative = "less")
# Final order: 1 > 4 > 2 > 5 > 3


prop.test(c(sum(MW.1), sum(MW.2), sum(MW.3), sum(MW.4), sum(MW.5)),
          rep(5000, 5),
          alternative = "two.sided")

# MW.1 > MW.2 Reject
prop.test(c(sum(MW.1), sum(MW.2)), rep(5000, 2), alternative = "less")
# MW.1 > MW.3 Accept
prop.test(c(sum(MW.1), sum(MW.3)), rep(5000, 2), alternative = "less")
# MW.1 > MW.4 Accept
prop.test(c(sum(MW.1), sum(MW.4)), rep(5000, 2), alternative = "less")
# MW.1 > MW.5 Accept
prop.test(c(sum(MW.1), sum(MW.5)), rep(5000, 2), alternative = "less")
# MW.2 > MW.3 Accept
prop.test(c(sum(MW.2), sum(MW.3)), rep(5000, 2), alternative = "less")
# MW.2 > MW.4 Accept
prop.test(c(sum(MW.2), sum(MW.4)), rep(5000, 2), alternative = "less")
# MW.2 > MW.5 Accept
prop.test(c(sum(MW.2), sum(MW.5)), rep(5000, 2), alternative = "less")
# MW.3 > MW.4 Reject
prop.test(c(sum(MW.3), sum(MW.4)), rep(5000, 2), alternative = "less")
# MW.3 > MW.5 Reject
prop.test(c(sum(MW.3), sum(MW.5)), rep(5000, 2), alternative = "less")
# MW.4 > MW.5 Accept
prop.test(c(sum(MW.4), sum(MW.5)), rep(5000, 2), alternative = "less")
# Final order: 2 > 1 > 4 > 5 > 3


prop.test(c(sum(S.1), sum(S.2), sum(S.3), sum(S.4), sum(S.5)),
          rep(5000, 5),
          alternative = "two.sided")

# S.1 > S.2 Reject
prop.test(c(sum(S.1), sum(S.2)), rep(5000, 2), alternative = "less")
# S.1 > S.3 Accept
prop.test(c(sum(S.1), sum(S.3)), rep(5000, 2), alternative = "less")
# S.1 > S.4 Reject
prop.test(c(sum(S.1), sum(S.4)), rep(5000, 2), alternative = "less")
# S.1 > S.5 Reject
prop.test(c(sum(S.1), sum(S.5)), rep(5000, 2), alternative = "less")
# S.2 > S.3 Accept
prop.test(c(sum(S.2), sum(S.3)), rep(5000, 2), alternative = "less")
# S.2 > S.4 Accept
prop.test(c(sum(S.2), sum(S.4)), rep(5000, 2), alternative = "less")
# S.2 > S.5 Reject
prop.test(c(sum(S.2), sum(S.5)), rep(5000, 2), alternative = "less")
# S.3 > S.4 Reject
prop.test(c(sum(S.3), sum(S.4)), rep(5000, 2), alternative = "less")
# S.3 > S.5 Reject
prop.test(c(sum(S.3), sum(S.5)), rep(5000, 2), alternative = "less")
# S.4 > S.5 Reject
prop.test(c(sum(S.4), sum(S.5)), rep(5000, 2), alternative = "less")
# Final order: 5 > 4 > 2 > 1 > 3


prop.test(c(sum(W.1), sum(W.2), sum(W.3), sum(W.4), sum(W.5)),
          rep(5000, 5),
          alternative = "two.sided")

# W.1 > W.2 Accept
prop.test(c(sum(W.1), sum(W.2)), rep(5000, 2), alternative = "less")
# W.1 > W.3 Accept
prop.test(c(sum(W.1), sum(W.3)), rep(5000, 2), alternative = "less")
# W.1 > W.4 Accept
prop.test(c(sum(W.1), sum(W.4)), rep(5000, 2), alternative = "less")
# W.1 > W.5 Accept
prop.test(c(sum(W.1), sum(W.5)), rep(5000, 2), alternative = "less")
# W.2 > W.3 Accept
prop.test(c(sum(W.2), sum(W.3)), rep(5000, 2), alternative = "less")
# W.2 > W.4 Reject
prop.test(c(sum(W.2), sum(W.4)), rep(5000, 2), alternative = "less")
# W.2 > W.5 Accept
prop.test(c(sum(W.2), sum(W.5)), rep(5000, 2), alternative = "less")
# W.3 > W.4 Reject
prop.test(c(sum(W.3), sum(W.4)), rep(5000, 2), alternative = "less")
# W.3 > W.5 Reject
prop.test(c(sum(W.3), sum(W.5)), rep(5000, 2), alternative = "less")
# W.4 > W.5 Accept
prop.test(c(sum(W.4), sum(W.5)), rep(5000, 2), alternative = "less")
# Final order: 4 > 1 > 2 > 5 > 3

# All regions combined
prop.test(table(data$Y, data$Ad)[2,], rep(20000, 5))