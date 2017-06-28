# Generate some data
set.seed(10)
grp1 <- rbinom(887, 1, 0.5)
grp2 <- rbinom(1000, 1, 0.5)
grp3 <- rbinom(1003, 1, 0.5)

# Visualize the data
table(grp1)
table(grp2)
table(grp3)

# Test Ho: pi1 = pi2 = pi3 (we do not reject)
prop.test(c(sum(grp1), sum(grp2), sum(grp3)), c(887, 1000, 1003), alternative = "two.sided")

