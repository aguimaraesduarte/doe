####### Visualization of the multiple comparison problem
# This function calculates the experimentwise Type I Error rate in the context of k comparisons each with significance level alpha
err <- function(k, alpha){
  return(1 - (1-alpha)^k)
}
 
# Plot of Error rate vs. k
alpha <- 0.01
plot(x = seq(1, 100, 1), y = err(k = seq(1, 100, 1), alpha = alpha), type = "l", xlab = "Number of Pairwise Comparisons (k)", ylab = "Type I Error Rate", main = as.expression(bquote(alpha == .(alpha[1]))))
abline(h = 0.05, col = "red", lty = 2)


####### Visualization of Bonferroni Correction
plot(x = seq(0, 1, 0.001), y = 1 - exp(-seq(0, 1, 0.001)), type = "l", xlab = expression(alpha), ylab = "Error Rate", main = "Bonferroni Correction")
abline(a=0, b = 1, col = "red", lty = 2)
legend("bottomright", legend = c("Asym. Error Rate", "Line of Equality"), lty = c(1,2), col = c("black", "red")) 
