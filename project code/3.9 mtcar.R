# Load necessary libraries
library(boot)

# Load the mtcars dataset
data("mtcars")

# Extract the mpg variable
mpg_data <- mtcars$mpg

# Function to compute the Hodges-Lehmann Estimator
hodges_lehmann <- function(x) {
  n <- length(x)
  pairwise_averages <- numeric(n * (n - 1) / 2 + n)
  k <- 1
  for (i in 1:n) {
    for (j in i:n) {
      pairwise_averages[k] <- (x[i] + x[j]) / 2
      k <- k + 1
    }
  }
  return(median(pairwise_averages))
}

# Compute the Hodges-Lehmann Estimator for the mpg data
hl_est_mpg <- hodges_lehmann(mpg_data)
print(hl_est_mpg)

# Bootstrap function for Hodges-Lehmann Estimator
bootstrap_hl <- function(data, indices) {
  sample_data <- data[indices]
  return(hodges_lehmann(sample_data))
}

# Compute bootstrap estimate for standard error of the Hodges-Lehmann Estimator
set.seed(123)
bootstrap_results_hl <- boot(mpg_data, bootstrap_hl, R = 1000)
print(bootstrap_results_hl)

# Standard error of the Hodges-Lehmann Estimator
se_hl_est <- sd(bootstrap_results_hl$t)
print(se_hl_est)

# Plotting the data and the Hodges-Lehmann estimator
hist(mpg_data, breaks = 10, main = "Histogram of MPG with Hodges-Lehmann Estimator", xlab = "MPG", col = "lightblue", border = "black")
abline(v = hl_est_mpg, col = "red", lwd = 2, lty = 2)
legend("topright", legend = paste("Hodges-Lehmann Estimator:", round(hl_est_mpg, 2)), col = "red", lwd = 2, lty = 2)
