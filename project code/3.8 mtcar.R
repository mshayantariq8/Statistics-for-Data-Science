# Load necessary libraries
library(boot)

# Load the mtcars dataset
data("mtcars")

# Extract the mpg variable
mpg_data <- mtcars$mpg

# Function to compute W-estimators
w_estimator <- function(x) {
  med <- median(x)
  madn <- mad(x, constant = 1 / 0.6745)
  weights <- pmin(1, 1.28 / abs((x - med) / madn))
  w_est <- sum(weights * x) / sum(weights)
  return(w_est)
}

# Compute W-estimator for the mpg data
w_est_mpg <- w_estimator(mpg_data)
print(w_est_mpg)

# Function to compute the Tau Measure of Location
tau_location <- function(x) {
  med <- median(x)
  madn <- mad(x, constant = 1 / 0.6745)
  weights <- 1 / (1 + ((x - med) / madn)^2)
  tau_loc <- sum(weights * x) / sum(weights)
  return(tau_loc)
}

# Compute the Tau Measure of Location for the mpg data
tau_loc_mpg <- tau_location(mpg_data)
print(tau_loc_mpg)

# Define the R function tauloc
tauloc <- function(x) {
  med <- median(x)
  madn <- mad(x, constant = 1 / 0.6745)
  weights <- 1 / (1 + ((x - med) / madn)^2)
  tau_loc <- sum(weights * x) / sum(weights)
  return(tau_loc)
}

# Test the tauloc function with the mpg data
tau_loc_mpg <- tauloc(mpg_data)
print(tau_loc_mpg)

# Function to compute Zuo’s Weighted Estimator
zuo_weighted_estimator <- function(x, c = 1.28) {
  med <- median(x)
  madn <- mad(x, constant = 1 / 0.6745)
  weights <- exp(-((x - med) / (c * madn))^2)
  zuo_loc <- sum(weights * x) / sum(weights)
  return(zuo_loc)
}

# Compute Zuo’s Weighted Estimator for the mpg data
zuo_loc_mpg <- zuo_weighted_estimator(mpg_data)
print(zuo_loc_mpg)

# Bootstrap function for Tau Measure of Location
bootstrap_tau_loc <- function(data, indices) {
  sample_data <- data[indices]
  return(tauloc(sample_data))
}

# Compute bootstrap estimate for standard error of the Tau Measure of Location
set.seed(123)
bootstrap_results_tau <- boot(mpg_data, bootstrap_tau_loc, R = 1000)
print(bootstrap_results_tau)

# Standard error of the Tau Measure of Location
se_tau_loc <- sd(bootstrap_results_tau$t)
print(se_tau_loc)

# Bootstrap function for Zuo’s Weighted Estimator
bootstrap_zuo_loc <- function(data, indices) {
  sample_data <- data[indices]
  return(zuo_weighted_estimator(sample_data))
}

# Compute bootstrap estimate for standard error of Zuo’s Weighted Estimator
set.seed(123)
bootstrap_results_zuo <- boot(mpg_data, bootstrap_zuo_loc, R = 1000)
print(bootstrap_results_zuo)

# Standard error of Zuo’s Weighted Estimator
se_zuo_loc <- sd(bootstrap_results_zuo$t)
print(se_zuo_loc)

# Plotting the data and the robust estimators
hist(mpg_data, breaks = 10, main = "Histogram of MPG with Robust Estimators", xlab = "MPG", col = "lightblue", border = "black")
abline(v = w_est_mpg, col = "green", lwd = 2, lty = 2)
abline(v = tau_loc_mpg, col = "red", lwd = 2, lty = 2)
abline(v = zuo_loc_mpg, col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c(paste("W-estimator:", round(w_est_mpg, 2)), paste("Tau Measure:", round(tau_loc_mpg, 2)), paste("Zuo’s Estimator:", round(zuo_loc_mpg, 2))), col = c("green", "red", "blue"), lwd = 2, lty = 2)
