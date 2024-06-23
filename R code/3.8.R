# Load necessary libraries
library(titanic)
library(boot)

# Load the Titanic dataset
data("titanic_train")

# Extract the age variable, removing NA values
ages <- na.omit(titanic_train$Age)

# 3.8 W-estimators
# Function to compute W-estimators (using a general robust method)
w_estimator <- function(x) {
  med <- median(x)
  madn <- mad(x, constant = 1 / 0.6745)
  weights <- pmin(1, 1.28 / abs((x - med) / madn))
  w_est <- sum(weights * x) / sum(weights)
  return(w_est)
}

# Compute W-estimator for the age data
w_est_age <- w_estimator(ages)
print(w_est_age)

# 3.8.1 Tau Measure of Location
# Function to compute the Tau Measure of Location
tau_location <- function(x) {
  med <- median(x)
  madn <- mad(x, constant = 1 / 0.6745)
  weights <- 1 / (1 + ((x - med) / madn)^2)
  tau_loc <- sum(weights * x) / sum(weights)
  return(tau_loc)
}

# Compute the Tau Measure of Location for the age data
tau_loc_age <- tau_location(ages)
print(tau_loc_age)

# 3.8.2 R Function tauloc
# Define the R function tauloc
tauloc <- function(x) {
  med <- median(x)
  madn <- mad(x, constant = 1 / 0.6745)
  weights <- 1 / (1 + ((x - med) / madn)^2)
  tau_loc <- sum(weights * x) / sum(weights)
  return(tau_loc)
}

# Test the tauloc function with the age data
tau_loc_age <- tauloc(ages)
print(tau_loc_age)

# 3.8.3 Zuo’s Weighted Estimator
# Function to compute Zuo’s Weighted Estimator
zuo_weighted_estimator <- function(x, c = 1.28) {
  med <- median(x)
  madn <- mad(x, constant = 1 / 0.6745)
  weights <- exp(-((x - med) / (c * madn))^2)
  zuo_loc <- sum(weights * x) / sum(weights)
  return(zuo_loc)
}

# Compute Zuo’s Weighted Estimator for the age data
zuo_loc_age <- zuo_weighted_estimator(ages)
print(zuo_loc_age)

# Bootstrap to Estimate the Standard Error for Tau and Zuo’s Estimators

# Bootstrap function for Tau Measure of Location
bootstrap_tau_loc <- function(data, indices) {
  sample_data <- data[indices]
  return(tauloc(sample_data))
}

# Compute bootstrap estimate for standard error of the Tau Measure of Location
set.seed(123)
bootstrap_results_tau <- boot(ages, bootstrap_tau_loc, R = 1000)
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
bootstrap_results_zuo <- boot(ages, bootstrap_zuo_loc, R = 1000)
print(bootstrap_results_zuo)

# Standard error of Zuo’s Weighted Estimator
se_zuo_loc <- sd(bootstrap_results_zuo$t)
print(se_zuo_loc)

# Plotting the Data and Robust Estimators
hist(ages, breaks = 30, main = "Histogram of Ages with Robust Estimators", xlab = "Age", col = "lightblue", border = "black")
abline(v = w_est_age, col = "green", lwd = 2, lty = 2)
abline(v = tau_loc_age, col = "red", lwd = 2, lty = 2)
abline(v = zuo_loc_age, col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c(paste("W-estimator:", round(w_est_age, 2)), paste("Tau Measure:", round(tau_loc_age, 2)), paste("Zuo’s Estimator:", round(zuo_loc_age, 2))), col = c("green", "red", "blue"), lwd = 2, lty = 2)
