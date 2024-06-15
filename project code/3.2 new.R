# Load necessary libraries
library(titanic)
library(boot)
library(MASS)  # For kde2d function
library(stats)  # For density and other statistical functions

# Load the Titanic dataset
data("titanic_train")

# Extract the 'Age' column and remove NA values
ages <- na.omit(titanic_train$Age)

# Summary statistics of the 'ages' data
summary(ages)

# Calculate the span h
A <- min(sd(ages), IQR(ages) / 1.34)
h <- 1.06 * A * length(ages)^(-1/5)

# 1. Normal Kernel Density Estimator
normal_kernel_density <- function(data, bandwidth) {
  density(data, kernel = "gaussian", bw = bandwidth)
}

# 2. Rosenblatt’s Shifted Histogram
rosenblatt_shifted_histogram <- function(data, bin_width) {
  n <- length(data)
  h <- bin_width
  density_est <- function(x) {
    A <- sum(data <= (x + h))
    B <- sum(data < (x - h))
    (A - B) / (2 * n * h)
  }
  x_vals <- seq(min(data), max(data), length.out = 512)
  y_vals <- sapply(x_vals, density_est)
  list(x = x_vals, y = y_vals)
}

# 3. Expected Frequency Curve
expected_frequency_curve <- function(data, h) {
  n <- length(data)
  data_sorted <- sort(data)
  median_abs_dev <- mad(data)
  density_estimates <- rep(0, n)
  for (i in 1:n) {
    close_points <- sum(abs(data_sorted - data_sorted[i]) <= h * median_abs_dev)
    density_estimates[i] <- close_points / (n * 2 * h * median_abs_dev)
  }
  data.frame(data_sorted, density_estimates)
}

# 4. Adaptive Kernel Density Estimator
adaptive_kernel_density <- function(data, h) {
  n <- length(data)
  initial_density <- expected_frequency_curve(data, h)
  f_tilde <- initial_density$density_estimates
  g <- exp(mean(log(f_tilde[f_tilde > 0])))  # Calculate geometric mean of positive f_tilde values
  
  lambda <- (f_tilde / g)^(-0.5)
  lambda[is.na(lambda) | is.infinite(lambda)] <- 1  # Replace invalid lambda values with 1
  
  # Define Epanechnikov kernel
  epanechnikov_kernel <- function(t) {
    if (abs(t) < sqrt(5)) {
      return(3 / 4 * (1 - 1 / 5 * t^2) / sqrt(5))
    } else {
      return(0)
    }
  }
  
  x_vals <- seq(min(data), max(data), length.out = 512)
  y_vals <- rep(0, length(x_vals))
  
  for (i in 1:length(x_vals)) {
    for (j in 1:n) {
      y_vals[i] <- y_vals[i] + epanechnikov_kernel((x_vals[i] - data[j]) / (h * lambda[j])) / (h * lambda[j])
    }
    y_vals[i] <- y_vals[i] / n
  }
  
  list(x = x_vals, y = y_vals)
}

# Plotting functions
plot_density_estimators_comparison <- function(data, bandwidth, bin_width, h) {
  # Normal Kernel Density
  normal_kde <- normal_kernel_density(data, bandwidth)
  
  # Rosenblatt's Shifted Histogram
  rosenblatt_hist <- rosenblatt_shifted_histogram(data, bin_width)
  
  # Expected Frequency Curve
  ef_curve <- expected_frequency_curve(data, h)
  
  # Adaptive Kernel Density
  adaptive_kde <- adaptive_kernel_density(data, h)
  
  # Adjust the y-axis limits to ensure all lines are visible
  plot(normal_kde, main = "Comparison of Density Estimators", xlab = "Age", ylab = "Density", col = "red", lwd = 2, ylim = c(0, 0.04))
  lines(rosenblatt_hist$x, rosenblatt_hist$y, col = "purple", lwd = 2)
  lines(ef_curve$data_sorted, ef_curve$density_estimates, col = "green", lwd = 2)
  lines(adaptive_kde$x, adaptive_kde$y, col = "blue", lwd = 2)
  
  legend("topright", legend = c("Normal Kernel", "Rosenblatt Histogram", "Expected Frequency", "Adaptive Kernel"),
         col = c("red", "purple", "green", "blue"), lwd = 2)
}

# Execute and plot comparison
plot_density_estimators_comparison(ages, bandwidth = "nrd0", bin_width = 1.2 * IQR(ages) / length(ages)^(1/5), h = 0.8)

