# Load necessary libraries
library(MASS)
library(boot)
library(ggplot2)

# Generate sample data
set.seed(123)
n <- 100  # sample size
normal_data <- rnorm(n)
non_normal_data <- rt(n, df = 2)  # t-distribution with 2 degrees of freedom

# Define location estimators

# Function to compute the trimmed mean
trimmed_mean <- function(x, trim = 0.2) {
  return(mean(x, trim = trim))
}

# Function to compute the one-step M-estimator
one_step_m_estimator <- function(x, bend = 1.28) {
  med <- median(x)
  madn <- mad(x, constant = 1 / 0.6745)
  w <- pmin(1, bend / abs((x - med) / madn))
  m_est <- sum(w * x) / sum(w)
  return(m_est)
}

# Function to compute Huber's M-estimator using the huber function from MASS
huber_m_estimator <- function(x, k = 1.5) {
  huber_result <- huber(x, k = k)
  return(huber_result$mu)
}

# Function to compute the MOM estimator
mom <- function(x, bend = 2.24) {
  med <- median(x)
  madn <- mad(x, constant = 1 / 0.6745)
  outliers <- abs(x - med) / madn > bend
  return(mean(x[!outliers]))
}

# Bootstrap functions for the estimators
bootstrap_trimmed_mean <- function(data, indices) {
  sample_data <- data[indices]
  return(trimmed_mean(sample_data))
}

bootstrap_one_step_m <- function(data, indices) {
  sample_data <- data[indices]
  return(one_step_m_estimator(sample_data))
}

bootstrap_huber_m <- function(data, indices) {
  sample_data <- data[indices]
  return(huber_m_estimator(sample_data))
}

bootstrap_mom <- function(data, indices) {
  sample_data <- data[indices]
  return(mom(sample_data))
}

# Compute bootstrap estimates for standard errors
set.seed(123)
bootstrap_results_normal <- data.frame(
  estimator = c("Trimmed Mean", "One-Step M", "Huber's M", "MOM"),
  SE = c(
    sd(boot(normal_data, bootstrap_trimmed_mean, R = 1000)$t),
    sd(boot(normal_data, bootstrap_one_step_m, R = 1000)$t),
    sd(boot(normal_data, bootstrap_huber_m, R = 1000)$t),
    sd(boot(normal_data, bootstrap_mom, R = 1000)$t)
  )
)

bootstrap_results_non_normal <- data.frame(
  estimator = c("Trimmed Mean", "One-Step M", "Huber's M", "MOM"),
  SE = c(
    sd(boot(non_normal_data, bootstrap_trimmed_mean, R = 1000)$t),
    sd(boot(non_normal_data, bootstrap_one_step_m, R = 1000)$t),
    sd(boot(non_normal_data, bootstrap_huber_m, R = 1000)$t),
    sd(boot(non_normal_data, bootstrap_mom, R = 1000)$t)
  )
)

# Print standard errors
cat("Standard Errors for Normal Data:\n")
print(bootstrap_results_normal)
cat("\nStandard Errors for Non-Normal Data:\n")
print(bootstrap_results_non_normal)

# Plotting the results
bootstrap_results_normal$distribution <- "Normal"
bootstrap_results_non_normal$distribution <- "Non-Normal"
bootstrap_results <- rbind(bootstrap_results_normal, bootstrap_results_non_normal)

ggplot(bootstrap_results, aes(x = estimator, y = SE, fill = distribution)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Standard Errors of Location Estimators",
       x = "Estimator",
       y = "Standard Error") +
  theme_minimal()
