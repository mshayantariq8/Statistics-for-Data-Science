# Load necessary libraries
library(titanic)
library(boot)  # for bootstrap functions

# Load the Titanic dataset
data("titanic_train")

# Extract the 'Age' column and remove NA values
ages <- na.omit(titanic_train$Age)

# Define Huber's Psi function and its derivative
psi <- function(x, K) {
  return(pmax(-K, pmin(K, x)))
}

psi_prime <- function(x, K) {
  return(ifelse(abs(x) <= K, 1, 0))
}

# MADN (Median Absolute Deviation Normalized)
madn <- function(x) {
  return(median(abs(x - median(x))) / 0.6745)
}

# Function to compute the M-estimator
m_estimator <- function(data, K = 1.28, tol = 0.0001) {
  n <- length(data)
  mu <- median(data)
  madn_value <- madn(data)
  
  repeat {
    A <- sum(psi((data - mu) / madn_value, K))
    B <- sum(psi_prime((data - mu) / madn_value, K))
    mu_new <- mu + (madn_value * A) / B
    
    if (abs(mu_new - mu) < tol) {
      break
    }
    
    mu <- mu_new
  }
  
  return(mu_new)
}

# Compute the M-estimator
m_estimate <- m_estimator(ages)

# Print the result
cat("M-Estimator of Location:", m_estimate, "\n")

# Estimating the standard error using influence function
se_m_estimator <- function(data, mu, madn_value, K) {
  n <- length(data)
  phi_values <- psi((data - mu) / madn_value, K)
  var_phi <- var(phi_values)
  se <- sqrt(var_phi / n)
  return(se)
}

# Calculate standard error of the M-estimator
standard_error <- se_m_estimator(ages, m_estimate, madn(ages), 1.28)
cat("Standard Error of the M-Estimator:", standard_error, "\n")

# Bootstrap method for error estimation
bootstrap_error <- function(data, nboot = 1000) {
  boot_obj <- boot(data, statistic = function(data, indices) {
    m_estimator(data[indices], 1.28)
  }, R = nboot)
  return(sd(boot_obj$t))
}

# Calculate bootstrap error
bootstrap_se <- bootstrap_error(ages)
cat("Bootstrap Standard Error of the M-Estimator:", bootstrap_se, "\n")

# Plotting the kernel density estimate with the M-estimator
plot(density(ages), main = "Kernel Density Estimate with M-Estimator")
abline(v = m_estimate, col = "red", lwd = 2)
abline(v = m_estimate + standard_error, col = "blue", lty = 2)
abline(v = m_estimate - standard_error, col = "blue", lty = 2)
abline(v = m_estimate + bootstrap_se, col = "green", lty = 2)
abline(v = m_estimate - bootstrap_se, col = "green", lty = 2)
legend("topright", legend = c("M-Estimator", "SE", "Bootstrap SE"), col = c("red", "blue", "green"), lty = c(1, 2, 2), lwd = 2)

