# Load necessary libraries
library(titanic)
library(boot)

# Load the Titanic dataset
data("titanic_train")

# Extract the age variable, removing NA values
ages <- na.omit(titanic_train$Age)

# Function to compute the One-Step M-estimator
one_step_m_estimator <- function(x, bend = 1.28) {
  med <- median(x)
  madn <- mad(x, constant = 1 / 0.6745)
  w <- pmin(1, bend / abs((x - med) / madn))
  m_est <- sum(w * x) / sum(w)
  return(m_est)
}

# Define the R function onestep
onestep <- function(x, bend = 1.28) {
  # Function to compute the One-Step M-estimator
  one_step_m_estimator <- function(x, bend = 1.28) {
    med <- median(x)
    madn <- mad(x, constant = 1 / 0.6745)
    w <- pmin(1, bend / abs((x - med) / madn))
    m_est <- sum(w * x) / sum(w)
    return(m_est)
  }
  
  m_est <- one_step_m_estimator(x, bend)
  return(m_est)
}

# Compute the One-Step M-estimator for the age data
m_est_age <- onestep(ages)
print(m_est_age)

# Bootstrap function for One-Step M-estimator
bootstrap_one_step_m <- function(data, indices) {
  sample_data <- data[indices]
  return(onestep(sample_data))
}

# Compute bootstrap estimate for standard error of the One-Step M-estimator
set.seed(123)
bootstrap_results <- boot(ages, bootstrap_one_step_m, R = 1000)
print(bootstrap_results)

# Standard error of the One-Step M-estimator
se_m_est <- sd(bootstrap_results$t)
print(se_m_est)

# Plotting the data and the One-Step M-estimator
hist(ages, breaks = 30, main = "Histogram of Ages with One-Step M-estimator", xlab = "Age", col = "lightblue", border = "black")
abline(v = m_est_age, col = "red", lwd = 2, lty = 2)
legend("topright", legend = paste("One-Step M-estimator:", round(m_est_age, 2)), col = "red", lwd = 2, lty = 2)
