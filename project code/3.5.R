# Load necessary libraries
library(titanic)

# Load the Titanic dataset
data("titanic_train")

# Extract the 'Age' column and remove NA values
ages <- na.omit(titanic_train$Age)

### 3.5.1 Estimating Quantiles
# Function to estimate the quantile
estimate_quantile <- function(data, q) {
  n <- length(data)
  m <- floor(q * n + 0.5)
  sorted_data <- sort(data)
  quantile_estimate <- sorted_data[m]
  return(quantile_estimate)
}

# Define the quantile to estimate
q <- 0.5  # Median

# Estimate the quantile
quantile_estimate <- estimate_quantile(ages, q)

# Print the result
cat("Quantile (q =", q, "):", quantile_estimate, "\n")

### 3.5.3 The Maritz-Jarrett Estimate of the Standard Error of \( \hat{x}_q \)
# Function to estimate the standard error using Maritz-Jarrett method
maritz_jarrett_se <- function(data, q) {
  n <- length(data)
  m <- floor(q * n + 0.5)
  sorted_data <- sort(data)
  
  a <- m - 1
  b <- n - m
  
  Wi <- function(i, n, m) {
    pbeta(i / n, a, b) - pbeta((i - 1) / n, a, b)
  }
  
  Ck <- function(k) {
    sum(sapply(1:n, function(i) Wi(i, n, m) * sorted_data[i]^k))
  }
  
  C1 <- Ck(1)
  C2 <- Ck(2)
  
  se <- sqrt(C2 - C1^2)
  return(se)
}

# Estimate the standard error
mj_se <- maritz_jarrett_se(ages, q)

# Print the result
cat("Maritz-Jarrett Standard Error (q =", q, "):", mj_se, "\n")

### 3.5.5 The Harrell-Davis Estimate of the Quantile
# Function to compute Harrell-Davis quantile estimate
harrell_davis_quantile <- function(data, q) {
  n <- length(data)
  sorted_data <- sort(data)
  weights <- sapply(1:n, function(i) dbeta((i - 0.5) / n, q * n, (1 - q) * n))
  hd_quantile <- sum(weights * sorted_data) / sum(weights)
  return(hd_quantile)
}

# Estimate the quantile
hd_quantile <- harrell_davis_quantile(ages, q)

# Print the result
cat("Harrell-Davis Quantile (q =", q, "):", hd_quantile, "\n")

### 3.5.7 A Bootstrap Estimate of the Standard Error of \( \hat{\theta}_q \)
# Function to compute bootstrap estimate of standard error for Harrell-Davis quantile
bootstrap_hd_se <- function(data, q, nboot = 1000) {
  n <- length(data)
  hd_quantiles <- numeric(nboot)
  
  for (i in 1:nboot) {
    resample <- sample(data, n, replace = TRUE)
    hd_quantiles[i] <- harrell_davis_quantile(resample, q)
  }
  
  se <- sd(hd_quantiles)
  return(se)
}

# Estimate the bootstrap standard error
bootstrap_se <- bootstrap_hd_se(ages, q)

# Print the result
cat("Bootstrap Standard Error (q =", q, "):", bootstrap_se, "\n")

# Plotting the kernel density estimate with the quantiles and standard errors
plot(density(ages), main = paste("Kernel Density Estimate with Quantiles (q =", q, ")"))
abline(v = quantile_estimate, col = "blue", lwd = 2)
abline(v = hd_quantile, col = "green", lwd = 2)
abline(v = quantile_estimate + mj_se, col = "blue", lty = 2)
abline(v = quantile_estimate - mj_se, col = "blue", lty = 2)
abline(v = hd_quantile + bootstrap_se, col = "green", lty = 2)
abline(v = hd_quantile - bootstrap_se, col = "green", lty = 2)

legend("topright", legend = c(
  paste("Sample Quantile =", round(quantile_estimate, 2)),
  paste("Harrell-Davis Quantile =", round(hd_quantile, 2)),
  paste("Maritz-Jarrett SE =", round(mj_se, 2)),
  paste("Bootstrap SE =", round(bootstrap_se, 2))
), col = c("blue", "green", "blue", "green"), lwd = 2, lty = c(1, 1, 2, 2))
