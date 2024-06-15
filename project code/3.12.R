# Load necessary libraries
library(MASS)
library(boot)
library(ggplot2)
library(titanic)

# Load the Titanic dataset
data("titanic_train")

# Extract the age variable, removing NA values
ages <- na.omit(titanic_train$Age)

# Define the Biweight Midvariance function
bivar <- function(x) {
  n <- length(x)
  M <- median(x)
  MAD <- mad(x)
  K <- 9
  Y <- (x - M) / (K * MAD)
  a <- ifelse(abs(Y) < 1, 1, 0)
  numerator <- sum(a * (x - M)^2 * (1 - Y^2)^4)
  denominator <- (sum(a * (1 - Y^2) * (1 - 5 * Y^2)))^2
  biweight_midvariance <- sqrt(n * numerator / denominator)
  return(biweight_midvariance)
}

# Define the Percentage Bend Midvariance function
pbvar <- function(x, beta = 0.2) {
  n <- length(x)
  M <- median(x)
  MAD <- mad(x)
  omega_beta <- qnorm(1 - beta / 2)
  Y <- (x - M) / (omega_beta * MAD)
  a <- ifelse(abs(Y) < 1, 1, 0)
  numerator <- sum(a * (x - M)^2 * (1 - Y^2)^4)
  denominator <- (sum(a * (1 - Y^2) * (1 - 5 * Y^2)))^2
  percentage_bend_midvariance <- sqrt(n * numerator / denominator)
  return(percentage_bend_midvariance)
}

# Define the Tau Measure of Variation function
tauvar <- function(x) {
  M <- median(x)
  MAD <- mad(x)
  c <- 3
  rho_c <- function(u) pmin(u^2, c^2)
  Y <- (x - M) / MAD
  tau_variation <- MAD^2 * mean(rho_c(Y))
  return(tau_variation)
}

# Define the Interquartile Range (IQR) function
idealf <- function(x) {
  return(IQR(x))
}

# Calculate the measures
biweight_mv <- bivar(ages)
percentage_bend_mv <- pbvar(ages)
tau_variation <- tauvar(ages)
iqr_age <- idealf(ages)

# Print the results
cat("Biweight Midvariance: ", biweight_mv, "\n")
cat("Percentage Bend Midvariance: ", percentage_bend_mv, "\n")
cat("Tau Measure of Variation: ", tau_variation, "\n")
cat("Interquartile Range: ", iqr_age, "\n")

# Combine results into a data frame for plotting
results <- data.frame(
  Measure = c("Biweight Midvariance", "Percentage Bend Midvariance", "Tau Measure of Variation", "Interquartile Range"),
  Value = c(biweight_mv, percentage_bend_mv, tau_variation, iqr_age)
)

# Plotting the results
ggplot(results, aes(x = Measure, y = Value, fill = Measure)) +
  geom_bar(stat = "identity") +
  labs(title = "Measures of Scale for Titanic Age Data",
       x = "Measure",
       y = "Value") +
  theme_minimal()
