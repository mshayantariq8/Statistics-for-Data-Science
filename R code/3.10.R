# Load necessary libraries

library(titanic)
library(boot)

# Install and load ggplot2 if not already installed
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Load the Titanic dataset
data("titanic_train")

# Extract the age variable, removing NA values
ages <- na.omit(titanic_train$Age)

# Function to compute the MOM estimator
mom <- function(x, bend = 2.24) {
  med <- median(x)
  madn <- mad(x, constant = 1 / 0.6745)
  outliers <- abs(x - med) / madn > bend
  return(mean(x[!outliers]))
}

# Define the R function mom_est
mom_est <- function(x, bend = 2.24) {
  mom <- function(x, bend = 2.24) {
    med <- median(x)
    madn <- mad(x, constant = 1 / 0.6745)
    outliers <- abs(x - med) / madn > bend
    return(mean(x[!outliers]))
  }
  
  mom_est <- mom(x, bend)
  return(mom_est)
}

# Function to compute the skipped estimator using boxplot rule
bmean <- function(x, mbox = TRUE) {
  if (mbox) {
    # Using Carling's method
    Q1 <- quantile(x, 0.25)
    Q3 <- quantile(x, 0.75)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
  } else {
    # Using the boxplot rule based on ideal fourths
    n <- length(x)
    ideal_fourths <- sort(x)[c(ceiling(n / 4), floor(3 * n / 4))]
    IQR <- diff(ideal_fourths)
    lower_bound <- ideal_fourths[1] - 1.5 * IQR
    upper_bound <- ideal_fourths[2] + 1.5 * IQR
  }
  
  outliers <- x < lower_bound | x > upper_bound
  return(mean(x[!outliers]))
}

# Define the R function bmean_est
bmean_est <- function(x, mbox = TRUE) {
  bmean <- function(x, mbox = TRUE) {
    if (mbox) {
      Q1 <- quantile(x, 0.25)
      Q3 <- quantile(x, 0.75)
      IQR <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
    } else {
      n <- length(x)
      ideal_fourths <- sort(x)[c(ceiling(n / 4), floor(3 * n / 4))]
      IQR <- diff(ideal_fourths)
      lower_bound <- ideal_fourths[1] - 1.5 * IQR
      upper_bound <- ideal_fourths[2] + 1.5 * IQR
    }
    
    outliers <- x < lower_bound | x > upper_bound
    return(mean(x[!outliers]))
  }
  
  bmean_est <- bmean(x, mbox)
  return(bmean_est)
}

# Compute the MOM and bmean estimators for the age data
mom_est_age <- mom_est(ages)
bmean_est_age <- bmean_est(ages)

# Print the estimators
cat("MOM Estimator for Age:", mom_est_age, "\n")
cat("Bmean Estimator for Age:", bmean_est_age, "\n")

# Bootstrap function for MOM estimator
bootstrap_mom <- function(data, indices) {
  sample_data <- data[indices]
  return(mom_est(sample_data))
}

# Bootstrap function for Bmean estimator
bootstrap_bmean <- function(data, indices) {
  sample_data <- data[indices]
  return(bmean_est(sample_data))
}

# Compute bootstrap estimate for standard error of the MOM estimator
set.seed(123)
bootstrap_results_mom <- boot(ages, bootstrap_mom, R = 1000)

# Compute bootstrap estimate for standard error of the Bmean estimator
bootstrap_results_bmean <- boot(ages, bootstrap_bmean, R = 1000)

# Standard errors of the estimators
se_mom_est <- sd(bootstrap_results_mom$t)
se_bmean_est <- sd(bootstrap_results_bmean$t)

# Print standard errors
cat("Standard Error of MOM Estimator:", se_mom_est, "\n")
cat("Standard Error of Bmean Estimator:", se_bmean_est, "\n")

# Plotting the distribution and estimators
ggplot(data.frame(Age = ages), aes(x = Age)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mom_est_age), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = bmean_est_age), color = "blue", linetype = "dashed", size = 1) +
  geom_density(color = "black", size = 1) +
  labs(title = "Age Distribution with MOM and Bmean Estimators",
       x = "Age",
       y = "Density") +
  theme_minimal() +
  annotate("text", x = mom_est_age, y = 0.02, label = "MOM", color = "red", vjust = -1) +
  annotate("text", x = bmean_est_age, y = 0.02, label = "Bmean", color = "blue", vjust = 1)
