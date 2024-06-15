# Load necessary libraries
library(datasets)
library(robustbase)
library(ggplot2)

# Load Titanic dataset
data("Titanic", package = "datasets")
titanic_df <- as.data.frame(Titanic)

# Assume we have the Titanic dataset with 'Age' column (using another Titanic dataset for demonstration)
# You can replace this with the actual Titanic dataset you have
# Example: ages <- na.omit(as.numeric(titanic_df$Age))

# For demonstration, use random normal data (replace this with actual Titanic 'Age' data)
set.seed(123)
ages <- rnorm(100, mean = 30, sd = 10)  # Example data

# Functions for outlier detection

# Function to detect outliers based on mean and variance
detect_outliers_mean_var <- function(x, k = 2.24) {
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  outliers <- abs(x - mean_x) / sd_x > k
  return(outliers)
}

# Function to detect outliers based on IQR
detect_outliers_iqr <- function(x, k = 1.5) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_bound <- q1 - k * iqr
  upper_bound <- q3 + k * iqr
  outliers <- x < lower_bound | x > upper_bound
  return(outliers)
}

# Function to detect outliers using Carling’s Modification
detect_outliers_carling <- function(x) {
  n <- length(x)
  j <- floor((n / 4) + (5 / 12))
  h <- (n / 4) + (5 / 12) - j
  
  x_sorted <- sort(x)
  q1 <- (1 - h) * x_sorted[j] + h * x_sorted[j + 1]
  q3 <- (1 - h) * x_sorted[n - j + 1] + h * x_sorted[n - j]
  
  k <- (17.63 * n - 23.64) / (7.74 * n - 3.71)
  
  lower_bound <- median(x) - k * (q3 - q1)
  upper_bound <- median(x) + k * (q3 - q1)
  
  outliers <- x < lower_bound | x > upper_bound
  return(outliers)
}

# Function to detect outliers using MAD-Median rule
detect_outliers_mad_median <- function(x, k = sqrt(qchisq(0.975, df = 1))) {
  median_x <- median(x, na.rm = TRUE)
  mad_x <- mad(x, constant = 1, na.rm = TRUE)
  outliers <- abs(x - median_x) / (mad_x / 0.6745) > k
  return(outliers)
}

# Function to detect outliers based on adjusted boxplot for skewness
adjboxout <- function(x) {
  result <- adjbox(x, method = "MC", plot = FALSE)
  return(result$out)
}

# Function outbox (combining Carling and IQR methods)
outbox <- function(x, mbox = FALSE, gval = NA) {
  if (mbox) {
    return(detect_outliers_carling(x))
  } else {
    return(detect_outliers_iqr(x, k = ifelse(is.na(gval), 1.5, gval)))
  }
}

# Function out (using MAD-Median rule)
out <- function(x) {
  return(detect_outliers_mad_median(x))
}

# Detect outliers using the various methods
outliers_mean_var <- detect_outliers_mean_var(ages)
outliers_iqr <- detect_outliers_iqr(ages)
outliers_carling <- detect_outliers_carling(ages)
outliers_mad_median <- detect_outliers_mad_median(ages)
outliers_adjbox <- adjboxout(ages)
outliers_outbox <- outbox(ages)
outliers_out <- out(ages)

# Combine all outliers in a data frame for visualization
results <- data.frame(
  Age = ages,
  MeanVar = outliers_mean_var,
  IQR = outliers_iqr,
  Carling = outliers_carling,
  MADMedian = outliers_mad_median,
  AdjBox = ages %in% outliers_adjbox,
  Outbox = outliers_outbox,
  Out = outliers_out
)

# Create plots to visualize the outliers
par(mfrow = c(3, 3))

boxplot(ages, main = "Mean and Variance Method", col = "lightblue")
points(which(results$MeanVar), ages[results$MeanVar], col = "red", pch = 19)

boxplot(ages, main = "IQR Method", col = "lightgreen")
points(which(results$IQR), ages[results$IQR], col = "red", pch = 19)

boxplot(ages, main = "Carling’s Modification", col = "lightpink")
points(which(results$Carling), ages[results$Carling], col = "red", pch = 19)

boxplot(ages, main = "MAD-Median Rule", col = "lightyellow")
points(which(results$MADMedian), ages[results$MADMedian], col = "red", pch = 19)

boxplot(ages, main = "Adjusted Boxplot", col = "lightgrey")
points(which(results$AdjBox), ages[results$AdjBox], col = "red", pch = 19)

boxplot(ages, main = "Outbox Method", col = "lightcyan")
points(which(results$Outbox), ages[results$Outbox], col = "red", pch = 19)

boxplot(ages, main = "Out Method", col = "lightcoral")
points(which(results$Out), ages[results$Out], col = "red", pch = 19)

par(mfrow = c(1, 1))
