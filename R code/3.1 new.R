# Load the necessary libraries
library(titanic)
library(boot)

# Load the Titanic dataset
data("titanic_train")

# View the first few rows of the dataset
head(titanic_train)

# Extract a relevant column for analysis, e.g., 'Age'
# Remove NA values for the purpose of this example
ages <- na.omit(titanic_train$Age)

# Summary statistics of the 'ages' data
summary(ages)

# Define a function to compute the statistic of interest, here we'll use the mean
mean_age <- function(data, indices) {
  return(mean(data[indices]))
}

# Perform bootstrap with 1000 resamples
set.seed(123)  # For reproducibility
boot_results <- boot(data = ages, statistic = mean_age, R = 1000)

# Print the bootstrap results
print(boot_results)

# Plot the bootstrap results
plot(boot_results)

# Compute the bootstrap estimate of the standard error
boot_se <- sd(boot_results$t)
cat("Bootstrap Standard Error of the Mean Age:", boot_se, "\n")

# Verify the results manually
# Compute the original sample mean and standard error
sample_mean <- mean(ages)
sample_se <- sd(ages) / sqrt(length(ages))
cat("Original Sample Mean Age:", sample_mean, "\n")
cat("Original Sample Standard Error of the Mean Age:", sample_se, "\n")
