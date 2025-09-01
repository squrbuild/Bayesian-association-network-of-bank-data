# Step 1: Install and load the necessary R packages
# If you haven't installed these packages, uncomment and run the following lines:
# install.packages("arules")
# install.packages("arulesViz")
# install.packages("bnlearn")
# install.packages("dplyr")
# install.packages("readr")
# install.packages("RColorBrewer")

library(arules)
library(arulesViz)
library(bnlearn)
library(dplyr)
library(readr)
library(RColorBrewer)

setwd("C:/Users/华硕/Desktop/study/毕业论文/code")
# Read your prepared data directly from the file
bank_data_encoded <- read_csv("prepared_bank_data.csv")

# Diagnosis 1: Check the distribution of the y column in the original data
cat("Diagnosis 1: Distribution of y in the original data:\n")
print(table(bank_data_encoded$y))
cat("\n")

# Remove redundant 'y_0' column, keep customer features and target variable 'y'
bank_data_binary <- bank_data_encoded %>%
  select(starts_with("marital_"), starts_with("job_"), starts_with("education_"),
         starts_with("contact_"), starts_with("poutcome_"), starts_with("season_"),
         starts_with("age_"), starts_with("balance_"), starts_with("duration_"),
         starts_with("campaign_"), starts_with("pdays_"), starts_with("previous_"),
         default, housing, loan, y) %>%
  # Rename 'y' column to 'y_1'
  rename(y_1 = y) %>%
  # Ensure y_1 always has two levels '0' and '1'
  mutate(y_1 = factor(y_1, levels = c("0", "1"))) %>%
  # Convert all other numeric columns to factor type
  mutate_if(is.numeric, as.factor)

# Diagnosis 2: Check the distribution of y_1 column in the processed bank_data_binary
cat("Diagnosis 2: Distribution of y_1 in the processed bank_data_binary:\n")
print(table(bank_data_binary$y_1))
cat("\n")


# Step 2: Convert data to arules transactions object
transactions <- as(bank_data_binary, "transactions")


# Step 3: Separate rule mining and combine results
# 3.1 Find rules leading to marketing success (y_1=1)
rules_success <- apriori(
  transactions,
  parameter = list(supp = 0.001, conf = 0.75), # Lower support and confidence for minority class
  appearance = list(rhs = "y_1=1", default = "lhs")
)

# 3.2 Find rules leading to marketing failure (y_1=0)
rules_failure <- apriori(
  transactions,
  parameter = list(supp = 0.005, conf = 0.9), # Higher support and confidence for majority class
  appearance = list(rhs = "y_1=0", default = "lhs")
)

# 1. Filter success rules: lift > 3, confidence > 0.75
rules_success_filtered <- subset(rules_success, lift > 3 & confidence > 0.75)

# 2. Filter failure rules: lift > 3, confidence > 0.9
rules_failure_filtered <- subset(rules_failure, lift > 3 & confidence > 0.9)

# Your code has loaded `rules_success_filtered` and `rules_failure_filtered`, now export
# Convert to data frame
rules_success_df <- as(rules_success_filtered, "data.frame")
rules_failure_df <- as(rules_failure_filtered, "data.frame")

# Export to CSV file
write_csv(rules_success_df, "rules_success.csv")
write_csv(rules_failure_df, "rules_failure.csv")
