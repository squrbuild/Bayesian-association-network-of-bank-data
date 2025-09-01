library(igraph)
library(bnlearn)
library(dplyr)
library(stringr)
library(readr)
setwd("C:/Users/华硕/Desktop/study/毕业论文/code")


# Load data files
rules_df <- read_csv("rules_success.csv")
bank_data_full <- read_csv("prepared_bank_data.csv")

# Extract variables
lhs_strings <- sub(" => .*", "", rules_df$rules)
cleaned_lhs <- gsub("[\\{\\}]", "", lhs_strings)
items_list <- strsplit(cleaned_lhs, ",")
all_items <- unlist(items_list)
variable_names <- sub("=.*", "", all_items)
unique_antecedent_vars <- unique(variable_names)

# Create and use name mapping table
name_map <- data.frame(
  original_name = unique_antecedent_vars,
  new_name = paste0("Var", seq_along(unique_antecedent_vars))
)
data_for_bn_intermediate <- bank_data_full %>%
  select(y, all_of(name_map$original_name))
names(data_for_bn_intermediate)[-1] <- name_map$new_name
data_for_bn_intermediate <- data_for_bn_intermediate %>%
  rename(y_1 = y)
data_for_bn_intermediate$y_1 <- factor(data_for_bn_intermediate$y_1, levels = c("0", "1"))
data_for_bn_intermediate <- data_for_bn_intermediate %>%
  mutate(across(all_of(name_map$new_name), as.factor))
data_for_bn <- as.data.frame(data_for_bn_intermediate)

# Build and fit the model
bn_structure <- hc(data_for_bn, score = "bic")
bn_model <- bn.fit(bn_structure, data = data_for_bn)

# *** New feature: extract and save all variable levels (for dropdown menus) ***
variable_levels <- lapply(data_for_bn, levels)

# Save all necessary files
saveRDS(bn_model, file = "bayesian_network_model.rds")
saveRDS(name_map, file = "name_map.rds")
saveRDS(variable_levels, file = "variable_levels.rds") # <-- New line

cat("--- Preparation complete! ---\n")
cat("Successfully generated bayesian_network_model.rds, name_map.rds, variable_levels.rds, and bayesian_network_model.bif files.\n")
