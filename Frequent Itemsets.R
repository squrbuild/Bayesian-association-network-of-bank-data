# Set working directory
setwd("C:/Users/华硕/Desktop/study/毕业论文/code")
print(getwd())



# 1. Load necessary packages, install if not available
if (!require(arules)) {
  install.packages("arules")
  library(arules)
} else {
  library(arules)
}


df <- read.csv("prepared_bank_data.csv", stringsAsFactors = FALSE)

str(df)
df_trans <- df

df_trans[] <- lapply(names(df_trans), function(col) {
  ifelse(df_trans[[col]] == 1, col, NA)
})


transactions_list <- apply(df_trans, 1, function(row) na.omit(row))

trans <- as(transactions_list, "transactions")

frequent_items <- apriori(trans, parameter = list(supp = 0.1, target = "frequent itemsets", minlen = 2))

inspect(head(frequent_items, n = 10))
rules <- apriori(trans,
                 parameter = list(supp = 0.02, conf = 0.2, minlen = 2),
                 appearance = list(rhs = c("y"), default = "lhs"))

rules_sorted <- sort(rules, by = "lift", decreasing = TRUE)
inspect(head(rules_sorted, 10))


length(rules)

appearance = list(rhs = c("y_1"), default = "lhs")
