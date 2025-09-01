install.packages("xgboost")
install.packages("SHAPforxgboost")
install.packages("lime")
install.packages("caret")
install.packages("Matrix")
library(xgboost)
library(SHAPforxgboost)
library(lime)
library(caret)
library(Matrix)
# Set working directory
setwd("C:/Users/华硕/Desktop/study/毕业论文/code")
print(getwd())

# 1. Prepare the data
df <- read.csv("prepared_bank_data.csv")
y <- df$y
X <- df[, setdiff(names(df), "y")]

set.seed(42)
model <- train(x = X, y = as.factor(y), method = "rf", trControl = trainControl(method = "cv", number = 3))
# Create explainer
explainer <- lime(X, model)

# Explain the first 5 data points, showing the key features for each prediction
explanation <- explain(X[1:5, ], explainer, n_features = 5, n_labels = 1)

# Visualize the explanation
plot_features(explanation)

# Train xgboost with sparse matrix
X_sparse <- sparse.model.matrix(~ . -1, data = X)
dtrain <- xgb.DMatrix(data = X_sparse, label = y)

xgb_model <- xgboost(data = dtrain,
                     max.depth = 4,
                     eta = 0.1,
                     nround = 100,
                     objective = "binary:logistic",
                     verbose = 0)

# SHAP value computation
shap_values <- shap.values(xgb_model = xgb_model, X_train = X_sparse)

# Convert sparse matrix to data.frame for shap.prep
X_dense <- as.data.frame(as.matrix(X_sparse))

# Prepare SHAP data frame
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = X_dense)

# Visualize global SHAP bar plot (mean absolute contribution)
shap.plot.summary(shap_long)
