# Load necessary libraries
library(caret)
library(e1071)
library(randomForest)
library(xgboost)
library(Metrics)
library(doParallel)
library(dplyr)

set.seed(12345)

# Load the dataset
data <- arrow::read_parquet("C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/Tabla Modularidades/Nuevos modelos/ml_input_data.parquet")
data[is.na(data)] <- 0

# Preprocessing
# Remove unnecessary columns (adjust based on dataset inspection)
data <- data[, !(names(data) %in% c("Unnamed: 0", "network","Var3", "improvement_perc"))]

# Ensure target variable is numeric
data$improvement <- as.numeric(data$improvement)

# Sample the data to reduce the number of observations while maintaining the same strata distribution
set.seed(12345)
sampleIndex <- createDataPartition(data$improvement, p = 0.01, list = FALSE)  # Adjust p to control the sample size
sampledData <- data[sampleIndex, ]

# Split the sampled data into training and testing sets
set.seed(12345)
trainIndex <- createDataPartition(sampledData$improvement, p = 0.8, list = FALSE)
trainData <- sampledData[trainIndex, ]
testData <- sampledData[-trainIndex, ]

# Ensure there are no NA values in the training and testing sets
if (any(is.na(trainData)) || any(is.na(testData))) {
  stop("Training or testing data contains NA values. Please clean the data before proceeding.")
}

# Prepare control for training
control <- trainControl(method = "cv", number = 3, search = "grid")

# Initialize a results dataframe for model performance
results <- data.frame(Model = character(), MSE = numeric(), RMSE = numeric(), stringsAsFactors = FALSE)

# Initialize a list to store variable importance for each model
variable_importance_list <- list()

# Logistic Regression (no hyperparameters to tune)
log_model <- train(improvement ~ ., data = trainData, method = "glm", trControl = control)
log_pred <- predict(log_model, testData)
log_mse <- mse(testData$improvement, log_pred)
log_rmse <- rmse(testData$improvement, log_pred)
results <- rbind(results, c("Logistic Regression", log_mse, log_rmse))

# Extract variable importance for logistic regression
log_importance <- varImp(log_model)
variable_importance_list[["Logistic Regression"]] <- log_importance$importance

# Support Vector Machine (SVM) for regression
svmGrid <- expand.grid(sigma = c(0.01, 0.05, 0.1), C = c(1, 10, 100))
svm_model <- train(improvement ~ ., data = trainData, method = "svmRadial", trControl = control, tuneGrid = svmGrid)
svm_pred <- predict(svm_model, testData)
svm_mse <- mse(testData$improvement, svm_pred)
svm_rmse <- rmse(testData$improvement, svm_pred)
results <- rbind(results, c("SVM", svm_mse, svm_rmse))

# Extract variable importance for SVM using permutation importance
library(DALEX)

# Create an explainer for the SVM model
explainer <- explain(svm_model, data = trainData, y = trainData$improvement)

# Compute SHAP values for a single observation
shap_values <- predict_parts(explainer, new_observation = trainData[1, ], type = "shap")
print(shap_values)

# Visualize SHAP values
plot(shap_values)

# Random Forest with hyperparameter tuning
rf_grid <- expand.grid(mtry = c(2, 3, 4))
rf_model <- train(improvement ~ ., data = trainData, method = "rf", trControl = control, tuneGrid = rf_grid)
rf_pred <- predict(rf_model, testData)
rf_mse <- mse(testData$improvement, rf_pred)
rf_rmse <- rmse(testData$improvement, rf_pred)
results <- rbind(results, c("Random Forest", rf_mse, rf_rmse))

# Extract variable importance for Random Forest
rf_importance <- varImp(rf_model)
variable_importance_list[["Random Forest"]] <- rf_importance$importance

# XGBoost with hyperparameter tuning
xgbGrid <- expand.grid(nrounds = c(50, 100), max_depth = c(3, 6), eta = c(0.01, 0.1), gamma = c(0, 1), colsample_bytree = c(0.6, 0.8), min_child_weight = c(1, 5), subsample = c(0.7, 1))
xgb_model <- train(improvement ~ ., data = trainData, method = "xgbTree", trControl = control, tuneGrid = xgbGrid)
xgb_pred <- predict(xgb_model, testData)
xgb_mse <- mse(testData$improvement, xgb_pred)
xgb_rmse <- rmse(testData$improvement, xgb_pred)
results <- rbind(results, c("XGBoost", xgb_mse, xgb_rmse))

# Extract variable importance for XGBoost
xgb_importance <- varImp(xgb_model)
variable_importance_list[["XGBoost"]] <- xgb_importance$importance

# Decision Trees with hyperparameter tuning
dtGrid <- expand.grid(cp = c(0.01, 0.05, 0.1))
dt_model <- train(improvement ~ ., data = trainData, method = "rpart", trControl = control, tuneGrid = dtGrid)
dt_pred <- predict(dt_model, testData)
dt_mse <- mse(testData$improvement, dt_pred)
dt_rmse <- rmse(testData$improvement, dt_pred)
results <- rbind(results, c("Decision Tree", dt_mse, dt_rmse))

# Extract variable importance for Decision Tree
dt_importance <- varImp(dt_model)
variable_importance_list[["Decision Tree"]] <- dt_importance$importance

# Convert results to appropriate types
results$MSE <- as.numeric(results$MSE)
results$RMSE <- as.numeric(results$RMSE)

# Display results
print(results)

# Combine variable importance into a single table
# Ensure variables are ordered consistently
ordered_variable_names <- rownames(variable_importance_list[[1]])  # Use the variable names from the first model

# Create an empty data frame to store the combined importance scores
variable_importance_table <- data.frame(Variable = ordered_variable_names)

# Add importance scores for each model to the table
for (model_name in names(variable_importance_list)) {
  # Extract importance scores for the current model
  importance_scores <- variable_importance_list[[model_name]]
  
  # Ensure the scores are ordered according to `ordered_variable_names`
  importance_scores <- importance_scores[ordered_variable_names, , drop = FALSE]
  
  # Add the scores to the table
  variable_importance_table[[model_name]] <- importance_scores$Overall
}

# Normalize the variable importance scores so that they sum to 1 for each model
normalized_importance_table <- variable_importance_table
for (model in colnames(normalized_importance_table)[-1]) {  # Skip the first column (Variable)
  normalized_importance_table[[model]] <- normalized_importance_table[[model]] / sum(normalized_importance_table[[model]])
}

# Display the normalized variable importance table
print(normalized_importance_table)


