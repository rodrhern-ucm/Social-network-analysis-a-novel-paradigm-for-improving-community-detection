# Load necessary libraries
library(caret)
library(e1071)
library(randomForest)
library(xgboost)
library(Metrics)

# Load the dataset
data <- arrow::read_parquet("C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/output/ml_input_data.parquet")

# Preprocessing
# Remove unnecessary columns (adjust based on dataset inspection)
data <- data[, !(names(data) %in% c("Unnamed: 0", "network", "algorithm", "improvement_perc"))]

# Ensure target variable is a factor
data$improvement <- as.factor(data$improvement)

# Split the data into training and testing sets
set.seed(12345)
trainIndex <- createDataPartition(data$improvement, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Prepare control for training
control <- trainControl(method = "cv", number = 5, search = "grid")

# Initialize a results dataframe
results <- data.frame(Model = character(), MSE = numeric(), RMSE = numeric(), stringsAsFactors = FALSE)

# Logistic Regression (no hyperparameters to tune)
set.seed(12345)
log_model <- train(improvement ~ ., data = trainData, method = "glm", family = "binomial", trControl = control)
log_pred <- predict(log_model, testData, type = "prob")[,2]
log_mse <- mse(as.numeric(testData$improvement) - 1, log_pred)
log_rmse <- rmse(as.numeric(testData$improvement) - 1, log_pred)
results <- rbind(results, c("Logistic Regression", log_mse, log_rmse))

# Support Vector Machine (SVM) with hyperparameter tuning
svmGrid <- expand.grid(sigma = c(0.01, 0.05, 0.1), C = c(1, 10, 100))
set.seed(12345)
svm_model <- train(improvement ~ ., data = trainData, method = "svmRadial", trControl = control, tuneGrid = svmGrid)
svm_pred <- predict(svm_model, testData, type = "prob")[,2]
svm_mse <- mse(as.numeric(testData$improvement) - 1, svm_pred)
svm_rmse <- rmse(as.numeric(testData$improvement) - 1, svm_pred)
results <- rbind(results, c("SVM", svm_mse, svm_rmse))

# Random Forest with hyperparameter tuning
rfGrid <- expand.grid(mtry = c(2, 3, 4))
set.seed(12345)
rf_model <- train(improvement ~ ., data = trainData, method = "rf", trControl = control, tuneGrid = rfGrid)
rf_pred <- predict(rf_model, testData, type = "prob")[,2]
rf_mse <- mse(as.numeric(testData$improvement) - 1, rf_pred)
rf_rmse <- rmse(as.numeric(testData$improvement) - 1, rf_pred)
results <- rbind(results, c("Random Forest", rf_mse, rf_rmse))

# XGBoost with hyperparameter tuning
xgbGrid <- expand.grid(nrounds = c(50, 100), max_depth = c(3, 6), eta = c(0.01, 0.1), gamma = c(0, 1), colsample_bytree = c(0.6, 0.8), min_child_weight = c(1, 5), subsample = c(0.7, 1))
set.seed(12345)
xgb_model <- train(improvement ~ ., data = trainData, method = "xgbTree", trControl = control, tuneGrid = xgbGrid)
xgb_pred <- predict(xgb_model, testData, type = "prob")[,2]
xgb_mse <- mse(as.numeric(testData$improvement) - 1, xgb_pred)
xgb_rmse <- rmse(as.numeric(testData$improvement) - 1, xgb_pred)
results <- rbind(results, c("XGBoost", xgb_mse, xgb_rmse))

# Decision Trees with hyperparameter tuning
dtGrid <- expand.grid(cp = c(0.01, 0.05, 0.1))
set.seed(12345)
dt_model <- train(improvement ~ ., data = trainData, method = "rpart", trControl = control, tuneGrid = dtGrid)
dt_pred <- predict(dt_model, testData, type = "prob")[,2]
dt_mse <- mse(as.numeric(testData$improvement) - 1, dt_pred)
dt_rmse <- rmse(as.numeric(testData$improvement) - 1, dt_pred)
results <- rbind(results, c("Decision Tree", dt_mse, dt_rmse))

# Convert results to appropriate types
results$MSE <- as.numeric(results$MSE)
results$RMSE <- as.numeric(results$RMSE)

# Display results
print(results)
