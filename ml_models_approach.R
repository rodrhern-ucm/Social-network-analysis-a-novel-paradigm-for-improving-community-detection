# Load required libraries
library(caret)  # For splitting data and model evaluation
library(e1071)  # Dependency for caret
library(pROC)   # For AUC and ROC curve

# Load data
combined_data <- arrow::read_parquet("C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/output/ml_input_data.parquet")
combined_data <- na.omit(combined_data)

# Ensure improvement is numeric for continuous regression
combined_data$improvement <- as.numeric(as.character(combined_data$improvement))

# Split the data into training and testing sets (80/20 split)
set.seed(12345) # For reproducibility
train_index <- createDataPartition(combined_data$improvement, p = 0.8, list = FALSE)
train_data <- combined_data[train_index, ]
test_data <- combined_data[-train_index, ]

# Fit a linear regression model
linear_model <- lm(improvement ~ Var1 + Var2 + Var3 + algorithm, 
                   data = train_data)

# Summary of the model
summary(linear_model)

# Predict on the test set
test_data$predicted <- predict(linear_model, newdata = test_data)

# Evaluate the model using Mean Squared Error (MSE)
mse <- mean((test_data$predicted - test_data$improvement)^2)
cat("Mean Squared Error (MSE):", mse, "\n")

# Evaluate the model using Mean Absolute Error (MAE)
mae <- mean(abs(test_data$predicted - test_data$improvement))
cat("Mean Absolute Error (MAE):", mae, "\n")

# Optional: Plot predicted vs actual values
plot(test_data$improvement, test_data$predicted, 
     main = "Predicted vs Actual", 
     xlab = "Actual Values", ylab = "Predicted Values", 
     pch = 16, col = "blue")
abline(0, 1, col = "red")
