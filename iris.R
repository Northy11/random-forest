#### Random Forest with Holdout method





library(randomForest)

# Load data

data(iris)



# Split data into 70% training and 30% testing

train_index <- sample(nrow(iris), 0.7 * nrow(iris))

train_data <- iris[train_index, ]

test_data <- iris[-train_index, ]



# Train Random Forest model

rf_model <- randomForest(Species ~ ., data = train_data)



# Make predictions on test data

rf_pred <- predict(rf_model, test_data)



# Calculate accuracy of model

table(rf_pred, test_data$Species)





#### Random Forest with Cross Validation





library(caret)

library(randomForest)



# Load data

data(iris)



# Train Random Forest model with cross validation

train_control <- trainControl(method = "cv", number = 10)

rf_model <- train(Species ~ ., data = iris, method = "rf", trControl = train_control)



# Print the model results

print(rf_model)



# Make predictions on test data

rf_pred <- predict(rf_model, iris[,-5])



# Calculate accuracy of model

table(rf_pred, iris[,5])





#### Random Forest with Bootstrapping





library(randomForest)



# Load data

data(iris)



# Train Random Forest model with bootstrapping

rf_model <- randomForest(Species ~ ., data = iris, bootstrap = TRUE)



# Print the model results

print(rf_model)



# Make predictions on test data

rf_pred <- predict(rf_model, iris[,-5])



# Calculate accuracy of model

table(rf_pred, iris[,5])

