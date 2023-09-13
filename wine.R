# Load required libraries
library(randomForest)
library(caret)

# Load wine quality dataset
wine = winequality.white
wine$taste <- ifelse(wine$quality < 5, "bad", "good")
wine$taste[wine$quality == 5] <- "normal"
wine$taste[wine$quality == 6] <- "normal"
wine$taste <- as.factor(wine$taste)
str(wine$taste)

# Create the random forest model
set.seed(123)
rf_model <- randomForest(quality ~ ., data = wine)

# Holdout method
set.seed(456)
train_index <- createDataPartition(wine$quality, p = 0.7, list = FALSE)
train_data <- wine[train_index, ]
test_data <- wine[-train_index, ]
rf_model_holdout <- randomForest(taste ~ . - quality, data = train_data)
pred_holdout <- predict(rf_model_holdout, test_data)
accuracy_holdout <- sum(pred_holdout == test_data$taste)/nrow(test_data)
accuracy_holdout

# Cross-validation
set.seed(789)
folds <- createFolds(wine$taste, k = 5, list = TRUE)
rf_model_cv <- train(taste ~ . - quality, data = wine, method = "rf", trControl = trainControl(method = "cv", index = folds))
pred_cv <- predict(rf_model_cv, wine)
accuracy_cv <- sum(pred_cv == wine$taste)/nrow(wine)
accuracy_cv
# Bootstrapping
set.seed(1011)
rf_model_boot <- randomForest(taste ~ . - quality, data = wine, mtry = sqrt(ncol(wine)), importance = TRUE)
pred_boot <- predict(rf_model_boot, wine)
accuracy_boot <- sum(pred_boot == wine$taste)/nrow(wine)
accuracy_boot
table(pred_boot,wine$quality)
