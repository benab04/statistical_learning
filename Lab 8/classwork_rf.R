install.packages("randomForest")  # Install the randomForest package
library(randomForest)             # Load randomForest for modeling
library(MLmetrics)                # Load MLmetrics for evaluation
library(MASS)                     # Load MASS for the Boston dataset
library(catdata)                  # Load catdata for the heart dataset

### CLASSIFICATION

# Heartdata
data(heart)
Heartdata <- as.data.frame(heart)  # Convert dataset into data.frame
Heartdata

# Splitting of data
#set.seed(1)
#sample <- sample(c(TRUE, FALSE), nrow(Heartdata), replace = TRUE, prob = c(0.7, 0.3))
#train <- Heartdata[sample, ]
#test <- Heartdata[!sample, ]
#train
#test

# Training the model
rf.heart <- randomForest(factor(y) ~ ., data = train)
summary(rf.heart)
rf.heart

# Confusion Matrix
ypred <- predict(rf.heart, test, type = 'class')
ypred
table(predict = ypred, truth = test$y)

# Metrics
ConfusionMatrix(ypred, test$y)  # Generates the confusion matrix
Accuracy(ypred, test$y)        # Computes accuracy
Precision(ypred, test$y)       # Computes precision: TP / (TP + FP)
Recall(ypred, test$y)
F1_Score(ypred, test$y)

rf.heart <- randomForest(factor(y) ~ ., data = train, mtry = 9,ntree=25, importance = TRUE) #mtry =9 for bagging model, ie, equal to no of predictors
summary(rf.heart)
rf.heart
ypred <- predict(rf.heart, test, type = 'class')
Accuracy(ypred, test$y)


### REGRESSION

# Commented out to use same train test data
#data(Boston)
#set.seed(1)
#sample <- sample(c(TRUE, FALSE), nrow(Boston), replace = TRUE, prob = c(0.7, 0.3))
#train <- Boston[sample, ]
#test <- Boston[!sample, ]
#train
#test

## Training the model with mtry=13
set.seed(1)
rf.boston <- randomForest(medv ~ ., data = train, mtry = 13, importance = TRUE)
rf.boston

# Plot
ypred <- predict(rf.boston, test)
plot(ypred, test$medv)
abline(0, 1)
mean((ypred - test$medv)^2)

# Or use these functions directly
MSE(ypred, test$medv)
MAE(ypred, test$medv)
RMSE(ypred, test$medv)
MAPE(ypred, test$medv)


importance(rf.boston)
varImpPlot(rf.boston)