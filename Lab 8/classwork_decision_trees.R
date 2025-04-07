# REGRESSION
library(MASS)

# Boston
data(Boston)
?Boston
dataset <- as.data.frame(Boston)
dataset

# Splitting of data
sample <- sample(c(TRUE, FALSE), nrow(dataset), replace = TRUE, prob = c(0.7, 0.3))
train <- dataset[sample, ]
test <- dataset[!sample, ]
train
test

# Training the model
tree.boston <- rpart(medv ~ ., data = train, method = 'anova')
summary(tree.boston)
tree.boston
rpart.plot(tree.boston, cex=0.8, box.palette = "Browns")

# Making predictions
ypred <- predict(tree.boston, test)
ypred
# Calculating Metrics
MSE <- mean((test$medv - ypred)^2) 
MAE <- mean(abs(test$medv - ypred))
R2 <- 1 - sum((test$medv - ypred)^2) / sum((test$medv - mean(test$medv))^2) 

# Metrics
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("R-squared (R2):", R2, "\n")

# Or use these functions directly
MSE(ypred, test$medv)
MAE(ypred, test$medv)
RMSE(ypred, test$medv)
MAPE(ypred, test$medv)


