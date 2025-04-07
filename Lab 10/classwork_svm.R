library("e1071")    # For SVM implementation
library(GGally)     # For pairwise plot visualization
library(ggplot2)    # For data visualization
library(caret)      # for confusion matrix

data(iris)
summary(iris)
str(iris)
head(iris,5)
svm_model <- svm(Species ~ .,data=iris,kernel="radial", cost = 1, epsilon=0.1)
summary(svm_model)
ggpairs(iris, ggplot2::aes(colour = Species, alpha = 0.4))
plot(svm_model, data=iris,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3, Sepal.Length=4)
)
pred = predict(svm_model,iris)
tab = table(Predicted=pred, Actual = iris$Species)
tab
misc_rate <- 1-sum(diag(tab)/sum(tab)) #misclassification rate
print(misc_rate)
accuracy <-1- misc_rate
print(accuracy)

set.seed(123)
# tmodel=tune(svm,Species~., data=iris,
            # ranges=list(epsilon=seq(0,1,0.1), cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
tmodel=tune(svm,Species~., data=iris,
            ranges=list(gamma=c(0.01, 0.1, 1), cost = 2^(2:7)))

plot(tmodel)
summary(tmodel)

mymodel=tmodel$best.model
summary(mymodel)
plot(mymodel, data=iris,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3, Sepal.Length=4))

pred1 = predict(mymodel,iris)
confusionMatrix(pred1, iris$Species)
tab1 = table(Predicted=pred1, Actual = iris$Species)
tab1
1-sum(diag(tab1)/sum(tab1))

#Effect of different kernel
# Linear Kernel
svm_linear <- svm(Species ~ ., data=iris, kernel="linear")

# Polynomial Kernel
svm_poly <- svm(Species ~ ., data=iris, kernel="polynomial", degree=3)

# RBF Kernel
svm_rbf <- svm(Species ~ ., data=iris, kernel="radial")

# Compare Performance
pred_linear <- predict(svm_linear, iris)
pred_poly <- predict(svm_poly, iris)
pred_rbf <- predict(svm_rbf, iris)

confusionMatrix(pred_linear, iris$Species)
confusionMatrix(pred_poly, iris$Species)
confusionMatrix(pred_rbf, iris$Species)
