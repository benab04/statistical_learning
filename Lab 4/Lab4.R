library(ISLR2)
library(ggplot2)
library(MASS)
library(class)    # For KNN
library(caret)    # For confusion matrix metrics
library(pROC)     # For ROC curves
library(ggplot2)  # For plotting
library(GGally)   # For pairs plot
library(caTools)  # For data splitting
head(Smarket)
attach(Smarket)
unique(Direction)
unique(Year)
dim(Smarket)
?Smarket
ggplot(Smarket, aes(x = Direction, y = Volume, fill = Direction)) +
  geom_boxplot()

p <- ggplot(Smarket, aes(y = Volume))
p +geom_histogram()

# LDA

lda_fit <- lda(Direction ~ Lag1+ Lag2,data = Smarket, subset = Year< 2005)
lda_fit

par(mar=c(1,1,1,1))
plot(lda_fit)

subset.2005 <- subset(Smarket, Year == 2005)

pred <- predict( lda_fit, newdata = subset.2005)
?predict.lda

pred.df <- data.frame(pred)
head(pred.df)

table(Actual = subset.2005$Direction,Predicted= pred.df$class)
?table


# QDA

attach(iris)
str(iris)
head(iris)


iris[1:4] <- scale(iris[1:4])

head(iris)

apply(iris[1:4], 2, mean)
apply(iris[1:4], 2, sd)

set.seed(1)

sample <- sample(c(TRUE, FALSE), nrow(iris), replace =TRUE, prob = c(0.7, 0.3))

sample
sample_n()

train <- iris[sample,]
test <- iris[!sample,]

qmodel <- qda( Species ~., data= train)
qmodel
qpredicted <- predict( qmodel, test)
qp1 <- predict( qmodel, train )$class
qtab <- table(Predicted =qp1, Actual = train$Species)
qtab

sum ( diag(qtab))/sum(qtab)

qp2 <- predict( qmodel, test)$class

qtab1 <- table(Predicted= qp2, Actual = test$Species)
qtab1
sum(diag(qtab1))/ sum(qtab1)


# KNN
set.seed(10000)
View(Default)
Default$student = as.numeric(Default$student) -1
Default$student

head(Default)
dim(Default)
default_index = sample(nrow(Default), 5000)
default_train = Default[ default_index, ]
default_test = Default[-default_index, ]


X_default_train = default_train[, -1]
y_default_train = default_train$default

X_default_test = default_test[, -1]
y_default_test = default_test$default

dim(y_default_train)
dim(y_default_test)

predicted = knn(train =X_default_train,
                test = X_default_test, 
                cl= y_default_train, k=50)

predicted[1:5]

tab <- table(Predicted = predicted, Actual = y_default_test)
tab
accuracy = sum(diag(tab))/sum(tab)
accuracy

set.seed(42)
k_to_try = 1: 100
acc_k = rep(x=0, times=length(k_to_try))

for(i in seq_along(k_to_try)){
  pred=knn(train = scale(X_default_train),
           test = scale(X_default_test),
           cl = y_default_train,
           k = k_to_try[i]
           )
  cm= confusionMatrix(pred, y_default_test)
  acc_k[i] = cm$overall["Accuracy"]
}


acc_k

max_accuracy <- max(acc_k)
best_k <- k_to_try[which.max(acc_k)]
best_k


plot(k_to_try, acc_k, type = "b", col = "blue", pch = 19, 
     xlab = "Number of Neighbors (k)", ylab = "Accuracy",
     main = "Accuracy vs k in k-NN")
abline(v = best_k, col = "red", lty = 2)  # Add vertical line at best_k
text(best_k, max_accuracy, 
     labels = paste("Best k =", best_k), pos = 3, col = "red")