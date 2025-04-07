library(MASS)     
library(class)    
library(caret)    
library(pROC)     
library(ggplot2) 
library(GGally) 
library(caTools) 
library(tidyr)
library(dplyr)
library(gridExtra)

diabetes <- read.csv("C:/Users/benab/OneDrive - iitkgp.ac.in/Desktop/Sem 6/SL Lab/Lab 3/diabetes.csv")
head(diabetes, 10)

diabetes$Outcome <- factor(diabetes$Outcome, labels = c("Non-Diabetic", "Diabetic"))
head(diabetes, 10)

ggpairs(diabetes[, -9], aes(color = diabetes$Outcome, alpha = 0.5)) +
  ggtitle("Pairs Plot of Diabetes Variables")

ggplot(gather(diabetes, key = "Predictor", value = "Value", -Outcome), 
       aes(x = Outcome, y = Value, fill = Outcome)) +
  geom_boxplot() +
  facet_wrap(~ Predictor, scales = "free") +
  theme_minimal()


set.seed(123)
split <- sample.split(diabetes$Outcome, SplitRatio = 0.8)
train <- subset(diabetes, split == TRUE)
test <- subset(diabetes, split == FALSE)
head(train)

lda_model <- lda(Outcome ~ ., data = train)
lda_model

lda_pred <- predict(lda_model, newdata = test)

results <- data.frame(
  Actual = test$Outcome,
  Predicted = lda_pred$class
)

# Plot actual vs predicted values
ggplot(results, aes(x = Actual, y = Predicted, color = Actual)) +
  geom_jitter(width = 0.2, height = 0.2, size = 2, alpha = 0.7) +
  labs(title = "Actual vs Predicted Diabetes Outcome (LDA)",
       x = "Actual Outcome",
       y = "Predicted Outcome",
       color = "Outcome") +
  scale_color_manual(values = c("Non-Diabetic" = "blue", "Diabetic" = "red")) +
  theme_minimal()

# 4. Confusion matrix and metrics for LDA
confusion_lda <- confusionMatrix(lda_pred$class, test$Outcome, positive ="Diabetic")
confusion_lda


accuracy_lda <- confusion_lda$overall['Accuracy']
accuracy_lda
f1_lda <- confusion_lda$byClass['F1']
f1_lda

# QDA Model
qda_model <- qda(Outcome ~ ., data = train)
qda_pred <- predict(qda_model, newdata = test)

# KNN Model
train_scale <- scale(train[, -9])
test_scale <- scale(test[, -9])

knn_pred <- knn(train_scale, test_scale, 
                cl = train$Outcome, k = 5)

confusion_qda <- confusionMatrix(qda_pred$class, test$Outcome, positive ="Diabetic")
confusion_qda

confusion_knn <- confusionMatrix(knn_pred, test$Outcome, positive ="Diabetic")
confusion_knn

metrics <- data.frame(
  Model = c("LDA", "QDA", "KNN (k=5)"),
  Accuracy = c(accuracy_lda, 
               confusion_qda$overall['Accuracy'],
               confusion_knn$overall['Accuracy']),
  F1_Score = c(f1_lda,
               confusion_qda$byClass['F1'],
               confusion_knn$byClass['F1'])
)
print(metrics)


roc_lda <- roc(response = test$Outcome, 
               predictor = as.numeric(lda_pred$posterior[,"Diabetic"]),
               levels = c("Non-Diabetic", "Diabetic"))  # Negative first, positive second

roc_qda <- roc(response = test$Outcome, 
               predictor = as.numeric(qda_pred$posterior[,"Diabetic"]),
               levels = c("Non-Diabetic", "Diabetic"))

plot(roc_lda, col = "blue", main = "ROC Curves")
lines(roc_qda, col = "red")
legend("bottomright", legend = c("LDA", "QDA"), col = c("blue", "red"), lwd = 2)


k_values <- 1:20
results <- data.frame(k = k_values, Accuracy = numeric(20), F1 = numeric(20))

for (k in k_values) {
  knn_pred_temp <- knn(train_scale, test_scale, 
                       cl = train$Outcome, k = k)
  cm <- confusionMatrix(knn_pred_temp, test$Outcome)
  results$Accuracy[k] <- cm$overall['Accuracy']
  results$F1[k] <- cm$byClass['F1']
}

best_k_accuracy <- results$k[which.max(results$Accuracy)]
best_k_f1 <- results$k[which.max(results$F1)]

cat("Best k for Accuracy:", best_k_accuracy, "\n")
cat("Best k for F1 Score:", best_k_f1, "\n")

best_accuracy <- max(results$Accuracy)
best_f1 <- max(results$F1)

cat("Highest Accuracy:", best_accuracy, "\n")
cat("Highest F1 Score:", best_f1, "\n")

# Plot accuracy and f1 score vs K
ggplot(results, aes(x = k)) +
  geom_line(aes(y = Accuracy, color = "Accuracy")) +
  geom_line(aes(y = F1, color = "F1 Score")) +
  scale_color_manual(values = c("Accuracy" = "blue", "F1 Score" = "red")) +
  labs(title = "KNN Performance vs Neighborhood Size",
       x = "Number of Neighbors (K)",
       y = "Metric Value") +
  theme_minimal()

