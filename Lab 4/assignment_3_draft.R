
# Load required libraries
library(MASS)     # For LDA/QDA
library(class)    # For KNN
library(caret)    # For confusion matrix metrics
library(pROC)     # For ROC curves
library(ggplot2)  # For plotting
library(GGally)   # For pairs plot
library(caTools)  # For data splitting
library(tidyr)
library(dplyr)
library(gridExtra)

# 1. Load dataset and display first few rows
diabetes <- read.csv("C:/Users/benab/OneDrive - iitkgp.ac.in/Desktop/Sem 6/SL Lab/Lab 3/diabetes.csv")
head(diabetes, 10)
diabetes_long <- diabetes %>%
  select(-Outcome) %>%
  gather(key="Variable", value="Value") %>%
  mutate(Outcome = rep(diabetes$Outcome, times=8))


# Combined scatterplot matrix
pairs(diabetes[,-9], 
      col=diabetes$Outcome + 1,
      pch=16,
      main="Scatterplot Matrix of all Variables")

print(p1)

vars <- names(diabetes)[names(diabetes) != "Outcome"]

plots <- list()
for(i in seq_along(vars)) {
  plots[[i]] <- ggplot(diabetes, aes_string(x='factor(Outcome)', y=vars[i], fill='factor(Outcome)')) +
    geom_boxplot() +
    labs(title=vars[i], x="", fill="Outcome") +
    scale_fill_manual(values=c("orange", "lightblue"),
                      labels=c("Non-Diabetic", "Diabetic")) +
    theme_minimal()
}

do.call(grid.arrange, c(plots, ncol=3, 
                        top="Distribution of Variables by Diabetes Status"))

# 2. Preliminary analysis
# Convert Outcome to factor for better visualization
diabetes$Outcome <- factor(diabetes$Outcome, 
                           levels = c(1, 0), 
                           labels = c("Diabetic", "Non-Diabetic"))
# Pairs plot for numeric variables
ggpairs(diabetes[, -9], aes(color = diabetes$Outcome, alpha = 0.5)) +
  ggtitle("Pairs Plot of Diabetes Variables")

# Box plots for each predictor vs Outcome
ggplot(gather(diabetes, key = "Predictor", value = "Value", -Outcome), 
       aes(x = Outcome, y = Value, fill = Outcome)) +
  geom_boxplot() +
  facet_wrap(~ Predictor, scales = "free") +
  theme_minimal()

# 3. Split data and fit LDA
set.seed(43)
split <- sample.split(diabetes$Outcome, SplitRatio = 0.8)
train <- subset(diabetes, split == TRUE)
test <- subset(diabetes, split == FALSE)
head(train)

lda_model <- lda(Outcome ~ ., data = train)
lda_pred <- predict(lda_model, newdata = test)

# 4. Confusion matrix and metrics for LDA
confusion_lda <- confusionMatrix(lda_pred$class, test$Outcome, positive ="Diabetic")
accuracy_lda <- confusion_lda$overall['Accuracy']
f1_lda <- confusion_lda$byClass['F1']

cm_table <- as.data.frame(confusion_lda$table)
colnames(cm_table) <- c("Actual", "Predicted", "Count")

ggplot(cm_table, aes(x = Actual, y = Predicted)) +
  geom_text(aes(label = Count), size = 8, fontface = "bold") +
  labs(title = "Confusion Matrix (LDA)",
       x = "Actual Label",
       y = "Predicted Label") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
# 5. Fit QDA and KNN models
# QDA
qda_model <- qda(Outcome ~ ., data = train)
qda_pred <- predict(qda_model, newdata = test)

# KNN (with feature scaling)
train_scale <- scale(train[, -9])
test_scale <- scale(test[, -9])

knn_pred <- knn(train_scale, test_scale, 
                cl = train$Outcome, k = 5)

# Compare metrics
confusion_qda <- confusionMatrix(qda_pred$class, test$Outcome, positive ="Diabetic")
confusion_qda
confusion_knn <- confusionMatrix(knn_pred, test$Outcome, positive ="Diabetic")

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

# 6. ROC curves for LDA and QDA
roc_lda <- roc(response = test$Outcome, 
               predictor = as.numeric(lda_pred$posterior[,"Diabetic"]),
               levels = c("Non-Diabetic", "Diabetic"))  # Negative first, positive second

roc_qda <- roc(response = test$Outcome, 
               predictor = as.numeric(qda_pred$posterior[,"Diabetic"]),
               levels = c("Non-Diabetic", "Diabetic"))

plot(roc_lda, col = "blue", main = "ROC Curves")
lines(roc_qda, col = "red")
legend("bottomright", legend = c("LDA", "QDA"), col = c("blue", "red"), lwd = 2)

# 7. Vary K for KNN and plot metrics
k_values <- 1:20
results <- data.frame(k = k_values, Accuracy = numeric(20), F1 = numeric(20))

for (k in k_values) {
  knn_pred_temp <- knn(train_scale, test_scale, 
                       cl = train$Outcome, k = k)
  cm <- confusionMatrix(knn_pred_temp, test$Outcome)
  results$Accuracy[k] <- cm$overall['Accuracy']
  results$F1[k] <- cm$byClass['F1']
}

# Plot accuracy vs K
ggplot(results, aes(x = k)) +
  geom_line(aes(y = Accuracy, color = "Accuracy")) +
  geom_line(aes(y = F1, color = "F1 Score")) +
  scale_color_manual(values = c("Accuracy" = "blue", "F1 Score" = "red")) +
  labs(title = "KNN Performance vs Neighborhood Size",
       x = "Number of Neighbors (K)",
       y = "Metric Value") +
  theme_minimal()

