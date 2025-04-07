diabetes <- read.csv("C:/Users/benab/OneDrive - iitkgp.ac.in/Desktop/Sem 6/SL Lab/Lab 3/diabetes.csv")
head(diabetes, 10)
names(diabetes)

plot(diabetes)

install.packages("dplyr")  
install.packages("tidyr") 
library(caret)
library(dplyr)             
library(tidyr)  
library(ggplot2)

# Modify the original dataset
diabetes_long <- diabetes %>%
  pivot_longer(cols = -Outcome, names_to = "Variable", values_to = "Value")

# Create a boxplot of all variables affecting Outcome
ggplot(diabetes_long, aes(x = factor(Outcome), y = Value, fill = factor(Outcome))) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8, alpha = 0.7) +
  scale_fill_manual(values = c("lightblue", "lightcoral")) +
  labs(
    title = "Boxplots of Variables Affecting Outcome",
    x = "Outcome (0 = No Diabetes, 1 = Diabetes)",
    y = "Value"
  ) +
  facet_wrap(~ Variable, scales = "free_y") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  )

# Correlation matrix to check for extreme correlation among predictors
correlation_matrix <- cor(diabetes[, -ncol(diabetes)])
correlation_matrix
install.packages("corrplot")
library(corrplot)
corrplot(correlation_matrix, method="square", addCoef.col = "black")


set.seed(32)
train <- sample(nrow(diabetes), 0.8 *nrow(diabetes)) # Sample 80% of data to be training
train_data <- diabetes[train,]
test_data <- diabetes[-train, ] # 20% of data kept for testing

# Training model M1 on all predictors in training data
model_M1 <- glm(Outcome ~ ., data = train_data, family = binomial)

# Summary of the model
summary(model_M1)
exp(coef(model_M1))

# Predict probabilities on test data
predicted_probs <- predict(model_M1, newdata = test_data, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

# Confusion matrix
confusion_matrix <- table(Predicted = predicted_classes, Actual = test_data$Outcome)
confusion_matrix


# Accuracy
accuracy <- mean(predicted_classes == test_data$Outcome)
cat("Accuracy:", accuracy, "\n")

# F1-score
f1_score <- F_meas(as.factor(predicted_classes), as.factor(test_data$Outcome))
cat("F1-Score:", f1_score, "\n")


# Fit model M2 with selected predictors in training data
model_M2 <- glm(Outcome ~ Pregnancies + Glucose + BMI, data = train_data, family = binomial)

# Check difference in deviance of models
print(model_M1$deviance - model_M2$deviance)

# Deviance comparison of two models
anova_result <- anova(model_M1, model_M2, test = "Chisq")
print(anova_result)

