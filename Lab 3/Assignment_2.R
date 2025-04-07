diabetes <- read.csv("C:/Users/benab/OneDrive - iitkgp.ac.in/Desktop/Sem 6/SL Lab/Lab 3/diabetes.csv")
head(diabetes, 10)

# Load libraries for visualization
library(ggplot2)

# Scatter plot: Glucose vs Outcome
ggplot(diabetes, aes(x = Glucose, y = Outcome)) +
  geom_point() +
  ggtitle("Scatter Plot: Glucose vs Outcome") +
  xlab("Glucose") +
  ylab("Outcome")

# Box plot: BMI by Outcome
ggplot(diabetes, aes(x = as.factor(Outcome), y = BMI)) +
  geom_boxplot() +
  ggtitle("Box Plot: BMI by Outcome") +
  xlab("Outcome") +
  ylab("BMI")

# Correlation matrix
correlation_matrix <- cor(diabetes[, -ncol(diabetes)])
correlation_matrix

# Split the dataset
set.seed(123)  # For reproducibility
sample_index <- sample(seq_len(nrow(diabetes)), size = 0.8 * nrow(diabetes))
training_data <- diabetes[sample_index, ]
test_data <- diabetes[-sample_index, ]

# Fit Logistic Regression (M1)
model_M1 <- glm(Outcome ~ ., data = training_data, family = binomial)

# Summary of the model
summary(model_M1)

# Interpretation: Significant predictors
# Look at the p-values from the summary. Predictors with p-value < 0.05 are significant.

# Predict probabilities on test data
predicted_probs <- predict(model_M1, newdata = test_data, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

# Confusion matrix
table(Predicted = predicted_classes, Actual = test_data$Outcome)

# Accuracy
accuracy <- mean(predicted_classes == test_data$Outcome)
cat("Accuracy:", accuracy, "\n")

# F1-score
library(caret)
f1_score <- F_meas(as.factor(predicted_classes), as.factor(test_data$Outcome))
cat("F1-Score:", f1_score, "\n")

# Fit Logistic Regression (M2) with selected predictors
model_M2 <- glm(Outcome ~ Pregnancies + Glucose + BMI, data = training_data, family = binomial)

# Deviance comparison
anova(model_M2, model_M1, test = "Chisq")

# Interpretation: If the p-value from the test is less than 0.05,
# M1 is significantly more informative than M2.

