## Basic stats commands

- duplicated(airquality)
- mean(airquality$Ozone)
- mean(airquality$Ozone,na.rm=TRUE) # .rm is to remove na values
- median(airquality$Ozone)
- var(airquality$Ozone)
- quantile(airquality$Ozone)
- range(airquality$Ozone)
- sd(airquality$Ozone)
- IQR(airquality$Ozone)
- cor(airquality,airquality,method="pearson",use="complete.obs")
- cor.test(airquality$Ozone,airquality$Wind,method="pearson",use="complete.obs")

## Basic operations

- dim(data)
- nrow(data)
- str(mtcars) # get structure
- is.na(airquality$Ozone)
- colnames(airquality) || names(airquality) for column names
- summary(airquality)
- airquality$Ozone[is.na(airquality$Ozone)]<-mean(airquality$Ozone,na.rm=TRUE)

#### converting to factor

- diabetes$Outcome <- factor(diabetes$Outcome, labels = c("Non-Diabetic", "Diabetic"))

## reading csv

- data <- read.csv("C:/Users/benab/OneDrive - iitkgp.ac.in/Desktop/Sem 6/SL Lab/Lab 2/manufacturing.csv")

## correlation matrix

- corr_matrix <- cor(data,method="pearson",use="complete.obs")
- library(corrplot)
- corrplot(corr_matrix, method = "square", type = "full",
  tl.col = "black", tl.srt = 45, addCoef.col = "black")

  If without target variable

- correlation_matrix <- cor(diabetes[, -ncol(diabetes)])

## scatter and box plots

#### scatter plots

- ggpairs(diabetes[, -9], aes(color = diabetes$Outcome, alpha = 0.5)) +
  ggtitle("Pairs Plot of Diabetes Variables")

#combined scatter plots for multiple classes

- pairs(diabetes[,-9],
  col=diabetes$Outcome + 1,
  pch=16,
  main="Scatterplot Matrix of all Variables")

#### box plots

#Box plot: BMI by Outcome

- ggplot(diabetes, aes(x = as.factor(Outcome), y = BMI)) +
  geom_boxplot() +
  ggtitle("Box Plot: BMI by Outcome") +
  xlab("Outcome") +
  ylab("BMI")

#all box plots

- ggplot(gather(diabetes, key = "Predictor", value = "Value", -Outcome),
  aes(x = Outcome, y = Value, fill = Outcome)) +
  geom_boxplot() +
  facet_wrap(~ Predictor, scales = "free") +
  theme_minimal()

## Feature selection

- null_model <- lm(mpg ~ 1, data=mtcars)
- full_model <- lm(mpg ~. , data = mtcars)

#Forward selection

- fwd <- step(null_model, direction = "forward", scope = formula(full_model))
- summary(fwd)

#Backward selection

- bwd <- step(full_model, direction = "backward", data=mtcars)
- summary(bwd)

#Bidirectional selection

- both <- step(full_model, direction="both", data=mtcars)
- summary(both)

## Linear regression

- model1 <- lm(Quality.Rating ~ Temperature...C. + Pressure..kPa. +Material.Fusion.Metric +Material.Transformation.Metric , data =data)

#### residuals

- residuals <- residuals(model1)
- qqnorm(residuals, main = "Q-Q Plot of Residuals") # Q-Q plot
- qqline(residuals, col = "blue", lwd = 1) # normality line

#### splitting

- train <- sample(nrow(data), 0.2 \* nrow(data))
- train_data <- data[train,]
- test_data <- data[-train, ]

- split <- sample.split(diabetes$Outcome, SplitRatio = 0.8)
- train <- subset(diabetes, split == TRUE)
- test <- subset(diabetes, split == FALSE)

#### predict

- pred <- predict(fit1, newdata =test_data)
- sqrt(mean((test_data$Quality.Rating - pred)^2)) # rmse

#### anova

- anova(model)

#### confidence intervals

- confint(model, level = 0.95)

## Logistic regression

- model_M1 <- glm(Outcome ~ ., data = training_data, family = binomial)

#### predict

- predicted_probs <- predict(model_M1, newdata = test_data, type = "response")
- predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

#### Confusion matrix

#Confusion matrix

- table(Predicted = predicted_classes, Actual = test_data$Outcome)

#### model comparison

- anova(model_M2, model_M1, test = "Chisq")

#### model results

- accuracy <- mean(predicted_classes == test_data$Outcome)

#F1-score

- library(caret)
- f1_score <- F_meas(as.factor(predicted_classes), as.factor(test_data$Outcome))

## LDA

- lda_model <- lda(Outcome ~ ., data = train)
- lda_pred <- predict(lda_model, newdata = test)

- results <- data.frame(
  Actual = test$Outcome,
  Predicted = lda_pred$class
  )

#### Plot actual vs predicted values

- ggplot(results, aes(x = Actual, y = Predicted, color = Actual)) +
  geom_jitter(width = 0.2, height = 0.2, size = 2, alpha = 0.7) +
  labs(title = "Actual vs Predicted Diabetes Outcome (LDA)",
  x = "Actual Outcome",
  y = "Predicted Outcome",
  color = "Outcome") +
  scale_color_manual(values = c("Non-Diabetic" = "blue", "Diabetic" = "red")) +
  theme_minimal()

### QDA Model

- qda_model <- qda(Outcome ~ ., data = train)
- qda_pred <- predict(qda_model, newdata = test)

- confusion_qda <- confusionMatrix(qda_pred$class, test$Outcome, positive ="Diabetic")

### KNN model

- train_scale <- scale(train[, -9])
- test_scale <- scale(test[, -9])

- knn_pred <- knn(train_scale, test_scale,
  cl = train$Outcome, k = 5)

- confusion_knn <- confusionMatrix(knn_pred, test$Outcome, positive ="Diabetic")

##### Selecting best K

- set.seed(42)
- k_to_try = 1: 100
- acc_k = rep(x=0, times=length(k_to_try))

- for(i in seq_along(k_to_try)){
  pred=knn(train = scale(X_default_train),
  test = scale(X_default_test),
  cl = y_default_train,
  k = k_to_try[i]
  )
  cm= confusionMatrix(pred, y_default_test)
  acc_k[i] = cm$overall["Accuracy"]
  }

- max_accuracy <- max(acc_k)
- best_k <- k_to_try[which.max(acc_k)]

- plot(k_to_try, acc_k, type = "b", col = "blue", pch = 19,
  xlab = "Number of Neighbors (k)", ylab = "Accuracy",
  main = "Accuracy vs k in k-NN")
- abline(v = best_k, col = "red", lty = 2) # Add vertical line at best_k
- text(best_k, max_accuracy,
  labels = paste("Best k =", best_k), pos = 3, col = "red")

#### Metrics

- metrics <- data.frame(
  Model = c("LDA", "QDA", "KNN (k=5)"),
  Accuracy = c(accuracy_lda,
  confusion_qda$overall['Accuracy'],
               confusion_knn$overall['Accuracy']),
  F1_Score = c(f1_lda,
  confusion_qda$byClass['F1'],
               confusion_knn$byClass['F1'])
  )

## Cross Validation

- degrees <- 1:5
- cv.error.loocv <- rep(0, length(degrees))
- cv.error.5fold <- rep(0, length(degrees))
- cv.error.10fold <- rep(0, length(degrees))

- for (i in degrees) {

  - glm.fit <- glm(Quality ~ poly(Temperature, i, raw = TRUE), data = data_sample)
    #LOOCV
  - cv.loocv <- cv.glm(data_sample, glm.fit, K = nrow(data_sample))
  - cv.error.loocv[i] <- cv.loocv$delta[1]
    #5-fold CV
  - cv.5 <- cv.glm(data_sample, glm.fit, K = 5)
    cv.error.5fold[i] <- cv.5$delta[1]
    #10-fold CV
  - cv.10 <- cv.glm(data_sample, glm.fit, K = 10)
  - cv.error.10fold[i] <- cv.10$delta[1]

  }

- poly.cv.results <- data.frame(
  Degree = degrees,
  LOOCV_MSE = cv.error.loocv,
  CV5_MSE = cv.error.5fold,
  CV10_MSE = cv.error.10fold
  )

### Bootstrapping

- set.seed(789)
- pop_sample <- rnorm(50, mean = 50, sd = sqrt(2))

- n_boot <- 100
- boot_sample_size <- 20

- boot.means <- rep(0, n_boot)
- boot.vars <- rep(0, n_boot)

- set.seed(456)

- for (i in 1:n_boot) {

  - boot.sample <- sample(pop_sample, size = boot_sample_size, replace = TRUE)
  - boot.means[i] <- mean(boot.sample)
  - boot.vars[i] <- var(boot.sample)
    }

- boot.mean.estimate <- mean(boot.means)
- boot.var.estimate <- mean(boot.vars)

- par(mfrow = c(1,2))
- hist(boot.means, col = "lightblue", main = "Bootstrap Distribution of Mean",
  xlab = "Mean", breaks = 10)
- hist(boot.vars, col = "lightgreen", main = "Bootstrap Distribution of Variance",
  xlab = "Variance", breaks = 10)
