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
