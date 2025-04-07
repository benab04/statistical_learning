library(ISLR2)
head(Default)

m1 <- glm(default ~., family = binomial, data = Default)
summary(m1)
deviance(m1) # Calculate deviance
logLik(m1)

m2 <- glm(default ~ student + balance, family = binomial, data = Default)
summary(m2)

deviance(m2) - deviance(m1)
qchisq(0.95, 1) # Compute chi square critical value. m=3, p=2.
# deviance(m2) - deviance(m1) < qchisq(0.95, 1), so m2 is better, which is smaller.
library(ggplot2)
library(dplyr)
library(MASS)
library(boot)


data("Default")
head(Default)
cor(Default)
str(Default)

num_data <- Default[, sapply(Default, is.numeric)] # Select only numeric columns
cor_matrix <- cor(num_data, use ="complete.obs")

print(cor_matrix) # Income and balance are negatively correlated

install.packages("corrplot")
library(corrplot)
corrplot(cor_matrix, method="circle", addCoef.col = "black")

# Logistic Regression
glm.fits = glm(default ~ student + balance, data = Default, family = binomial)
# Automatically encodes student to 0 and 1 and creates a new variable studentYes
summary(glm.fits)

coef(glm.fits)
library(stats)
log_likelihood <- logLik(glm.fits)
log_likelihood

summary(glm.fits)$coef 
summary(glm.fits)$coef [,4]
glm.probs = predict(glm.fits, Default, type="response") # type = "response" ensures probabilities (values between 0 and 1)
glm.probs [1:10]
contrasts(Default$default)

glm.pred <- ifelse(glm.probs >0.5 , "Yes", "No") # Convert to binary predictions
table( glm.pred, Default$default)
mean(glm.pred==Default$default)

# Splitting data set
trn  <- sample(dim(Default)[1], 8000)
Default_train <- Default[trn,]
Default_test <- Default [-trn, ]
Default_test <- Default_test[, -1]
head (Default_test)

dim(Default_train)
dim(Default_test)


glm.fit <- glm(default ~ student + balance, data=Default_train, family =binomial)

pred <- predict(glm.fit, Default_test, type="response")
pred [1:8]

pred_class <- ifelse(pred >=0.5, "Yes", "No")
pred_class[1:5]


table(Default[-trn,]$default, pred_class)
mean(pred_class == Default[-trn,]$default)
# precision = True Positives / ( True positives + False positives)

contrasts(Default$default) # Shows Yes will be what and No will be what

?predict.glm

?cv.glm

summary(glm.fit)
glm.fit
head(Default)

attach(Default)
boxplot(balance ~ default)
boxplot(income ~ default)
ggplot( Default, aes(x = default, y = balance, fill=default)) + geom_boxplot()
ggplot( Default, aes(x = income, y = balance, color=default)) + geom_point()

# Contingency table among two categorical variables
table(default, student)



# Multinomial dataset

head(iris)
unique(iris$Species)
library(VGAM)
library(nnet)

m_multi <- multinom(Species ~. , data =  iris)
summary(m_multi)
