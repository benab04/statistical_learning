library(ISLR)
library(ggplot2)
library(boot)

set.seed(2)

head(Auto)
dim(Auto)

train = sample(392,196)
Auto.tr <- Auto[train,]
Auto.test <- Auto[-train]
dim(Auto.tr)

lm.fit <- lm(mpg ~ horsepower, data =Auto , subset =train)
summary(lm.fit)

pred <- predict(lm.fit, newdata = Auto[-train] )

mse <- mean((Auto.test$mpg - pred)^2)
mse

attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train]^2)

# Cross validation
?cv.glm
glm.auto <- glm(mpg~ horsepower, data= Auto)
summary(glm.auto)

cv.auto <- cv.glm(Auto, glm.auto, K=5)
cv.auto$delta

mpg - predict(lm.fit, Auto)[-train]


# Leave one out cross validation

# Fit GLM and linear models
glm.fit <- glm(mpg ~ horsepower, data = Auto)
coef(glm.fit)
lm.fit <- lm(mpg ~ horsepower, data = Auto)
coef(lm.fit)

# Load boot library and perform cross-validation
library(boot)
glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta

# Initialize vector for CV errors
cv.error <- rep(0, 5)

# Perform cross-validation for polynomial models
for (i in 1:5) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}

# Plot results
plot(seq(1,5), cv.error, xlab = "Model Order", ylab = "MSE", 
     type = "o", col = "blue", pch = 16)


#k -fold cross validation
set.seed(17)
cv.error.10 = rep(0,10)

for(i in 1:10){
  glm.fit =glm(mpg~poly(horsepower, i), data=Auto)
  cv.error.10[i]=cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10
plot(seq(1,10), cv.error.10, xlab ="Model Order", ylab="MSE", type="o", pch=8)


# Bootstrap
boot.fn <- function(data, index){
  fit <- lm (mpg ~ horsepower, data=data, subset =index)
  return (coef(fit))
}

results <- boot(Auto, boot.fn, R=1000)
print(results)

par(mfrow = c(1,2))
hist(results$t[,1], main = "Bootstrap Distribution of Intercept", xlab ="Intercpet")
hist(results$t[,2], main ="Bootstrap Distribution of Slope", xlab ="Slope")
