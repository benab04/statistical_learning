library(MASS)
library(ISLR2)
?Boston
is.na(Boston)
names(Boston)
nrow(Boston)

fit1 <- lm(medv ~ lstat, data= Boston)
summary(fit1)
anova(fit1)
dim(Boston) # Show dimensions of dataset

#Create training and testing datasets
train <- sample(nrow(Boston), 404) # Get 404 random values (80%) from 506 
train_boston <- Boston[train ,] # Store the rows to the training dataset
test_boston <- Boston[-train, ]

fit2 <- lm(medv ~ . , data = train_boston) # Fit the training dataset on a linear model
summary(fit2)

#Create a new dummy dataset from test_boston by removing medv column
# subset(test_boston, select = c( zn, indus, chas)) makes it select only these columns
dummy <- subset(test_boston, select = -medv)
#Predict the medv values for the dummy dataset
pred <- predict( fit2, newdata = dummy)
# Calculate MSE for predicted and actual values in test dataset
mean((test_boston$medv - pred)^2)

#Predict the medv for new values 5, 10, 15 based on the fitted model
predict(fit1, data.frame(lstat = c(5,10,15))) 
#To display the prediction along with the confidence interval
predict(fit1, data.frame(lstat = c(5,10,15)), interval ="confidence")

