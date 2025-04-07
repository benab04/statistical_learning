# Reset all plot settings
plot.new() 
dev.off()

# Reading csv file
data <- read.csv("C:/Users/benab/OneDrive - iitkgp.ac.in/Desktop/Sem 6/SL Lab/Lab 2/manufacturing.csv")
head(data, 6)
# Checking for NaN values
data[apply(is.na(data), 1, any), ] 
# Show column names
names(data)

# Compute correlation matrix and plot it
corr_matrix <- cor(data,method="pearson",use="complete.obs")
library(corrplot)
corrplot(corr_matrix, method = "square", type = "full", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

# Create a linear model to predict Quality Rating 
model1 <- lm(Quality.Rating ~ Temperature...C. + Pressure..kPa. +Material.Fusion.Metric +Material.Transformation.Metric , data =data)
summary(model1)

# Compute residuals
residuals <- residuals(model1)

qqnorm(residuals, main = "Q-Q Plot of Residuals")  # Q-Q plot
qqline(residuals, col = "blue", lwd = 1)           # normality line

# Plot model characteristics
par(mfrow = c(2,2))
plot(model)

par(mfrow = c(1, 1))

set.seed(34)
# Create training and testing data sets
train <- sample(nrow(data), 0.8 *nrow(data)) # Sample 80% of data to be training
train_data <- data[train,]
test_data <- data[-train, ] # 20% of data kept for testing

# Fit a model with training data
fit1 <- lm(Quality.Rating ~ Temperature...C. + Pressure..kPa. + Material.Fusion.Metric +Material.Transformation.Metric , data =train_data)
summary(fit1)

# Get predicted values
pred <- predict(fit1, newdata =test_data)
# Compute RMSE
sqrt(mean((test_data$Quality.Rating - pred)^2))

# Get range of the Quality rating in the data
range(data$Quality.Rating)

# Plot the actual vs predicted values
plot(test_data$Quality.Rating, pred, 
     main = "Actual vs Predicted Quality Rating", 
     xlab = "Actual Quality Rating", 
     ylab = "Predicted Quality Rating", 
     col = "blue", pch = 19)
abline(0, 1, col = "red", lwd = 2)  



