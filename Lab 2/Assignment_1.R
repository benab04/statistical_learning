plot.new()
dev.off()

data <- read.csv("C:/Users/benab/OneDrive - iitkgp.ac.in/Desktop/Sem 6/SL Lab/Lab 2/manufacturing.csv")
head(data, 6)
names(data)


corr_matrix <- cor(data,method="pearson",use="complete.obs")
library(corrplot)
corrplot(corr_matrix, method = "square", type = "full", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

cor.test(data$Material.Fusion.Metric, data$Material.Transformation.Metric, use="complete.obs", method="pearson")
cor.test(data$Temperature...C., data$Material.Fusion.Metric, use="complete.obs", method="pearson")
cor.test(data$Temperature...C., data$Material.Transformation.Metric, method="pearson", use="complete.obs")
cor.test(data$Pressure..kPa., data$Temperature.x.Pressure, method="pearson", use="complete.obs")
cor.test(data$Temperature.x.Pressure, data$Material.Fusion.Metric, method="pearson", use="complete.obs")

plot(data$Temperature.x.Pressure, data$Material.Fusion.Metric)
plot(data$Material.Fusion.Metric, data$Material.Transformation.Metric)
pairs(data)


model1 <- lm(Quality.Rating ~ Temperature...C. + Pressure..kPa. +Material.Fusion.Metric +Material.Transformation.Metric , data =data)
summary(model1)

residuals <- residuals(model1)
qqnorm(residuals, main = "Q-Q Plot of Residuals")  # Q-Q plot
qqline(residuals, col = "blue", lwd = 1)           # normality line

par(mfrow = c(2,2))
plot(model)
par(mfrow = c(1, 1))

qqline(residuals)

model2 <- lm(Quality.Rating ~ Temperature...C. +Material.Fusion.Metric +Material.Transformation.Metric , data =data)
summary(model2)
anova(model2)

model3 <- lm(Quality.Rating ~ Temperature...C.   + Material.Transformation.Metric , data =data)
summary(model3)
anova(model3)

res3 <- residuals(model3)
qqnorm(res3)

train <- sample(nrow(data), 0.2 *nrow(data))
train_data <- data[train,]
test_data <- data[-train, ]

fit1 <- lm(Quality.Rating ~ Temperature...C.   + Material.Transformation.Metric , data =train_data)
summary(fit1)

pred <- predict(fit1, newdata =test_data)
sqrt(mean((test_data$Quality.Rating - pred)^2))

range(data$Quality.Rating)


fit2 <- lm(Quality.Rating ~ Temperature...C. + Pressure..kPa. +Material.Fusion.Metric +Material.Transformation.Metric , data =train_data)
summary(fit2)

pred <- predict(fit2, newdata =test_data)
sqrt(mean((test_data$Quality.Rating - pred)^2))

