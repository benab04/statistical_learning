LungCapData2 <- read.csv("C:/Users/benab/OneDrive - iitkgp.ac.in/Desktop/Sem 6/SL Lab/Lab 7/LungCapData2.csv")
head(LungCapData2)
View(LungCapData2)
summary(LungCapData2)
attach(LungCapData2)

plot(Age, LungCap, main="Relation", las=1)

model1 <- lm(LungCap ~ Age)
summary(model1)
lines(smooth.spline(Age, predict(model1)), col="yellow", lwd=3)
#pred_values =predict(model1)

#pred_values
##Fitting a polynomial regression model with degree 2
model2 <- lm(LungCap ~ Age + I(Age^2))
summary(model2)
lines(smooth.spline(Age, predict(model2)), col="blue", lwd=3)

##Fitting a polynomial regression model with degree 3
model3 <- lm(LungCap ~ Age + I(Age^2) +I(Age^3))
summary(model3)
lines(smooth.spline(Age, predict(model3)), col="red", lwd=3)


model4 <- lm(LungCap ~ poly(Age, degree = 4, raw=1))
summary(model4)
lines(smooth.spline(Age, predict(model4)), col="green", lwd=3)

# Testing for degree 100
model5 <- lm(LungCap ~ poly(Age, degree = 100, raw=1))
summary(model5)
lines(smooth.spline(Age, predict(model5)), col="purple", lwd=3)


legend("topleft", legend =c("Degree 1", "Degree 2","Degree 3", "Degree 4", "Degree 100"),
       fill=c( "yellow","blue", "red", "green","purple") ,
       cex = 0.65)


# Checking if increasing the polynomial degree effectively explains the variance
# Anova for model 1 and model 2
anova(model1,model2)

# Anova for model 1 and model 5
anova(model1,model5)
par(mfrow = c(1, 1))  # Sets the plotting layout to 1 row, 1 column


