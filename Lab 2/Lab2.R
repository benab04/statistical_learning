mtcars
head(mtcars,10)
library(ISLR2)
head(Credit)
names(mtcars) # Column names
?mtcars # See documentation
str(mtcars) # structure of mtcars
unique(mtcars$cyl) # Get set of unique values in cyl column
nrow(mtcars)
ncol(mtcars)
boxplot(mpg~cyl, data=mtcars) 
plot(mtcars$hp, mtcars$mpg) # Scatter plot
pairs(mtcars) # show pairwise plot
m1 <- lm ( mpg ~ disp + hp + drat + wt + qsec, data = mtcars)
summary (m1) # get coefficients and significant factors
aov(m1) # Anova results
anova(m1) # Anova table
resi <- residuals(m1)
resi
qqnorm(resi)
par(mfrow = c(2,2))
plot(m1)
confint(m1) # Confidence interval of the beta values

#Forward selection
null_model <- lm(mpg ~ 1, data=mtcars)
full_model <- lm(mpg ~. , data = mtcars)
summary(null_model)
fwd <- step(null_model, direction = "forward",   scope = formula(full_model))
summary(fwd)

#Backward selection

summary(full_model)

bwd <- step(full_model, direction = "backward", data=mtcars)
summary(bwd)

#Bidirectional selection
both <- step(full_model, direction="both", data=mtcars)
summary(both)
