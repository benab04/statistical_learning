library(ISLR)

# Load the Wage dataset
data(Wage)
View(Wage)
min(Wage$age)
max(Wage$age)

# Create age categories (step function bins)
Wage$age_cat <- cut(Wage$age, breaks = c(0, 25, 40, 55, 70))

# Fit a linear regression model with age categories
model <- lm(wage ~ age_cat, data = Wage)

# Summarize the model
summary(model)

#Alternative Step Function
table(cut(Wage$age,4))
fit_step = lm(wage~cut(age,4), data = Wage)
print(coef(summary(fit_step)))
# Predict the value of the generated ages, returning the standard error using se = TRUE
# Get min/max values of age using the range() function
library(dplyr)  

agelims <- Wage %>%
  select(age) %>%
  range()

age_grid = seq(from = min(agelims), to = max(agelims))
preds = predict(fit_step, newdata = list(age = age_grid), se = TRUE)

# Compute error bands (2*SE)
se_bands = cbind("upper" = preds$fit+2*preds$se.fit, 
                 "lower" = preds$fit-2*preds$se.fit)

# Plot
library(ggplot2)
ggplot() +
  geom_point(data = Wage, aes(x = age, y = wage)) +
  geom_line(aes(x = age_grid, y = preds$fit), color = "red") +
  geom_ribbon(aes(x = age_grid, 
                  ymin = se_bands[,"lower"], 
                  ymax = se_bands[,"upper"]), 
              alpha = 0.3) +
  xlim(agelims) +
  labs(title = "Step Function")
