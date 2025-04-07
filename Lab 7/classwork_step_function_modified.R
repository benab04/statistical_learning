LungCapData2 <- read.csv("C:/Users/benab/OneDrive - iitkgp.ac.in/Desktop/Sem 6/SL Lab/Lab 7/LungCapData2.csv")
head(LungCapData2)

min(LungCapData2$Age)
max(LungCapData2$Age)

# Create age categories (step function bins)
LungCapData2$age_cat <- cut(LungCapData2$Age, breaks = c(0, 5, 10, 15, 20))

# Fit a linear regression model with age categories
model <- lm(LungCap ~ age_cat, data = LungCapData2)

# Summarize the model
summary(model)

#Alternative Step Function
table(cut(LungCapData2$Age,4))
fit_step = lm(LungCap~cut(Age,4), data = LungCapData2)
print(coef(summary(fit_step)))
# Predict the value of the generated ages, returning the standard error using se = TRUE
# Get min/max values of age using the range() function
library(dplyr)  

agelims <- LungCapData2 %>%
  select(Age) %>%
  range()

age_grid = seq(from = min(agelims), to = max(agelims))
preds = predict(fit_step, newdata = list(Age = age_grid), se = TRUE)

# Compute error bands (2*SE)
se_bands = cbind("upper" = preds$fit+2*preds$se.fit, 
                 "lower" = preds$fit-2*preds$se.fit)

# Plot
library(ggplot2)
ggplot() +
  geom_point(data = LungCapData2, aes(x = Age, y = LungCap)) +
  geom_line(aes(x = age_grid, y = preds$fit), color = "red") +
  geom_ribbon(aes(x = age_grid, 
                  ymin = se_bands[,"lower"], 
                  ymax = se_bands[,"upper"]), 
              alpha = 0.3) +
  xlim(agelims) +
  labs(title = "Step Function")
