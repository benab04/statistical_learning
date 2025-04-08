library(ISLR)
library(dplyr)
library(ggplot2)
library(splines)

LungCapData2 <- read.csv("C:/Users/benab/OneDrive - iitkgp.ac.in/Desktop/Sem 6/SL Lab/Lab 7/LungCapData2.csv")

# Plot the data
plot(LungCapData2$Age, LungCapData2$LungCap, col = "gray",
     xlab = "Age", ylab = "LungCap", main = "Piecewise Polynomial Regression (Disjoint at Knot)")

range(LungCapData2$Age)
# Define the knot
knot <- 12

# Split the data into two subsets
data_before_knot <- subset(LungCapData2, Age < knot)
data_after_knot <- subset(LungCapData2, Age >= knot)

# Fit a cubic polynomial for Age < 12
model_before <- lm(LungCap ~ poly(Age, degree = 3, raw = TRUE), data = data_before_knot)

# Fit a cubic polynomial for Age >= 12
model_after <- lm(LungCap ~ poly(Age, degree = 3, raw = TRUE), data = data_after_knot)

# Generate Age values for prediction (before and after the knot)
age_before <- seq(min(LungCapData2$Age), knot, length.out = 100)
age_after <- seq(knot, max(LungCapData2$Age), length.out = 100)

# Predict LungCap for the Age values before and after the knot
pred_before <- predict(model_before, newdata = list(Age = age_before))
pred_after <- predict(model_after, newdata = list(Age = age_after))

# Add the curve for Age < 12
lines(age_before, pred_before, col = "red", lwd = 2)

# Add the curve for Age >= 12
lines(age_after, pred_after, col = "blue", lwd = 2)

# Add a vertical line at the knot
abline( v = knot, col="black", lty=2)

# Add a legend
legend("topleft", legend = c("Age < 12", "Age >= 12", "knot at Age = 12"), 
       col = c("red", "blue", "black"), lty = c(1, 1, 2), lwd = 2, cex=0.65)

# Fit a cubic spline with a knot at Age = 12
library(splines)
model_spline <- lm(LungCap ~ bs(Age, knots = knot, degree = 3), data = LungCapData2)

# Generate a sequence of Age values for prediction
age_grid <- seq(min(LungCapData2$Age), max(LungCapData2$Age), length.out = 100)

# Generate predictions for the spline
pred_spline <- predict(model_spline, newdata =list(Age= age_grid))

# Add the spline curve to the plot
lines(age_grid, pred_spline, col = "green", lwd = 2)

# Update the legend
legend("topleft", 
       legend = c("Age < 12", "Age >= 12", "Cubic Spline", "Knot at Age = 12"), 
       col = c("red", "blue", "green", "black"), 
       lty = c(1, 1, 1, 2), 
       lwd = 2, 
       cex=0.65)

# Splines with several disjoints

# Fit a regression spline using basis functions
fit <- lm(LungCap ~ bs(Age, knots = c(5, 10, 15)), data = LungCapData2)
summary(fit)

# Generate a sequence of Age values for prediction
# Get min/max values of Age using the range() function
agelims <- LungCapData2 %>%
  select(Age) %>%
  range

agelims

# Generate a sequence of Age values for prediction
age_grid = seq(from = min(agelims), to = max(agelims), length.out = 100)

# Ensure correct column name in new data
pred = predict(fit, newdata = data.frame(Age = age_grid), se = TRUE)


# Compute error bands correctly
se_bands = with(pred, cbind("upper" = fit + 2 * se.fit,
                            "lower" = fit - 2 * se.fit))

# se_bands = cbind("upper" = pred$fit + 2*pred$se.fit,
#                  "lower" = pred$fit - 2*pred$se.fit)
# Create a data frame for plotting
plot_data <- data.frame(
  Age = age_grid,
  Fit = with(pred, fit),
  Lower = se_bands[, "lower"],
  Upper = se_bands[, "upper"]
)

# Plot the spline and error bands
ggplot() +
  geom_point(data = LungCapData2, aes(x = Age, y = LungCap)) +
  geom_line(data = plot_data, aes(x = Age, y = Fit), color = "blue") +
  geom_ribbon(data = plot_data, aes(x = Age, ymin = Lower, ymax = Upper), alpha = 0.3) +
  xlim(range(LungCapData2$Age))

