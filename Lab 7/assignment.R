library(ggplot2)
library(GGally)
library(splines)
library(mgcv)
library(tidyr)
library(dplyr)


file_path="C:/Users/benab/OneDrive - iitkgp.ac.in/Desktop/Sem 6/SL Lab/Lab 7/STAT501_Lesson01/STAT501_Lesson01/poverty.txt"
poverty_df  <- read.table(file_path, header = TRUE, sep = "\t")

head(poverty_df )
str(poverty_df)
summary(poverty_df)


sum(is.na(poverty_df))

poverty_df$Location <- as.factor(poverty_df$Location)
str(poverty_df)
summary(poverty_df)

ggpairs(poverty_df[, 2:6])


ggplot(poverty_df, aes(x = TeenBrth, y = PovPct)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Relationship between Teen Birth Rate and Poverty Percentage",
       x = "Teen Birth Rate", y = "Poverty Percentage")

ggplot(poverty_df, aes(x = ViolCrime, y = PovPct)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Relationship between Violent Crime Rate and Poverty Percentage",
       x = "Violent Crime Rate", y = "Poverty Percentage")

ggplot(poverty_df, aes(x = Brth15to17      , y = PovPct)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Relationship between Violent Crime Rate and Poverty Percentage",
       x = "Violent Crime Rate", y = "Poverty Percentage")

ggplot(poverty_df, aes(x = Brth18to19       , y = PovPct)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Relationship between Violent Crime Rate and Poverty Percentage",
       x = "Violent Crime Rate", y = "Poverty Percentage")

boxplot_data <- poverty_df %>%
  pivot_longer(cols = -Location, names_to = "Variable", values_to = "Value")

ggplot(boxplot_data, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Variable", y = "Value", title = "Boxplot of All Variables") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


lm_model <- lm(PovPct ~ TeenBrth + ViolCrime + Brth15to17 + Brth18to19, data = poverty_df)
summary(lm_model)

poly_model <- lm(PovPct ~ poly(Brth15to17, 2) + poly(Brth18to19, 2) + 
                   poly(ViolCrime, 2) + poly(TeenBrth, 2), data = poverty_df)

summary(poly_model)


anova(lm_model, poly_model)

degree_range <- 2:5

# Store AIC values and R² values
aic_values <- numeric(length(degree_range))
bic_values <- numeric(length(degree_range))
r2_values <- numeric(length(degree_range))

# Loop through each degree
for (i in 1:length(degree_range)) {
  degree <- degree_range[i]
  
  # Fit polynomial regression model with the same degree for all predictors
  model <- lm(PovPct ~ poly(Brth15to17, degree) + 
                poly(Brth18to19, degree) + 
                poly(ViolCrime, degree) + 
                poly(TeenBrth, degree), 
              data = poverty_df)
  
  # Store AIC and R²
  aic_values[i] <- AIC(model)
  bic_values[i] <- BIC(model)
  r2_values[i] <- summary(model)$r.squared
}

# Find the best degree based on minimum AIC
best_aic_index <- which.min(aic_values)
best_aic_degree <- degree_range[best_aic_index]

best_bic_index <- which.min(bic_values)
best_bic_degree <- degree_range[best_bic_index]

# Find the best degree based on maximum R²
best_r2_index <- which.max(r2_values)
best_r2_degree <- degree_range[best_r2_index]

# Print results
cat("Best polynomial degree based on AIC:", best_aic_degree, "\n")
cat("AIC values for degrees 2-5:", aic_values, "\n\n")

cat("Best polynomial degree based on BIC:", best_bic_degree, "\n")
cat("BIC values for degrees 2-5:", bic_values, "\n\n")

cat("Best polynomial degree based on R²:", best_r2_degree, "\n")
cat("R² values for degrees 2-5:", r2_values, "\n")


# ------ PLOT THE BEST AIC MODEL --------
# Arrange data by location
poverty_df <- poverty_df %>%
  arrange(Location)

# Fit the best model using AIC-optimal polynomial degrees
best_aic_model <- lm(PovPct ~ poly(Brth15to17, best_aic_combination$Brth15to17) + 
                       poly(Brth18to19, best_aic_combination$Brth18to19) + 
                       poly(ViolCrime, best_aic_combination$ViolCrime) + 
                       poly(TeenBrth, best_aic_combination$TeenBrth), 
                     data = poverty_df)

anova(best_aic_model)
# Predict values using the best AIC model
poverty_df$Predicted_AIC <- predict(best_aic_model, newdata = poverty_df)


# ------ PLOT THE BEST R² MODEL --------
# Fit the best model using R²-optimal polynomial degrees
best_r2_model <- lm(PovPct ~ poly(Brth15to17, best_r2_combination$Brth15to17) + 
                      poly(Brth18to19, best_r2_combination$Brth18to19) + 
                      poly(ViolCrime, best_r2_combination$ViolCrime) + 
                      poly(TeenBrth, best_r2_combination$TeenBrth), 
                    data = poverty_df)

# Predict values using the best R² model
poverty_df$Predicted_R2 <- predict(best_r2_model, newdata = poverty_df)


# Convert data to long format for ggplot
plot_data <- poverty_df %>%
  select(Location, PovPct, Predicted_AIC, Predicted_R2) %>%
  pivot_longer(cols = c(PovPct, Predicted_AIC, Predicted_R2), names_to = "Type", values_to = "Value")

# Create the scatter plot with smooth connecting curve
ggplot(plot_data, aes(x = Location, y = Value, color = Type, group = Type)) +
  geom_point(size = 3, alpha = 0.8) +  # Scatter points
  geom_line(size = 1) +  # Connecting curve
  scale_color_manual(values = c("PovPct" = "#1D3557",   # Red (Actual)
                                "Predicted_AIC" = "#E63946", # Dark Blue (AIC)
                                "Predicted_R2" = "#2A9D8F"),  # Dark Green (R²)
                     labels = c("Actual", "Predicted (AIC)", "Predicted (R²)")) +
  theme_minimal() +
  labs(title = "Actual vs. Predicted Poverty Percentage by Location",
       x = "Location",
       y = "Poverty Percentage",
       color = "Legend") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))  # Rotate x-axis labels for readability


# Spline model with 3 knots
spline_model_3k <- lm(PovPct~ bs(Brth15to17, knots = quantile(poverty_df$Brth15to17, probs = c(0.25, 0.5, 0.75))) + 
                        bs(Brth18to19 , knots = quantile(poverty_df$Brth18to19 , probs = c(0.25, 0.5, 0.75))) + 
                        bs(ViolCrime , knots = quantile(poverty_df$ViolCrime , probs = c(0.25, 0.5, 0.75)))+
                        bs(TeenBrth , knots = quantile(poverty_df$TeenBrth , probs = c(0.25, 0.5, 0.75))),
                      data = poverty_df)

# Summary of spline model
summary(spline_model_3k)

poverty_df$Spline_Predicted <- predict(spline_model_3k)

# Arrange data by location for better visualization
poverty_df <- poverty_df %>%
  arrange(Location)

# Create a long-format dataset for plotting
plot_data <- poverty_df %>%
  select(Location, PovPct, Spline_Predicted) %>%
  pivot_longer(cols = c(PovPct, Spline_Predicted), 
               names_to = "Type", 
               values_to = "Value")

# Create the plot
ggplot(plot_data, aes(x = Location, y = Value, color = Type, group = Type)) +
  geom_point(size = 3, alpha = 0.8) +  # Scatter points
  geom_line(size = 1) +  # Connecting lines
  scale_color_manual(values = c("PovPct" = "#1D3557",        # Actual values
                                "Spline_Predicted" = "#E63946"), # Predicted values
                     labels = c("Actual", "Spline Predicted")) +
  theme_minimal() +
  labs(title = "Actual vs. Spline Predicted Poverty Percentage by Location",
       x = "Location",
       y = "Poverty Percentage",
       color = "Legend") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))  # Rotate x-axis labels

# Calculate residuals
residuals <- poverty_df$PovPct - poverty_df$Spline_Predicted

# Number of observations and predictors
n <- nrow(poverty_df)
k <- 4  # Number of predictors in the spline model

# Calculate RSS (Residual Sum of Squares)
rss <- sum(residuals^2)

# Calculate AIC and BIC
aic <- n * log(rss/n) + 2 * k
bic <- n * log(rss/n) + k * log(n)

# Calculate R²
r2 <- 1 - (sum(residuals^2) / sum((poverty_df$PovPct - mean(poverty_df$PovPct))^2))

# Print results
cat("AIC:", aic, "\n")
cat("BIC:", bic, "\n")
cat("R²:", r2, "\n")



# Fit a Generalized Additive Model (GAM)
gam_model <- gam(PovPct ~ s(Brth15to17) + s(Brth18to19) + s(ViolCrime) + s(TeenBrth), data = poverty_df)

summary(gam_model)
par(mfrow=c(2,2))
# Visualization of GAM effects
plot(gam_model)


AIC(lm_model, poly_model, spline_model_3k, gam_model)
BIC(lm_model, poly_model, spline_model_3k, gam_model)



