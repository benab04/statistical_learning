#############################
# SETUP & DATA LOADING
#############################


# Load required libraries
library(boot)      # for cross-validation and bootstrap
library(ggplot2)   # for plotting

# For reproducibility
set.seed(123)

# Load the dataset
data <- read.csv("C:/Users/benab/OneDrive - iitkgp.ac.in/Desktop/Sem 6/SL Lab/Lab 2/manufacturing.csv", header = TRUE, stringsAsFactors = FALSE)


head(data, 10)
dim(data)

# (Optional) Rename columns if necessary for easier handling:
# Here we assume the file has the following columns:
# "Temperature (Â°C)", "Pressure (kPa)", "Temperature x Pressure",
# "Material Fusion Metric", "Material Transformation Metric", "Quality Rating"
# Let's rename them:
names(data) <- c("Temperature", "Pressure", "Temp_x_Press", 
                 "MatFusion", "MatTransform", "Quality")

data_sample <- data[sample(nrow(data), 100, replace = FALSE), ]

head(data_sample)
#############################
# PART 1: POLYNOMIAL MODELS: QUALITY ~ TEMPERATURE
#############################

# We will fit polynomial models using Temperature as predictor
# with degrees from 1 to 5. For each model, we perform LOOCV,
# 5-fold CV and 10-fold CV to estimate the mean squared error (MSE).

# Create vectors to store CV errors for each degree and method
degrees <- 1:5
cv.error.loocv <- rep(0, length(degrees))
cv.error.5fold <- rep(0, length(degrees))
cv.error.10fold <- rep(0, length(degrees))

# Loop over polynomial degrees
for (i in degrees) {
  start_time <- Sys.time()
  cat("Starting polynomial degree:", i, "\n")
  
  # Fit the GLM with raw polynomial terms
  glm.fit <- glm(Quality ~ poly(Temperature, i, raw = TRUE), data = data_sample)
  
  # LOOCV (or a test version with fewer folds if data is huge)
  cv.loocv <- cv.glm(data_sample, glm.fit, K = nrow(data_sample))
  cv.error.loocv[i] <- cv.loocv$delta[1]
  
  # 5-fold CV
  cv.5 <- cv.glm(data_sample, glm.fit, K = 5)
  cv.error.5fold[i] <- cv.5$delta[1]
  
  # 10-fold CV
  cv.10 <- cv.glm(data_sample, glm.fit, K = 10)
  cv.error.10fold[i] <- cv.10$delta[1]
  
  end_time <- Sys.time()
  cat("Finished degree:", i, "in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

# Combine results in a table
poly.cv.results <- data.frame(
  Degree = degrees,
  LOOCV_MSE = cv.error.loocv,
  CV5_MSE = cv.error.5fold,
  CV10_MSE = cv.error.10fold
)
print("CV Errors for Polynomial Models (Quality ~ Temperature):")
print(poly.cv.results)

# Plot the CV errors versus polynomial degree
# (Plotting all three CV errors on the same plot)
plot(degrees, cv.error.loocv, type = "o", col = "blue", pch = 16,
     xlab = "Polynomial Degree", ylab = "MSE",
     main = "CV Errors for Polynomial Models (Quality ~ Temperature)")
lines(degrees, cv.error.5fold, type = "o", col = "red", pch = 17)
lines(degrees, cv.error.10fold, type = "o", col = "darkgreen", pch = 18)
legend("topright", legend = c("LOOCV", "5-Fold", "10-Fold"),
       col = c("blue", "red", "darkgreen"), pch = c(16,17,18), lty = 1)

# Discussion:
# Compare the table/plot to see which degree minimizes the CV error.
# For instance, if degree 2 or 3 yields the lowest errors across methods,
# that polynomial degree is preferable.

#############################
# PART 2: LINEAR MODELS WITH DIFFERENT COMBINATIONS OF PREDICTORS
#############################

# We now compare different linear models (without interactions) to predict Quality.
# The available predictors are:
#   Temperature, Pressure, Temp_x_Press, MatFusion, MatTransform
#
# We will consider a few models with different combinations:
#model.formulas <- list(
  #"Temperature" = Quality ~ Temperature,
  #"Temperature-Pressure" = Quality ~ Temperature + Pressure,
  #"Temp-Pressure-MatFusion" = Quality ~ Temperature + Pressure + MatFusion,
  #"Temp-Pressure-MatFusion-MatTransform" = Quality ~ Temperature + Pressure + MatFusion + MatTransform,
  #"All Predictors" = Quality ~ Temperature + Pressure + Temp_x_Press + MatFusion + MatTransform
#)

predictors <- c("Temperature", "Pressure", "Temp_x_Press", "MatFusion", "MatTransform")

# Initialize an empty list to hold formulas
model.formulas <- list()

# Generate all possible predictor combinations
for (i in 1:length(predictors)) {
  cmb <- combn(predictors, i, simplify = FALSE)
  for (combo in cmb) {
    formula_str <- paste("Quality ~", paste(combo, collapse = " + "))
    formula_obj <- as.formula(formula_str)
    model.formulas[[formula_str]] <- formula_obj
  }
}

# Number of models
n.models <- length(model.formulas)

# Initialize storage vectors for LOOCV, 5-fold, and 10-fold errors
cv.error.loocv <- rep(0, n.models)
cv.error.5fold <- rep(0, n.models)
cv.error.10fold <- rep(0, n.models)

model.names <- names(model.formulas)

# Loop through models and compute CV errors
for (i in 1:n.models) {
  start_time <- Sys.time()
  cat("Starting model", i, ":", model.names[i], "\n")
  
  # Fit model
  glm.fit <- glm(model.formulas[[i]], data = data_sample)
  
  # LOOCV (default behavior)
  cv.loocv <- cv.glm(data_sample, glm.fit)
  cv.error.loocv[i] <- cv.loocv$delta[1]
  
  # 5-fold CV
  cv.5 <- cv.glm(data_sample, glm.fit, K = 5)
  cv.error.5fold[i] <- cv.5$delta[1]
  
  # 10-fold CV
  cv.10 <- cv.glm(data_sample, glm.fit, K = 10)
  cv.error.10fold[i] <- cv.10$delta[1]
  
  # Time tracking
  end_time <- Sys.time()
  duration <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 2)
  cat("Finished model", i, "in", duration, "seconds. LOOCV MSE =", cv.error.loocv[i], 
      "5-fold MSE =", cv.error.5fold[i], "10-fold MSE =", cv.error.10fold[i], "\n\n")
}

# Combine results into a table
combination.cv.results <- data.frame(
  Model = model.names,
  LOOCV_MSE = cv.error.loocv,
  CV5_MSE = cv.error.5fold,
  CV10_MSE = cv.error.10fold
)

# Print results
print("Cross-Validation Errors for Different Predictor Combinations:")
print(combination.cv.results)

# Reshape data for visualization
library(reshape2)
cv.melted <- melt(combination.cv.results, id.vars = "Model", variable.name = "CV_Type", value.name = "MSE")

# Barplot
ggplot(cv.melted, aes(x = Model, y = MSE, fill = CV_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cross-Validation MSE for Different Predictor Combinations",
       x = "Predictor Combination",
       y = "MSE") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Discussion:
# Examine the table/plot to select the model with the smallest LOOCV error,
# which is the most preferable based on these results.

#############################
# PART 3: BOOTSTRAP ESTIMATION OF MEAN & VARIANCE
#############################

# We now generate 50 random numbers from N(μ = 50, σ^2 = 2).
# (Recall: standard deviation is sqrt(2))
set.seed(321)
pop_sample <- rnorm(50, mean = 50, sd = sqrt(2))
head(pop_sample)

# Create 100 bootstrap samples, each of size 20 (with replacement)
n_boot <- 100
boot_sample_size <- 20

# Vectors to store bootstrap estimates
boot.means <- rep(0, n_boot)
boot.vars  <- rep(0, n_boot)

set.seed(456)  # new seed for bootstrap sampling

for (i in 1:n_boot) {
  boot.sample <- sample(pop_sample, size = boot_sample_size, replace = TRUE)
  boot.means[i] <- mean(boot.sample)
  boot.vars[i]  <- var(boot.sample)
}

# Bootstrap estimates for the population parameters:
boot.mean.estimate <- mean(boot.means)
boot.var.estimate  <- mean(boot.vars)

cat("Bootstrap Estimation Results (", n_boot, "samples of size", boot_sample_size, "):\n")
cat("Estimated Mean:", boot.mean.estimate, "\n")
cat("Estimated Variance:", boot.var.estimate, "\n")

# Optionally, plot the distributions of bootstrap estimates
par(mfrow = c(1,2))
hist(boot.means, col = "lightblue", main = "Bootstrap Distribution of Mean",
     xlab = "Mean", breaks = 10)
hist(boot.vars, col = "lightgreen", main = "Bootstrap Distribution of Variance",
     xlab = "Variance", breaks = 10)
par(mfrow = c(1,1))

