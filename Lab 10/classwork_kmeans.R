install.packages("clue")
install.packages("factoextra")
install.packages("mclust")

# Load required libraries
library(ggplot2)
library(clue)      # For Hungarian algorithm (cluster-label matching)
library(mclust)    # For Adjusted Rand Index (ARI)
library(factoextra) # For enhanced visualization

data(iris)
head(iris)

# Remove Species column (store for later evaluation)
true_labels <- as.numeric(iris$Species)  # Convert to numeric (1,2,3)
iris_features <- iris[, -5]              # Features only

# Min - max scaling
min_max_scale <- function(x) {
  (x-min(x)) /(max(x)-min(x))
}

iris_scaled <- as.data.frame(lapply(iris_features, min_max_scale))

head(iris_scaled)


# Find optimal K (scree plot)

# Compute total within-cluster sum of squares (WSS) for k=1 to 10
wss <- sapply(1:10, function(k) {
  set.seed(123)  # Reproducibility
  kmeans(iris_scaled, k, nstart = 25)$tot.withinss
})

# Plot scree plot (elbow method)
plot(1:10, wss, type = "b", pch = 1,
     xlab = "Number of Clusters (k)",
     ylab = "Total within-Cluster Sum of Squares",
     main = "Scree Plot for Optimal K")
abline(v=3, lty =2, col="red")


# K-Means Clustering (K=3)
set.seed(123)
k <- 3
kmeans_result <- kmeans(iris_scaled, centers = k, nstart = 25)

# Evaluate Clustering vs True Labels

# Align clusters to true labels (Hungarian algorithm)
confusion <- table(kmeans_result$cluster, true_labels)

confusion

mapping <- solve_LSAP(confusion, maximum = TRUE)

aligned_clusters <- as.numeric(factor(kmeans_result$cluster, levels = as.integer(mapping)))

# Confusion Matrix
confusion_matrix <- table(Predicted = aligned_clusters, Actual = true_labels)

print(confusion_matrix)

# Accuracy Calculation
accuracy <- sum(aligned_clusters == true_labels) / length(true_labels)
cat("\nAccuracy:", round(accuracy * 100, 2), "%\n")

# Adjusted Rand Index (ARI)
ari <- adjustedRandIndex(aligned_clusters, true_labels)
cat("Adjusted Rand Index (ARI):", round(ari, 3), "\n")


#PCA WITH K-MEANS CLUSTERING

pca_result <- prcomp(iris_scaled)

var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
var_explained

fviz_eig(pca_result, addlabels = TRUE,
         main = "PCA Scree Plot (Variance per Component)")

# Get cumulative variance manually
cum_var <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))
cum_var

plot(1:length(cum_var), cum_var,
     type = "b",  # Both points and lines
     pch = 19,    # Solid circle points
     col = "blue",
     xlab = "Principal Components",
     ylab = "Cumulative Variance",
     main = "Cumulative Variance Explained",
     ylim = c(0, 1.1),
     xaxt = "n")  # Remove default x-axis

# Add custom x-axis
axis(1, at = 1:length(cum_var))

# Add 99% threshold line
abline(h = 0.95, col = "red", lty = 2)

# Add percentage labels
text(1:length(cum_var), cum_var,
     labels = paste0(round(cum_var*100, 0), "%"),
     pos = 3, col = "blue")  # pos=3 puts text above points

