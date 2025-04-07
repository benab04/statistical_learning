install.packages("rpart")
install.packages("rpart.plot")

library(catdata)       # provide heart dataset
library(rpart)         # building decision tree
library(rpart.plot)    # visualizing decision tree
library(MLmetrics)     # calculate performance metrics

## CLASSIFICATION

# Heartdata
data(heart)
?heart
Heartdata <- as.data.frame(heart)  # convert dataset into data.frame
Heartdata

# Splitting of data
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(Heartdata), replace = TRUE, prob = c(0.8, 0.2))
train <- Heartdata[sample, ]
test <- Heartdata[!sample, ]

# Training the decision tree model
tree.heart <- rpart(y ~ sbp + tobacco + ldl + adiposity + factor(famhist) + typea + obesity + alcohol + age, 
                    data = train, 
                    method = "class")

summary(tree.heart)
tree.heart
rpart.plot(tree.heart, cex = 0.6)


# Confusion Matrix
ypred <- predict(tree.heart, test, type = 'class')
ypred
table(predict = ypred, truth = test$y)

# Metrics
ConfusionMatrix(ypred, test$y)
Accuracy(ypred, test$y)
Precision(ypred, test$y)
Recall(ypred, test$y)
F1_Score(ypred, test$y)


# Pruning
plotcp(tree.heart)  # Plot complexity parameter (CP) vs error
tree.heart$cptable  # Get CP table
index <- which.min(tree.heart$cptable[, "xerror"])  # Find optimal CP index
index
cpopt <- tree.heart$cptable[index, "CP"]  # Optimal CP value
cpopt
opttree.heart <- prune(tree.heart, cp = cpopt)
rpart.plot(opttree.heart)

# Confusion Matrix
ypred <- predict(opttree.heart, test, type = 'class')
ypred
table(predict = ypred, truth = test$y)

# Metrics
ConfusionMatrix(ypred, test$y)
Accuracy(ypred, test$y)
Precision(ypred, test$y)
Recall(ypred, test$y)
F1_Score(ypred, test$y)
