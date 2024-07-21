#==================================================================
#
#
# BIG DATA
# Final Exam
# Matas Mažvila
#
#
#==================================================================
# Load necessary libraries
library(e1071)
library(ISLR)
require(caTools)
require(plotrix)
library(tidyr)
library(knitr)
library(hdm)
library(dplyr)
library(MASS)
library(randomForest)
library(tree)
library(glmnet) 
library(gbm)
library(tm)
library(data.tree)
library(DiagrammeR)
library(rpart)
library(rpart.plot)
library(ape)
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ggtree")
library(ggtree)
library(ggplot2)

# Exercise 3

# Problem 1
# Part a

# Define the tree structure manually using ape
tree_structure <- "((((1:1,2:1):1.5,(1.5:1,2.5:1)A:1)B:1,3.5:2)C:1,3:2)D;"
tree <- ape::read.tree(text = tree_structure)
tree$node.label <- c("X1 < 120", "X2 < 9", "X1 < 70", "X2 < 5", "X2 < 5")

# Plot the tree using ggtree with slight visual adjustments
ggtree(tree, ladderize = FALSE) + 
  geom_tiplab(vjust = 1.8, hjust = 0.6) + 
  geom_text2(aes(label = label, subset = !isTip), hjust = -0.2, vjust = -1.1) +
  scale_x_reverse(expand = expansion(mult = c(0.05, 0.1))) + 
  coord_flip() +
  ggtitle("Binary Split Tree for Mutation Probability")


# Problem 2
# Part a
library(ggdendro)

# Define the dissimilarity matrix
dissimilarity_matrix <- matrix(c(
  0, 0.2, 0.6, 0.4, 0.8,
  0.2, 0, 0.3, 0.5, 0.1,
  0.6, 0.3, 0, 0.6, 0.7,
  0.4, 0.5, 0.6, 0, 0.9,
  0.8, 0.1, 0.7, 0.9, 0
), nrow = 5, ncol = 5)

# Convert the matrix to a distance object
dist_obj <- as.dist(dissimilarity_matrix)

# Perform hierarchical clustering using complete linkage
hc_complete <- hclust(dist_obj, method = "complete")

# Perform hierarchical clustering using single linkage
hc_single <- hclust(dist_obj, method = "single")

# Plot dendrogram for complete linkage
plot(hc_complete, main = "Complete Linkage Dendrogram", sub = "", xlab = "Observations", ylab = "Height")
rect.hclust(hc_complete, k = 2, border = "red") # visually indicate clusters

# Plot dendrogram for single linkage
plot(hc_single, main = "Single Linkage Dendrogram", sub = "", xlab = "Observations", ylab = "Height")
rect.hclust(hc_single, k = 2, border = "blue") # visually indicate clusters

# Part c

# Define the dissimilarity matrix
dissimilarity_matrix2 <- matrix(c(
  0, 0.2, 0.6, 0.4, 0.8,
  0.2, 0, 0.3, 0.5, 0.1,
  0.6, 0.3, 0, 0.6, 0.7,
  0.4, 0.5, 0.6, 0, 0.9,
  0.8, 0.1, 0.7, 0.9, 0
), nrow = 5, ncol = 5)

# Convert the matrix to a distance object
dist_obj2 <- as.dist(dissimilarity_matrix2)

# Perform hierarchical clustering using complete linkage
hc_complete2 <- hclust(dist_obj2, method = "complete")

# Perform hierarchical clustering using single linkage
hc_single2 <- hclust(dist_obj2, method = "single")

# Reorder observations for complete linkage dendrogram
hc_complete_reordered2 <- hc_complete2
hc_complete_reordered2$labels <- c("4", "5", "3", "1", "2")

# Plot reordered complete linkage dendrogram
plot(hc_complete_reordered2, main = "Reordered Complete Linkage Dendrogram", sub = "", xlab = "Observations", ylab = "Height")
rect.hclust(hc_complete_reordered2, k = 2, border = "red")

# Reorder observations for single linkage dendrogram
hc_single_reordered2 <- hc_single2
hc_single_reordered2$labels <- c("1", "5", "3", "4", "2")

# Plot reordered single linkage dendrogram
plot(hc_single_reordered2, main = "Reordered Single Linkage Dendrogram", sub = "", xlab = "Observations", ylab = "Height")
rect.hclust(hc_single_reordered2, k = 2, border = "blue")

# Problem 3

# Part a

# Generate the exponential phase points
exponential_points <- 2^(0:10)

# Calculate the distance matrix
dist_matrix <- dist(exponential_points)

# Perform hierarchical clustering using single, complete, and average linkage
hc_single <- hclust(dist_matrix, method = "single")
hc_complete <- hclust(dist_matrix, method = "complete")
hc_average <- hclust(dist_matrix, method = "average")

# Single linkage dendrogram
plot(hc_single, main = "Single Linkage Dendrogram", xlab = "Observations", ylab = "Height")

# Complete linkage dendrogram
plot(hc_complete, main = "Complete Linkage Dendrogram", xlab = "Observations", ylab = "Height")

# Average linkage dendrogram
plot(hc_average, main = "Average Linkage Dendrogram", xlab = "Observations", ylab = "Height")

# Part b

# Define the new dissimilarity function
diss <- function(x) {
  dist_mat <- as.matrix(dist(x))
  n <- length(x)
  new_diss <- matrix(0, n, n)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      new_diss[i, j] <- new_diss[j, i] <- max(x[i], x[j]) / min(x[i], x[j])
    }
  }
  as.dist(new_diss)
}

# The data points
data_points <- 2^(0:10)

# Calculate the dissimilarity matrix using the new function
diss_matrix <- diss(data_points)

# Perform hierarchical clustering using single, complete, and average linkage
single_linkage <- hclust(diss_matrix, method = "single")
complete_linkage <- hclust(diss_matrix, method = "complete")
average_linkage <- hclust(diss_matrix, method = "average")

# Plot the dendrograms
plot(single_linkage, main = "Single Linkage Dendrogram", xlab = "Observations", sub = "", cex = 0.6)
plot(complete_linkage, main = "Complete Linkage Dendrogram", xlab = "Observations", sub = "", cex = 0.6)
plot(average_linkage, main = "Average Linkage Dendrogram", xlab = "Observations", sub = "", cex = 0.6)
