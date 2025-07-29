# People-Analytics-for-Teach-for-America

# Load necessary libraries ----
library(readxl)
library(ggplot2)
library(caret)
library(C50)
library(ggdendro)
library(tidyverse)
library(dplyr)
library(e1071)
library(kernlab)
# Load and inspect the dataset ----
file_path <- "/Users/macbookair/Downloads/TFA_A_Data.xlsx"
data <- read_excel(file_path, sheet = "TFA_A_Data")
# Quick dataset exploration
head(data)
str(data)
# Check for missing values
colSums(is.na(data))
# Summarize
summary(data)
# Converting Categorical variables into factors
data$appdeadline <- factor(data$appdeadline)
data$major1group <- factor(data$major1group)
data$major2group <- factor(data$major2group)
data$minorgroup <- factor(data$minorgroup)
data$major1 <- factor(data$major1)
data$major2 <- factor(data$major2)
data$minor <- factor(data$minor)
# Converting Binary variables into factors
data$completedadm <- factor(data$completedadm)
data$attendedevent <- factor(data$attendedevent)
data$stem <- factor(data$stem)
data$schoolsel <- factor(data$schoolsel)
# Summarize
summary(data)
#Assess the Nature of NA Data
#Why: Understanding the patterns of NA's helps decide whether to impute, remo
ve, or analyze separately.
# Check the proportion of missing values in schoolsel
prop_na <- sum(is.na(data$schoolsel)) / nrow(data)
cat("Proportion of missing values in schoolsel:", prop_na, "\n")
# Create a new category for missing values (run 2 times)
data$schoolsel <- ifelse(is.na(data$schoolsel), "unknown", data$schoolsel)
data$schoolsel <- factor(data$schoolsel, levels = c("1", "2", "3", "4", "5",
"unknown"))
summary(data$schoolsel)
# Check class balance ----
13
class_balance <- table(data$completedadm)
class_balance_pct <- prop.table(class_balance) * 100
cat("Class Balance:", class_balance, "\n")
cat("Completion Rate:", round(class_balance_pct[2], 2), "%\n")
# Feature engineering
# Assigning weighted points to majors
data <- data %>%
mutate(
major1_weight = ifelse(!is.na(major1group) & major1group != "none", 2.0,
0),
major2_weight = ifelse(!is.na(major2group) & major2group != "none", 1.5,
0),
minor_weight = ifelse(!is.na(minorgroup) & minorgroup != "none", 1.0, 0),
major_points_weighted = major1_weight + major2_weight + minor_weight,
major_combination_type = case_when(
!is.na(major1group) & !is.na(major2group) &
major1group != "none" & major2group != "none" ~ "double_major",
!is.na(major1group) & !is.na(minorgroup) &
major1group != "none" & minorgroup != "none" ~ "major_minor",
!is.na(major1group) & major1group != "none" ~ "single_major",
TRUE ~ "no_major"
)
)
# Validate major combinations ----
table(data$major_combination_type)
# Application deadline and timing features ----
data <- data %>%
mutate(
total_essay_length = essay1length + essay2length + essay3length,
signup_to_submit_time = signupdate - submitteddate,
start_to_submit_time = starteddate - submitteddate
)
summary(select(data, total_essay_length, signup_to_submit_time, start_to_subm
it_time))
#CLUSTERING
# Standardize timing features by application deadline using scale() ----
data <- data %>%
group_by(appdeadline) %>%
mutate(
signupdate_std = scale(signupdate, center = TRUE, scale = TRUE)[, 1],
starteddate_std = scale(starteddate, center = TRUE, scale = TRUE)[, 1],
submitteddate_std = scale(submitteddate, center = TRUE, scale = TRUE)[, 1
]
) %>%
ungroup()
# Hierarchical Clustering ----
# Compute Euclidean distances
d <- dist(select(data, signupdate_std, starteddate_std, submitteddate_std), m
ethod = "euclidean")
# Perform hierarchical clustering using Ward's method
hc <- hclust(d, method = "ward.D")
14
# Visualize dendrogram
ggdendrogram(hc, theme_dendro = TRUE) +
ggtitle("Dendrogram of Applicants (Ward's Method)")
# Cut tree to form clusters
data$hc_cluster <- cutree(hc, k = 3)
# Characterize hierarchical clusters
hc_cluster_summary <- data %>%
group_by(hc_cluster) %>%
summarize(
avg_signupdate = mean(signupdate),
avg_starteddate = mean(starteddate),
avg_submitteddate = mean(submitteddate),
count = n(),
.groups = 'drop'
)
# Print hierarchical cluster summaries
print(hc_cluster_summary)
# K-means Clustering ----
# Extract standardized clustering data
clustering_data <- select(data, signupdate_std, starteddate_std, submitteddat
e_std)
# Perform K-means clustering
set.seed(333)
kmeans_result <- kmeans(clustering_data, centers = 3)
# Add K-means cluster assignments to the dataset
data <- data %>%
mutate(km_cluster = kmeans_result$cluster)
# Handle Late Submissions ----
data <- data %>%
mutate(
is_late_submission = ifelse(submitteddate < 0, 1, 0), # 1 for late, 0 fo
r on-time
adjusted_submission_days = ifelse(submitteddate < 0, abs(submitteddate),
submitteddate) # Convert negative values to absolute
)
# Summarize late submissions by cluster
late_submission_summary <- data %>%
group_by(km_cluster) %>%
summarize(
total_late = sum(is_late_submission),
total_applicants = n(),
percent_late = (total_late / total_applicants) * 100,
avg_submission_days = mean(adjusted_submission_days, na.rm = TRUE),
.groups = 'drop'
)
# Print summary of late submissions
print(late_submission_summary)
# Characterize K-means clusters ----
km_cluster_summary <- data %>%
15
group_by(km_cluster) %>%
summarize(
avg_signupdate = mean(signupdate, na.rm = TRUE),
avg_starteddate = mean(starteddate, na.rm = TRUE),
avg_submitteddate = mean(adjusted_submission_days, na.rm = TRUE), # Use
adjusted submission days
count = n(),
.groups = 'drop'
)
# Print K-means cluster summaries
print(km_cluster_summary)
# Name Clusters for Interpretability ----
data <- data %>%
mutate(
km_cluster_name = case_when(
km_cluster == 1 ~ "Prolonged Decision Makers",
km_cluster == 2 ~ "Moderate Decision Makers",
km_cluster == 3 ~ "Efficient Decision Makers",
TRUE ~ "Unknown"
)
)
# View final detailed cluster data ----
detailed_cluster_data <- data %>%
select(km_cluster_name, signupdate, starteddate, submitteddate, adjusted_su
bmission_days, is_late_submission)
print(detailed_cluster_data, n = 30)
#Explore cluster behavior with completedadm:
completion_by_cluster <- data %>%
group_by(km_cluster_name) %>%
summarize(
completion_rate = mean(completedadm == "1", na.rm = TRUE),
count = n(),
.groups = 'drop'
)
print(completion_by_cluster)
# Prepare Dataset for Modeling ----
# Select relevant features
# 1. First, let's create a more comprehensive feature set
model_data <- select(data,c(
completedadm,gpa, total_essay_length, essayssentiment,
major_points_weighted, signup_to_submit_time, start_to_submit_time,
schoolsel, km_cluster,stem, attendedevent))
summary(model_data)
# Train-Test Split
set.seed(333) # For reproducibility
train_index <- createDataPartition(model_data$completedadm, p = 0.7, list = F
ALSE)
train_set <- model_data[train_index, ]
test_set <- model_data[-train_index, ]
#models#
##### Decision Tree using C5.0 ----
16
# Train C5.0 Model
library(C50)
train_predictors <- train_set %>% select(-completedadm)
tfa_model <- C5.0(train_predictors, train_set$completedadm)
# Make predictions and evaluate
tfa_pred <- predict(tfa_model, test_set)
initial_results <- confusionMatrix(test_set$completedadm, tfa_pred)
print("Initial Model Results:")
print(initial_results)
#tuning wit cost matrix
# Define a cost matrix
cost_matrix <- matrix(c(0, 5, 1, 0), nrow = 2,
dimnames = list(c("0", "1"), c("0", "1")))
#refine cost matrix
# Adjust Cost Matrix
cost_matrix <- matrix(c(0, 10, 1, 0), nrow = 2,
dimnames = list(c("0", "1"), c("0", "1")))
# 3rd balancing a cost matrix
cost_matrix <- matrix(c(0, 3, 1, 0), nrow = 2,
dimnames = list(c("0", "1"), c("0", "1")))
# 4th balancing a cost matrix (decent)
cost_matrix <- matrix(c(0, 2, 1, 0), nrow = 2,
dimnames = list(c("0", "1"), c("0", "1")))
# Train C5.0 with cost matrix
tfa_cost <- C5.0(train_predictors, train_set$completedadm,trials = 10,costs =
cost_matrix)
tfa_cost_pred <- predict(tfa_cost, test_set)
cost_results <- confusionMatrix(test_set$completedadm, tfa_cost_pred)
print("\nCost-Sensitive Model Results:")
print(cost_results)
#KNN###############
# Load the required library for kNN ----
library(class)
# Normalize Numeric Features ----
# Extract numeric features
numeric_features <- train_set %>%
select(total_essay_length, signup_to_submit_time, start_to_submit_time, gpa
, essayssentiment)
# Normalize the numeric features
normalize <- function(x) {
(x - min(x)) / (max(x) - min(x))
}
train_norm <- as.data.frame(lapply(numeric_features, normalize))
test_norm <- as.data.frame(lapply(test_set[, colnames(numeric_features)], nor
malize))
# Add the categorical columns back
17
train_norm$completedadm <- train_set$completedadm
test_norm$completedadm <- test_set$completedadm
# Select predictors and response for kNN ----
train_x <- train_norm[, -which(colnames(train_norm) == "completedadm")]
test_x <- test_norm[, -which(colnames(test_norm) == "completedadm")]
train_y <- train_norm$completedadm
test_y <- test_norm$completedadm
# Determine optimal k using cross-validation ----
accuracy <- c()
k_values <- seq(1, 15, 2) # Try odd k values to avoid ties
for (k in k_values) {
pred <- knn(train = train_x, test = train_x, cl = train_y, k = k)
acc <- mean(pred == train_y)
accuracy <- c(accuracy, acc)
}
# Plot k vs. accuracy
plot(k_values, accuracy, type = "b", main = "k vs Accuracy", xlab = "k", ylab
= "Accuracy")
# Optimal k
optimal_k <- k_values[which.max(accuracy)]
cat("Optimal k:", optimal_k, "\n")
# Train Final kNN Model ----
knn_pred <- knn(train = train_x, test = test_x, cl = train_y, k = optimal_k)
# Evaluate kNN Model ----
conf_matrix_knn <- confusionMatrix(as.factor(knn_pred), as.factor(test_y), po
sitive = "1")
print(conf_matrix_knn)
#SVM
# Ensure `completedadm` is a factor
train_set$completedadm <- as.factor(train_set$completedadm)
test_set$completedadm <- as.factor(test_set$completedadm)
# SVM Model with RBF Kernel ----
svm_rbf <- ksvm(completedadm ~ ., data = train_set, kernel = "rbfdot",C = 1,p
rob.model = TRUE)
# Predictions on Test Set and Print Confusion Matrix for RBF Kernel
svm_rbf_predictions <- predict(svm_rbf, test_set, type = "response")
conf_matrix_svm_rbf <- confusionMatrix(svm_rbf_predictions, test_set$complete
dadm, positive = "1")
print(conf_matrix_svm_rbf)
#Final SVM tuning (without oversampling)
# Define a grid of hyperparameters
cost_values <- c(0.1, 1, 10, 100) # Different cost values to test
gamma_values <- c(0.01, 0.1, 1) # Different gamma values to test
# Create a placeholder for results
grid_search_results <- data.frame(
Cost = numeric(),
Gamma = numeric(),
Accuracy = numeric(),
Sensitivity = numeric(),
18
Specificity = numeric(),
Balanced_Accuracy = numeric()
)
# Recalculate weights with a larger penalty
class_weights <- c("0" = 10, "1" = 1)
# Perform grid search
for (cost in cost_values) {
for (gamma in gamma_values) {
# Train SVM model with current hyperparameters
svm_model <- svm(completedadm ~ ., data = train_set, kernel = "radial",co
st = cost,gamma = gamma,
class.weights = class_weights,scale = TRUE)
svm_predictions <- predict(svm_model, test_set)
conf_matrix <- confusionMatrix(svm_predictions, test_set$completedadm, po
sitive = "1")
# Extract performance metrics
accuracy <- conf_matrix$overall["Accuracy"]
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
balanced_accuracy <- (sensitivity + specificity) / 2
# Save results
grid_search_results <- rbind(grid_search_results, data.frame(
Cost = cost,
Gamma = gamma,
Accuracy = accuracy,
Sensitivity = sensitivity,
Specificity = specificity,
Balanced_Accuracy = balanced_accuracy
))
}}
# Print grid search results
print(grid_search_results)
# Find the best parameters
best_params <- grid_search_results[which.max(grid_search_results$Balanced_Acc
uracy), ]
cat("Best Parameters:\n")
print(best_params)
# Train final SVM model using best parameters
svm_model_optimized <- svm(
completedadm ~ .,
data = train_set,
kernel = "radial",
cost = best_params$Cost,
gamma = best_params$Gamma,
class.weights = class_weights,
scale = TRUE
)
# Make predictions with optimized model
svm_predictions_optimized <- predict(svm_model_optimized, test_set)
# Evaluate performance
conf_matrix_optimized <- confusionMatrix(svm_predictions_optimized, test_set$
completedadm, positive = "1")
print(conf_matrix_optimized

