# Final Data Mining and Visualisation
# Name: Daniel Smith
#Assignment: 
#Date: 5/07/2023
library(factoextra)

# Task 1: Select variables
dat <- read.csv(file = "loans_full_schema.csv", header = TRUE, sep = ",", na.strings = "", stringsAsFactors = FALSE)
selected_vars <- c("term", "total_debit_limit", "installment", "loan_amount", "num_active_debit_accounts", "num_cc_carrying_balance", "interest_rate")
dat_selected <- dat[selected_vars]

# Task 2: Clean the data
dat_cleaned <- na.omit(dat_selected)

# Task 3: Scale variables without interest rate
dat_scaled <- scale(dat_cleaned[, 1:6])

# Task 4: Elbow plot analysis
set.seed(123)
k_values <- 1:10
wss <- numeric(length(k_values))

for (i in k_values) {
  kmeans_model <- kmeans(dat_scaled, centers = i, nstart = 1)
  wss[i] <- kmeans_model$tot.withinss
}

plot(k_values, wss, type = "b", xlab = "Number of Clusters (k)", ylab = "Within-Cluster Sum of Squares (WSS)", main = "Elbow Plot")

# Task 5a: K-means clustering (k=4)
set.seed(123)
kmeans_model <- kmeans(dat_scaled, centers = 4, nstart = 10)

# Task 5a: Examine results
cluster_sizes <- table(kmeans_model$cluster)
print(cluster_sizes)


# Task 5b: Calculate between clusters sum of squares to total sum of squares ratio
between_ss <- sum(kmeans_model$betweenss)
total_ss <- sum(kmeans_model$totss)
ratio <- between_ss / total_ss

# Print the ratio
print(paste("Between Clusters SS to Total SS Ratio:", round(ratio * 100, 2), "%"))


# Task 6: Examine cluster centers
cluster_centers <- kmeans_model$centers
print(cluster_centers)

# Calculate average num_active_debit_accounts
average_num_active_debit_accounts <- mean(dat_selected$num_active_debit_accounts)

# Get cluster centers
cluster_centers <- kmeans_model$centers

# Find the cluster that tends to group longer-term loans
long_term_cluster <- cluster_centers[which.max(cluster_centers[, "term"]), ]

# Compare with the average
if (long_term_cluster["num_active_debit_accounts"] > average_num_active_debit_accounts) {
  print("The cluster tends to have applicants with num_active_debit_accounts above average.")
} else if (long_term_cluster["num_active_debit_accounts"] < average_num_active_debit_accounts) {
  print("The cluster tends to have applicants with num_active_debit_accounts below average.")
} else {
  print("The cluster tends to have applicants with num_active_debit_accounts at average.")
}

# Task 1: Select variables
dat <- read.csv(file = "loans_full_schema.csv", header = TRUE, sep = ",", na.strings = "", stringsAsFactors = FALSE)
selected_vars <- c("term", "total_debit_limit", "installment", "loan_amount", "num_active_debit_accounts", "num_cc_carrying_balance", "interest_rate")
dat_selected <- dat[selected_vars]

# Task 2: Clean the data
dat_cleaned <- na.omit(dat_selected)

# Task 3: Scale variables without interest rate
dat_scaled <- scale(dat_cleaned[, 1:6])

# Task 4: Elbow plot analysis
set.seed(123)
k_values <- 1:10
wss <- numeric(length(k_values))

for (i in k_values) {
  kmeans_model <- kmeans(dat_scaled, centers = i, nstart = 1)
  wss[i] <- kmeans_model$tot.withinss
}

plot(k_values, wss, type = "b", xlab = "Number of Clusters (k)", ylab = "Within-Cluster Sum of Squares (WSS)", main = "Elbow Plot")

# Task 5a: K-means clustering (k=4)
set.seed(123)
kmeans_model <- kmeans(dat_scaled, centers = 4, nstart = 10)

# Task 5a: Examine results
cluster_sizes <- table(kmeans_model$cluster)
print(cluster_sizes)


# Task 5b: Calculate between clusters sum of squares to total sum of squares ratio
between_ss <- sum(kmeans_model$betweenss)
total_ss <- sum(kmeans_model$totss)
ratio <- between_ss / total_ss

# Print the ratio
print(paste("Between Clusters SS to Total SS Ratio:", round(ratio * 100, 2), "%"))


# Task 6: Examine cluster centers
cluster_centers <- kmeans_model$centers
print(cluster_centers)

# Calculate average num_active_debit_accounts
average_num_active_debit_accounts <- mean(dat_selected$num_active_debit_accounts)

# Get cluster centers
cluster_centers <- kmeans_model$centers

# Find the cluster that tends to group longer-term loans
long_term_cluster <- cluster_centers[which.max(cluster_centers[, "term"]), ]
print(long_term_cluster)
print(average_num_active_debit_accounts)

# Compare with the average
if (long_term_cluster["num_active_debit_accounts"] > average_num_active_debit_accounts) {
  print("The cluster tends to have applicants with num_active_debit_accounts above average.")
} else if (long_term_cluster["num_active_debit_accounts"] < average_num_active_debit_accounts) {
  print("The cluster tends to have applicants with num_active_debit_accounts below average.")
} else {
  print("The cluster tends to have applicants with num_active_debit_accounts at average.")
}


#6c
print(cluster_centers[2,])
print(cluster_centers[1,])

# Task 8: Create boxplot comparing interest rates for each cluster
boxplot(dat_selected$interest_rate ~ cluster_assignments, xlab = "Cluster", ylab = "Interest Rate", main = "Distribution of Interest Rates by Cluster")

print(cluster_centers)
print(cluster_centers[3,])





