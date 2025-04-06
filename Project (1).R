setwd("C:\\Users\\athar\\OneDrive\\Desktop\")
getwd()
data <- read.csv("/Users/Desktop/UB/DATA ANALYTICS/DAPM/DAPM Final Project/data.csv")
# install.packages("geosphere")
library(geosphere)

R <- 6371 #This sets the Earth's radius (in kilometers) to the variable R.
#The function is defined to convert degrees to radians.
deg2rad <- function(deg) {
  return(deg * (pi/180))
}


# Function calculates the distance between two geographical points using the haversine formula.
distcalculate <- function(lat1, lon1, lat2, lon2) {
  d_lat <- deg2rad(lat2 - lat1)
  d_lon <- deg2rad(lon2 - lon1)
  a <- sin(d_lat/2)^2 + cos(deg2rad(lat1)) * cos(deg2rad(lat2)) * sin(d_lon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  return(R * c)
}
# Data Preprocessing
data$distance <- sapply(1:nrow(data), function(i) {
  distcalculate(data[i, "Restaurant_latitude"],
                data[i, "Restaurant_longitude"],
                data[i, "Delivery_location_latitude"],
                data[i, "Delivery_location_longitude"])
})

df1 <- data
head(df1)
summary(df1)
missing_values <- sapply(df1, function(x) sum(is.na(x)))# to find na values in the data
print(missing_values)
empty_string_count <- sapply(df1, function(x) sum(x == ""))# to find empty(null)values from the data
empty_string_count

data <- na.omit(data)
sum(is.na(data))
# Load necessary libraries
library(dplyr)
# Replace NA values with median
df1$Delivery_person_Age <- ifelse(is.na(df1$Delivery_person_Age),
                                   median(df1$Delivery_person_Age, na.rm = TRUE),
                                   df1$Delivery_person_Age)

df1$Delivery_person_Ratings <- ifelse(is.na(df1$Delivery_person_Ratings),
                                  median(df1$Delivery_person_Ratings, na.rm = TRUE),
                                  df1$Delivery_person_Ratings)



mode_value <- as.numeric(names(sort(table(df1$multiple_deliveries), decreasing = TRUE)[1]))

# Replace NA values with mode
df1$multiple_deliveries <- ifelse(is.na(df1$multiple_deliveries), mode_value, df1$multiple_deliveries)

columns_to_consider <- c("Weather_conditions", "Road_traffic_density", "Festival","City")
for (i in columns_to_consider) {
  mode_value <- as.character(names(sort(table(df1[[i]]), decreasing = TRUE)[1]))
  df1[[i]][df1[[i]] == ""] <- mode_value
}

unique_levels <- unique(data$Weather_conditions)

# Create dummy variables using ifelse
for (level in unique_levels) {
  data[paste0("Weather_", level)] <- ifelse((data$Weather_conditions) == level, 1, 0)
}
unique_levels <- unique(data$Road_traffic_density)


# Create dummy variables using ifelse
for (level in unique_levels) {
  data[paste0("Road_", level)] <- ifelse((data$Road_traffic_density) == level, 1, 0)
}

unique_levels <- unique(data$Festival)

# Create dummy variables using ifelse
for (level in unique_levels) {
  data[paste0("Festival_", level)] <- ifelse((data$Festival) == level, 1, 0)
}
unique_levels <- unique(data$City)
# Create dummy variables using ifelse
for (level in unique_levels) {
  data[paste0("City_", level)] <- ifelse((data$City) == level, 1, 0)
}


data_subset = data[, !colnames(data) %in% c('Weather_conditions','Road_traffic_density','Festival','City','ID','Delivery_person_ID','Restaurant_latitude','Restaurant_longitude','Delivery_location_latitude','Delivery_location_longitude','Order_Date','Time_Orderd','Time_Order_picked','Type_of_order','Type_of_vehicle')]

#LINEAR REGRESSION
library(caTools)
s<- sample.split(Y = data_subset, SplitRatio = 0.60)
train <- subset(data_subset, s == TRUE)
test <- subset(data_subset,  s == FALSE)

linearmodel<- lm(Time_taken..min.~.,data=train)
summary(linearmodel)

# Make predictions on the test set
predictions <- predict(linearmodel, newdata = test)

# Calculate RMSE
rmse_linear <- sqrt(mean((test$Time_taken..min. - predictions)^2))
cat("RMSE:", rmse_linear, "\n")

# Calculate R-squared
r2_linear <- 1 - sum((test$Time_taken..min. - predictions)^2) / sum((test$Time_taken..min. - mean(test$Time_taken..min.))^2)
cat("R-squared:", r2_linear, "\n")

# install.packages("glmnet")
library(glmnet)
x.train <- data.matrix(train[,-1])
y.train <- train$Time_taken..min.
x.test <- data.matrix(test[,-1])
y.test <- test$Time_taken..min.

#RIDGE REGRESSION
ridge_model <- cv.glmnet(x.train, y.train, alpha = 0)
lambda_optimal_ridge <- ridge_model$lambda.min

# Make predictions on the test set
predictions <- predict(ridge_model, newx = x.test, s = lambda_optimal_ridge)

# Calculate RMSE
rmse_ridge <- sqrt(mean((y.test - predictions)^2))
cat("RMSE:", rmse_ridge, "\n")

# Calculate R-squared
r2_ridge <- 1 - sum((y.test - predictions)^2) / sum((y.test - mean(y.test))^2)
cat("R-squared:", r2_ridge, "\n")


#LASSO REGRESSION
lasso_model <- cv.glmnet(x.train, y.train, alpha = 1)
optimal_lambda_lasso <- lasso_model$lambda.min

# Make predictions on the test set
predictions <- predict(lasso_model, newx = x.test, s = optimal_lambda_lasso)

# Calculate RMSE
rmse_lasso <- sqrt(mean((y.test - predictions)^2))
cat("RMSE:", rmse_lasso, "\n")

# Calculate R-squared
r2_lasso <- 1 - sum((y.test - predictions)^2) / sum((y.test - mean(y.test))^2)
cat("R-squared:", r2_lasso, "\n")

#RANDOM FOREST REGRESSION
library(randomForest)
rf_model <- randomForest(Time_taken..min. ~ ., data = train, ntree = 100)

# Make predictions on the test set
predictions <- predict(rf_model, newdata = test)

# Calculate RMSE
rmse_rf <- sqrt(mean((test$Time_taken..min. - predictions)^2))
cat("RMSE:", rmse_rf, "\n")
r2_rf <- 1 - sum((test$Time_taken..min. - predictions)^2) / sum((test$Time_taken..min. - mean(test$Time_taken..min.))^2)
cat("R-squared:", r2_rf, "\n")

#DECISION TREE REGRESSION
# install.packages("rpart")
library(rpart)

# Fit Decision Tree model
tree_model <- rpart(Time_taken..min. ~ ., data = train, method = "anova")

# Make predictions on the test set
predictions_tree <- predict(tree_model, newdata = test)

# Calculate RMSE
rmse_tree <- sqrt(mean((test$Time_taken..min. - predictions_tree)^2))
cat("Decision Tree RMSE:", rmse_tree, "\n")

# Calculate R-squared
r2_tree <- 1 - sum((test$Time_taken..min. - predictions_tree)^2) / sum((test$Time_taken..min. - mean(test$Time_taken..min.))^2)
cat("Decision Tree R-squared:", r2_tree, "\n")

#FIT GRADIENT BOOSTING
# install.packages("gbm")
library(gbm)

# Combine predictors and response into a data frame
train_data <- cbind.data.frame(y.train, x.train)

# Fit Gradient Boosting model
gbm_model <- gbm(y.train ~ ., data = train_data, distribution = "gaussian", n.trees = 500)

# Make predictions on the test set
predictions_gbm <- predict(gbm_model, newdata = data.frame(x.test))

# Calculate RMSE
rmse_gbm <- sqrt(mean((y.test - predictions_gbm)^2))
cat("Gradient Boosting RMSE:", rmse_gbm, "\n")

# Calculate R-squared
r2_gbm <- 1 - sum((y.test - predictions_gbm)^2) / sum((y.test - mean(y.test))^2)
cat("Gradient Boosting R-squared:", r2_gbm, "\n")

models <- c("Linear Regression", "Ridge Regression", "Lasso Regression" ,"Decision Tree" ,"Random Forest", "Gradient Boosting")
rmse <- c(rmse_linear, rmse_ridge, rmse_lasso, rmse_tree, rmse_rf, rmse_gbm)
r2 <- c(r2_linear, r2_ridge, r2_lasso, r2_tree, r2_rf, r2_gbm)
results <- data.frame(models, rmse, r2)
print(results)
