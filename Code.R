# Installed homebrew, installed git, initialised git, pushed the project onto GitHub repository.

# Setting working directory.
setwd("/Users/ekaterina/Documents/EC349 project")

# Loading data

# Pre-Processing Yelp Academic Data for the Assignment
library(jsonlite)

# Clear
cat("\014")  
# rm(list=ls())

# Set Directory as appropriate
setwd("/Users/ekaterina/Documents/EC349 project/data")

# Load Small Datasets (user and review)
load("yelp_review_small.Rda")
load("yelp_user_small.Rda")

# Load Json Datasets
business_data <- stream_in(file("yelp_academic_dataset_business.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
checkin_data  <- stream_in(file("yelp_academic_dataset_checkin.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
tip_data  <- stream_in(file("yelp_academic_dataset_tip.json")) #note that stream_in reads the json lines (as the files are json lines, not json)

# Analytic approach

# Descriptive

str(review_data_small)
summary(review_data_small$stars)
summary(review_data_small$useful)
summary(review_data_small$funny)
summary(review_data_small$cool)
summary(user_data_small$review_count)
summary(user_data_small$average_stars)

library(ggplot2)

# Stars histogram
ggplot(review_data_small, aes(x=stars)) +
  geom_histogram(binwidth=1, fill='blue', color='black') +
  ggtitle("Distribution of Stars Ratings")

# Review count boxplot
ggplot(user_data_small, aes(y = review_count)) +
  geom_boxplot(fill = "blue", color = "black") +
  ggtitle("Boxplot of Review Count")

# Average stars boxplot
ggplot(user_data_small, aes(y = average_stars)) +
  geom_boxplot(fill = "green", color = "black") +
  ggtitle("Boxplot of Average Stars")

# DIAGNOSTIC

# Correlations: review data only 
corr_review_variables <- c("useful", "funny", "cool")

# Use sapply to calculate correlations
corr_review <- sapply(corr_review_variables, function(var) {
  cor(review_data_small$stars, review_data_small[[var]], use = "complete.obs")
})

# rm(list = c("corr_review_data", "corr_variables_review"))

# Correlations: review data and user data 
library(dplyr)

# Renaming conflicting columns in user_data_small before merging
user_data_small <- user_data_small %>%
  rename(
    user_useful = useful,
    user_funny = funny,
    user_cool = cool
  )

# Merging review_data_small with user_data_small
combined_user_review_data <- merge(review_data_small, user_data_small, by = "user_id")

# Defining the user variables for which to calculate correlations with 'stars'
corr_user_variables <- c("review_count", "user_useful", "user_funny", "user_cool", "fans", "average_stars")  # Add other relevant variables from user_data_small

# Calculating the correlations
corr_user <- sapply(corr_user_variables, function(var) {
  cor(combined_user_review_data$stars, combined_user_review_data[[var]], use = "complete.obs")
})

# Print the user correlations
print(corr_user)

# Correlations: review data and tip data

# Renaming conflicting columns in tip_data before merging
tip_data <- tip_data %>%
  rename(
    tip_business_id = business_id,
    tip_text = text,
    tip_date = date
  )

# Merging review_data_small with tip_data
combined_tip_review_data <- merge(review_data_small, tip_data, by = "user_id")

# Defining the user variables for which to calculate correlations with 'stars'
corr_tip_variables <- c("compliment_count") 

# Calculating the correlations
corr_tip <- sapply(corr_tip_variables, function(var) {
  cor(combined_user_review_data$stars, combined_user_review_data[[var]], use = "complete.obs")
})

# Print the user correlations
print(corr_user)







