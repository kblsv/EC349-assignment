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

rm(list = c("attributes_df"))

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

# Defining the tip variables for which to calculate correlations with 'stars'
corr_tip_variables <- c("compliment_count") 

# Calculating the correlations
corr_tip <- sapply(corr_tip_variables, function(var) {
  cor(combined_tip_review_data$stars, combined_tip_review_data[[var]], use = "complete.obs")
})

# Correlations: review data and business data 

business_data <- business_data %>%
  rename(
    business_stars = stars  # Renamed to avoid conflict with 'stars' in review_data_small
  )
# Merging
combined_review_business_data <- merge(review_data_small, business_data, by = "business_id")

# Looking at numerical/integer variables first
corr_numerical_business_variables <- c("latitude", "longitude", "business_stars", "review_count", "is_open")

corr_numerical_business <- sapply(corr_numerical_business_variables, function(var) {
  cor(combined_review_business_data$stars, combined_review_business_data[[var]], use = "complete.obs")
})

# Looking at non-numerical variables

colnames(business_data$attributes)

# To confirm different categories, see unique values in each column.
# Apply the 'unique' function to each column of the 'attributes' dataframe
unique_values_in_attributes <- lapply(business_data$attributes, unique)

# print(unique_values_in_attributes) - too long for console

View(unique_values_in_attributes)

write.csv(unique_values_in_attributes, "unique_values_in_attributes.csv", row.names = FALSE)

# Convert each list element to a string and then combine them into a dataframe
attributes_df <- data.frame(
  lapply(unique_values_in_attributes, function(x) paste(x, collapse = ", "))
)

write.csv(attributes_df, "unique_values_in_attributes.csv", row.names = FALSE)

# Looking at binary attributes

# Extract Nested Attributes and conv erting to datafram
attributes_df <- purrr::map_df(combined_review_business_data$attributes, ~.x)
#Remove non binary
attributes_df <- attributes_df %>% select(all_of(attributes_to_correlate))
#Convert to Numeric
# True = 1, False = 0, NA/NULL remains NA
attributes_df <- attributes_df %>%
  mutate(across(everything(), ~ifelse(. == "True", 1, ifelse(. == "False", 0, ifelse(. == "None", NA, NA)))))

# Checking that all values are indeed 1, 0, or NA: 
unique_values_in_attributes_df <- lapply(attributes_df, unique)
names(unique_values_in_attributes_df) <- names(attributes_df)
View(unique_values_in_attributes_df)

#Adding stars to attributes_df
attributes_df$stars <- combined_review_business_data$stars

# Define the attributes for which you want to calculate correlation with 'stars'
# Excluding 'stars' column for the correlation calculation
corr_binary_business_variables <- setdiff(names(attributes_df), "stars")

# Calculate the correlation for each attribute against 'stars'
corr_binary_business <- sapply(corr_binary_business_variables, function(attr) {
  cor(attributes_df[[attr]], attributes_df$stars, use = "complete.obs")
})

# Modelling

