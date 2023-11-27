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

rm(list = c("model2_data"))

# Correlations: review data and user data 
library(dplyr)

# Renaming conflicting columns in user_data_small before merging
user_data_small <- user_data_small %>%
  rename(
    #user_useful = useful,
    #user_funny = funny,
    #user_cool = cool,
    user_review_count = review_count
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

# Creating a separate dataframe with dependent variable and predictors

library(dplyr)

model1_data <- review_data_small %>%
  left_join(user_data_small, by = "user_id") %>%
  left_join(business_data %>% 
              mutate(DriveThru = attributes$DriveThru, 
                     Open24Hours = attributes$Open24Hours), by = "business_id")

model1_data <- model1_data %>%
  mutate(DriveThru = ifelse(is.na(attributes$DriveThru), NA, attributes$DriveThru),
         Open24Hours = ifelse(is.na(attributes$Open24Hours), NA, attributes$Open24Hours))

model1_data <- model1_data %>%
  dplyr::select(`stars`, `average_stars`, `business_stars`, `DriveThru`, `Open24Hours`)

model1_data <- model1_data %>%
  mutate(
    Open24Hours = case_when(
      Open24Hours == "True" ~ 1,
      Open24Hours == "False" ~ 0,
      TRUE ~ NA_real_  # Assign NA for any other case
    ),
    DriveThru = case_when(
      DriveThru == "True" ~ 1,
      DriveThru == "False" ~ 0,
      DriveThru == "None" ~ NA_real_,  # Assign NA for "None"
      TRUE ~ NA_real_  # Assign NA for any other case
    )
  )

# Splitting the data, with the test dataset containing 10,000 observations
install.packages('caret')
library(caret)

set.seed(1)
train <- sample(1:nrow(model1_data),nrow(model1_data)-10000)
train_data<-model1_data[train,]
test_data<-model1_data[-train,]

# Making 'stars' ordered as it needs to be a factor
train_data$stars <- factor(train_data$stars, levels = c(1, 2, 3, 4, 5), ordered = TRUE)
test_data$stars <- factor(test_data$stars, levels = c(1, 2, 3, 4, 5), ordered = TRUE)

# Ordered multinomial logit 
install.packages("MASS")
library(MASS)

logit_model11 <- polr(stars ~ average_stars + business_stars + Open24Hours, data = train_data, method = 'logistic')
logit_model12<- polr(stars ~ average_stars + business_stars + DriveThru, data = train_data, method = 'logistic')
logit_model13 <- polr(stars ~ average_stars + business_stars + DriveThru + Open24Hours, data = train_data, method = 'logistic')
logit_model14 <- polr(stars ~ average_stars + business_stars, data = train_data, method = 'logistic')
logit_model15 <- polr(stars ~ average_stars, data = train_data, method = 'logistic')
logit_model16 <- polr(stars ~ business_stars, data = train_data, method = 'logistic')

summary(logit_model11)
summary(logit_model12)
summary(logit_model13)
summary(logit_model14)
summary(logit_model15)
summary(logit_model16)

str(train_data)

#observing missing data more closely 
install.packages("naniar")
library(naniar)
gg_miss_var(model1_data)
miss_var_summary(model1_data)

# model2_data
# creating a dataframe containining all variables (broken down in steps due to memory usage)

library(dplyr)

model2_data <- review_data_small %>%
  left_join(user_data_small, by = "user_id") %>%
  left_join(business_data %>%
              mutate(
                ByAppointmentOnly = attributes$ByAppointmentOnly,
                BusinessAcceptsCreditCards = attributes$BusinessAcceptsCreditCards,
                BikeParking = attributes$BikeParking,
                RestaurantsPriceRange2 = attributes$RestaurantsPriceRange2,
                CoatCheck = attributes$CoatCheck,
                RestaurantsTakeOut = attributes$RestaurantsTakeOut,
                RestaurantsDelivery = attributes$RestaurantsDelivery,
                Caters = attributes$Caters,
                WheelchairAccessible = attributes$WheelchairAccessible,
                HappyHour = attributes$HappyHour,
                OutdoorSeating = attributes$OutdoorSeating,
                HasTV = attributes$HasTV,
                RestaurantsReservations = attributes$RestaurantsReservations,
                DogsAllowed = attributes$DogsAllowed,
                GoodForKids = attributes$GoodForKids,
                RestaurantsTableService = attributes$RestaurantsTableService,
                RestaurantsGoodForGroups = attributes$RestaurantsGoodForGroups,
                DriveThru = attributes$DriveThru,
                BusinessAcceptsBitcoin = attributes$BusinessAcceptsBitcoin,
                GoodForDancing = attributes$GoodForDancing,
                AcceptsInsurance = attributes$AcceptsInsurance,
                BYOB = attributes$BYOB,
                Corkage = attributes$Corkage,
                Open24Hours = attributes$Open24Hours,
                RestaurantsCounterService = attributes$RestaurantsCounterService
              ), by = "business_id")

#selecting only necessary columns (non-character) in model2_data due to Error in `vctrs::vec_locate_matches()`
model2_data<- model2_data %>%
  select(stars, useful, funny, cool, user_review_count, user_useful, user_funny, user_cool, fans, average_stars,
         compliment_hot, compliment_more, compliment_profile, compliment_cute, compliment_list, compliment_note,
         compliment_plain, compliment_cool, compliment_funny, compliment_writer, compliment_photos, latitude,
         longitude, business_stars, review_count, is_open, ByAppointmentOnly, BusinessAcceptsCreditCards, BikeParking,
         RestaurantsPriceRange2, CoatCheck, RestaurantsTakeOut, RestaurantsDelivery, Caters, WheelchairAccessible,
         HappyHour, OutdoorSeating, HasTV, RestaurantsReservations, DogsAllowed, GoodForKids, RestaurantsTableService,
         RestaurantsGoodForGroups, DriveThru, BusinessAcceptsBitcoin, GoodForDancing, AcceptsInsurance, BYOB,
         Corkage, Open24Hours, RestaurantsCounterService)

#transforming True, False to 1,0, None to NA, chr in RestaurantsPriceRange2 to int
model2_data <- model2_data %>%
  mutate(
    ByAppointmentOnly = case_when(
      ByAppointmentOnly == "True" ~ 1,
      ByAppointmentOnly == "False" ~ 0,
      TRUE ~ NA_real_
    ),
    BusinessAcceptsCreditCards = case_when(
      BusinessAcceptsCreditCards == "True" ~ 1,
      BusinessAcceptsCreditCards == "False" ~ 0,
      TRUE ~ NA_real_
    ),
    BikeParking = case_when(
      BikeParking == "True" ~ 1,
      BikeParking == "False" ~ 0,
      TRUE ~ NA_real_
    ),
    RestaurantsPriceRange2 = as.integer(as.character(RestaurantsPriceRange2)),
    CoatCheck = case_when(
      CoatCheck == "True" ~ 1,
      CoatCheck == "False" ~ 0,
      TRUE ~ NA_real_
    ),
    RestaurantsTakeOut = case_when(
      RestaurantsTakeOut == "True" ~ 1,
      RestaurantsTakeOut == "False" ~ 0,
      TRUE ~ NA_real_
    ),
    RestaurantsDelivery = case_when(
      RestaurantsDelivery == "True" ~ 1,
      RestaurantsDelivery == "False" ~ 0,
      TRUE ~ NA_real_
    ),
    Caters = case_when(
      Caters == "True" ~ 1,
      Caters == "False" ~ 0,
      TRUE ~ NA_real_
    ),
    WheelchairAccessible = case_when(
      WheelchairAccessible == "True" ~ 1,
      WheelchairAccessible == "False" ~ 0,
      TRUE ~ NA_real_
    ),
    HappyHour = case_when(
      HappyHour == "True" ~ 1,
      HappyHour == "False" ~ 0,
      TRUE ~ NA_real_
    ),
    OutdoorSeating = case_when(
      OutdoorSeating == "True" ~ 1,
      OutdoorSeating == "False" ~ 0,
      TRUE ~ NA_real_
    ),
    HasTV = case_when(
      HasTV == "True" ~ 1,
      HasTV == "False" ~ 0,
      TRUE ~ NA_real_
    ),
    RestaurantsReservations = case_when(
      RestaurantsReservations == "True" ~ 1,
      RestaurantsReservations == "False" ~ 0,
      TRUE ~ NA_real_
    ),
    DogsAllowed = case_when(
      DogsAllowed == "True" ~ 1,
      DogsAllowed == "False" ~ 0,
      TRUE ~ NA_real_
    ),
    GoodForKids = case_when(
      GoodForKids == "True" ~ 1,
      GoodForKids == "False" ~ 0,
      TRUE ~ NA_real_
    ),
    RestaurantsTableService = case_when(
      RestaurantsTableService == "True" ~ 1,
      RestaurantsTableService == "False" ~ 0,
      TRUE ~ NA_real_
    ),
    RestaurantsGoodForGroups = case_when(
      RestaurantsGoodForGroups == "True" ~ 1,
      RestaurantsGoodForGroups == "False" ~ 0,
      TRUE ~ NA_real_
    ),
    DriveThru = case_when(
      DriveThru == "True" ~ 1,
      DriveThru == "False" ~ 0,
      TRUE ~ NA_real_
    ),
    BusinessAcceptsBitcoin = case_when(
      BusinessAcceptsBitcoin == "True" ~ 1,
      BusinessAcceptsBitcoin == "False" ~ 0,
      TRUE ~ NA_real_
    ),
    GoodForDancing = case_when(
      GoodForDancing == "True" ~ 1,
      GoodForDancing == "False" ~ 0,
      TRUE ~ NA_real_
    ),
    AcceptsInsurance = case_when(
      AcceptsInsurance == "True" ~ 1,
      AcceptsInsurance == "False" ~ 0,
      TRUE ~ NA_real_
    ),
    BYOB = case_when(
      BYOB == "True" ~ 1,
      BYOB == "False" ~ 0,
      TRUE ~ NA_real_
    ),
    Corkage = case_when(
      Corkage == "True" ~ 1,
      Corkage == "False" ~ 0,
      TRUE ~ NA_real_
    ),
    Open24Hours = case_when(
      Open24Hours == "True" ~ 1,
      Open24Hours == "False" ~ 0,
      TRUE ~ NA_real_
    ),
    RestaurantsCounterService = case_when(
      RestaurantsCounterService == "True" ~ 1,
      RestaurantsCounterService == "False" ~ 0,
      TRUE ~ NA_real_
    )
  )


# model2_data<- model2_data %>%
#  left_join(tip_data, by = "user_id") - causes many-to-many relationship error

# splitting into train adn test again
set.seed(1)
train1 <- sample(1:nrow(model2_data),nrow(model2_data)-10000)
train_data2<-model2_data[train1,]
test_data2<-model2_data[-train1,]

# Convert 'stars' to factor in for train and test data 2
train_data2$stars <- factor(train_data2$stars, levels = c(1, 2, 3, 4, 5), ordered = TRUE)
test_data2$stars <- factor(test_data2$stars, levels = c(1, 2, 3, 4, 5), ordered = TRUE)


#Modelling: regressions for each dataset 

#Logit
# 2.1 regress stars on variables from business_data
logit_model21 <- polr(stars ~ latitude + longitude + business_stars + review_count + is_open+ByAppointmentOnly + BusinessAcceptsCreditCards + BikeParking+ 
                        RestaurantsPriceRange2 + CoatCheck + RestaurantsTakeOut + 
                        RestaurantsDelivery + Caters + WheelchairAccessible + HappyHour + 
                        OutdoorSeating + HasTV + RestaurantsReservations + DogsAllowed + 
                        GoodForKids + RestaurantsTableService + RestaurantsGoodForGroups + 
                        DriveThru + BusinessAcceptsBitcoin + GoodForDancing+ AcceptsInsurance +
                        BYOB + Corkage + Open24Hours + RestaurantsCounterService,
                       data = train_data2, method = 'logistic')
str(train_data2)
# 2.2 regress stars on variables from review_data_small
logit_model22 <- polr(stars ~ useful + funny + cool, data = train_data2, method = 'logistic')
# 2.3 regress stars on variables from user_data_small
logit_model23 <-polr(stars ~ user_review_count + user_useful + user_funny+ user_cool + fans
                     + average_stars + compliment_hot + compliment_more+ compliment_profile+
                       compliment_cute + compliment_list+compliment_note+compliment_plain+
                       compliment_cool+compliment_funny+compliment_writer+compliment_photos
                     , data = train_data2, method = 'logistic')

#Ridge
install.packages('glmnet')
library(glmnet)
grid<-10^seq(10,-2, length = 100)

#Ridge 1.1 regress stars on business_data
# Define predictors and response for Model 1
x1 <- model.matrix(~ latitude + longitude + business_stars + review_count + is_open + 
                     ByAppointmentOnly + BusinessAcceptsCreditCards + BikeParking + 
                     RestaurantsPriceRange2 + CoatCheck + RestaurantsTakeOut + 
                     RestaurantsDelivery + Caters + WheelchairAccessible + HappyHour + 
                     OutdoorSeating + HasTV + RestaurantsReservations + DogsAllowed + 
                     GoodForKids + RestaurantsTableService + RestaurantsGoodForGroups + 
                     DriveThru + BusinessAcceptsBitcoin + GoodForDancing + AcceptsInsurance + 
                     BYOB + Corkage + Open24Hours + RestaurantsCounterService - 1, 
                   data = train_data2)  # '-1' to exclude intercept
y1 <- train_data2$stars

# Fit ridge regression model
ridge_model1 <- glmnet(x1, y1, alpha = 0)  # alpha = 0 for ridge regression

#Ridge 1.2 regress stars on review_data_small

#Ridge 2.3 regress stars on user
# Boosting? after measuring fit 