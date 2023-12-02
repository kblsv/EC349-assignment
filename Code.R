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
    user_useful = useful,
    user_funny = funny,
    user_cool = cool,
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

#merge the necessary datasets after renaming any clashing variables (code above)
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

# Convert 'stars' to factor in for train and test data 2 if using logit
train_data2$stars <- factor(train_data2$stars, levels = c(1, 2, 3, 4, 5), ordered = TRUE)
test_data2$stars <- factor(test_data2$stars, levels = c(1, 2, 3, 4, 5), ordered = TRUE)


#Modelling: regressions for each dataset 

#Logit
# 2.1 regress stars on variables from business_data
logit_model21 <- polr(stars ~ latitude + longitude + business_stars + review_count + is_open,
                       data = train_data2, method = 'logistic')

logit_model211 <- polr(stars~ByAppointmentOnly + BusinessAcceptsCreditCards + BikeParking+ 
                         RestaurantsPriceRange2 + CoatCheck + RestaurantsTakeOut + 
                         RestaurantsDelivery + Caters + WheelchairAccessible + HappyHour + 
                         OutdoorSeating + HasTV + RestaurantsReservations + DogsAllowed + 
                         GoodForKids + RestaurantsTableService + RestaurantsGoodForGroups + 
                         DriveThru + BusinessAcceptsBitcoin + GoodForDancing +
                         BYOB + Corkage,
                       data = train_data2, method = 'logistic')

# 2.2 regress stars on variables from review_data_small - causes error, potentially due to high skewness, many zeros
logit_model22 <- polr(stars ~ useful + funny + cool, data = train_data2, method = 'logistic')
# 2.3 regress stars on variables from user_data_small
logit_model23 <-polr(stars ~ user_review_count + user_useful + user_funny+ user_cool + fans
                     + average_stars + compliment_hot + compliment_more+ compliment_profile+
                       compliment_cute + compliment_list+compliment_note+compliment_plain+
                       compliment_cool+compliment_funny+compliment_writer+compliment_photos
                     , data = train_data2, method = 'logistic')

# Boosting? after measuring fit 

# Evaluation: Validation 

# logit_model11 
#predict most likely class
test_predictions11 <- predict(logit_model11, newdata = test_data, type = "class")
#ensuring test_predictions11 has the same levels as test_data$stars
test_predictions11 <- factor(test_predictions11, levels = levels(test_data$stars))
# Convert test_predictions11 to an ordered factor
test_predictions11 <- factor(test_predictions11, 
                                     levels = levels(test_data$stars), 
                                     ordered = TRUE)
accuracy11 <- sum(test_predictions11 == test_data$stars) / nrow(test_data)
print(accuracy11) #get NA, so exclude missing values:
# Calculate accuracy excluding NA values
valid_cases <- !is.na(test_predictions11) & !is.na(test_data$stars)
accuracy11 <- sum(test_predictions11[valid_cases] == test_data$stars[valid_cases]) / sum(valid_cases)
print(accuracy11)
#compare to baseline
#most common class in the traindata
most_common_class <- names(sort(table(train_data$stars), decreasing = TRUE))[1]

# Create baseline predictions
baseline_predictions <- rep(most_common_class, nrow(test_data))

# Calculate baseline accuracy
baseline_accuracy <- sum(baseline_predictions == test_data$stars) / nrow(test_data)
print(baseline_accuracy)

#confusion matrix
confusionMatrix(test_predictions11, test_data$stars)

#Univariate analysis: splitting variables into groups and calculating mean ratings
# binning useful, funny, cool with custom bins due to large amount of values near 0 
model2_data$useful_bins <- cut(model2_data$useful, 
                               breaks = c(-Inf, 0, 5, 20, Inf), 
                               include.lowest = TRUE, 
                               labels = c("0", "1-5", "6-20", "21+"))
mean_stars_by_useful <- aggregate(stars ~ useful_bins, data = model2_data, mean)
print(mean_stars_by_useful)

model2_data$funny_bins <- cut(model2_data$funny, 
                              breaks = c(-Inf, 0, 5, 20, Inf), 
                              include.lowest = TRUE, 
                              labels = c("0", "1-5", "6-20", "21+"))
mean_stars_by_funny <- aggregate(stars ~ funny_bins, data = model2_data, mean)
print(mean_stars_by_funny)

model2_data$cool_bins <- cut(model2_data$cool, 
                             breaks = c(-Inf, 0, 5, 20, Inf), 
                             include.lowest = TRUE, 
                             labels = c("0", "1-5", "6-20", "21+"))
mean_stars_by_cool <- aggregate(stars ~ cool_bins, data = model2_data, mean)
print(mean_stars_by_cool)
#correlations for usefuly, funny, cool
corr_review_data <- model2_data[, c("useful","cool","funny")]
corr_review_matrix <- cor(corr_review_data, use = "complete.obs")
print(corr_review_matrix)

#binning user data
#observing the distributions to decide on which points to cut the data
summary(model2_data$user_review_count)
summary(model2_data$user_useful)
summary(model2_data$user_funny)
summary(model2_data$user_cool)
summary(model2_data$fans)
summary(model2_data$average_stars)

model2_data$user_review_count_bins <- cut(model2_data$user_review_count, 
                                          breaks = c(-Inf, 0, 10, 50, 100, Inf), 
                                          include.lowest = TRUE, 
                                          labels = c("0", "1-10", "11-50", "51-100", "101+"))
mean_stars_by_user_review_count <- aggregate(stars ~ user_review_count_bins, data = model2_data, mean)
print(mean_stars_by_user_review_count)

model2_data$user_useful_bins <- cut(model2_data$user_useful, 
                                    breaks = c(-Inf, 50, 500, 1000, 5000, Inf), 
                                    include.lowest = TRUE, 
                                    labels = c("0-50", "51-500", "501-1000", "1001-5000", "5000+"))
mean_stars_by_user_useful <- aggregate(stars ~ user_useful_bins, data = model2_data, mean)
print(mean_stars_by_user_useful)

model2_data$user_funny_bins <- cut(model2_data$user_funny, 
                                   breaks = c(-Inf, 5, 25, 50, 100, Inf), 
                                   include.lowest = TRUE, 
                                   labels = c("0-5", "6-25", "26-50", "51-100", "100+"))
mean_stars_by_user_funny <- aggregate(stars ~ user_funny_bins, data = model2_data, mean)
print(mean_stars_by_user_funny)

model2_data$user_cool_bins <- cut(model2_data$user_cool, 
                                  breaks = c(-Inf, 5, 25, 50, 100, Inf), 
                                  include.lowest = TRUE, 
                                  labels = c("0-5", "6-25", "26-50", "51-100", "100+"))
mean_stars_by_user_cool <- aggregate(stars ~ user_cool_bins, data = model2_data, mean)
print(mean_stars_by_user_cool)

model2_data$fans_bins <- cut(model2_data$fans, 
                             breaks = c(-Inf, 1, 5, 10, 50, Inf), 
                             include.lowest = TRUE, 
                             labels = c("0", "1-5", "6-10", "11-50", "50+"))
mean_stars_by_fans <- aggregate(stars ~ fans_bins, data = model2_data, mean)
print(mean_stars_by_fans)

model2_data$average_stars_bins <- cut(model2_data$average_stars, 
                                      breaks = c(1, 2, 3, 4, 5), 
                                      include.lowest = TRUE, 
                                      right = FALSE)
mean_stars_by_average_stars <- aggregate(stars ~ average_stars_bins, data = model2_data, mean)
print(mean_stars_by_average_stars)

#correlations for user_data
corr_user_data <- model2_data[, c("user_review_count","user_useful","user_funny","user_cool","fans")]
corr_user_matrix <- cor(corr_user_data, use = "complete.obs")
print(corr_user_matrix)

#binning compliment data from user data
#again, a lot of zeros and skew to higher values, so I focus on concentration of lower values
summary(model2_data$compliment_hot)
summary(model2_data$compliment_more)
summary(model2_data$compliment_profile)
summary(model2_data$compliment_cute)
summary(model2_data$compliment_list)
summary(model2_data$compliment_note)
summary(model2_data$compliment_plain)
summary(model2_data$compliment_cool)
summary(model2_data$compliment_funny)
summary(model2_data$compliment_writer)
summary(model2_data$compliment_photos)

model2_data$compliment_hot_bins <- cut(model2_data$compliment_hot, 
                                       breaks = c(-Inf, 0, 1, 5, 20, Inf), 
                                       include.lowest = TRUE, 
                                       labels = c("0", "1", "2-5", "6-20", "20+"))
mean_stars_by_compliment_hot <- aggregate(stars ~ compliment_hot_bins, data = model2_data, mean)
print(mean_stars_by_compliment_hot)

model2_data$compliment_more_bins <- cut(model2_data$compliment_more, 
                                        breaks = c(-Inf, 0, 1, 10, 50, Inf), 
                                        include.lowest = TRUE, 
                                        labels = c("0", "1", "2-10", "11-50", "50+"))
mean_stars_by_compliment_more <- aggregate(stars ~ compliment_more_bins, data = model2_data, mean)
print(mean_stars_by_compliment_more)

model2_data$compliment_profile_bins <- cut(model2_data$compliment_profile, 
                                           breaks = c(-Inf, 0, 1, 5, 20, Inf), 
                                           include.lowest = TRUE, 
                                           labels = c("0", "1", "2-5", "6-20", "20+"))
mean_stars_by_compliment_profile <- aggregate(stars ~ compliment_profile_bins, data = model2_data, mean)
print(mean_stars_by_compliment_profile)

model2_data$compliment_cute_bins <- cut(model2_data$compliment_cute, 
                                        breaks = c(-Inf, 0, 1, 5, 20, Inf), 
                                        include.lowest = TRUE, 
                                        labels = c("0", "1", "2-5", "6-20", "20+"))
mean_stars_by_compliment_cute <- aggregate(stars ~ compliment_cute_bins, data = model2_data, mean)
print(mean_stars_by_compliment_cute)

model2_data$compliment_list_bins <- cut(model2_data$compliment_list, 
                                        breaks = c(-Inf, 0, 1, 5, 10, Inf), 
                                        include.lowest = TRUE, 
                                        labels = c("0", "1", "2-5", "6-10", "10+"))
mean_stars_by_compliment_list <- aggregate(stars ~ compliment_list_bins, data = model2_data, mean)
print(mean_stars_by_compliment_list)

model2_data$compliment_note_bins <- cut(model2_data$compliment_note, 
                                        breaks = c(-Inf, 0, 5, 20, 50, Inf), 
                                        include.lowest = TRUE, 
                                        labels = c("0", "1-5", "6-20", "21-50", "50+"))
mean_stars_by_compliment_note <- aggregate(stars ~ compliment_note_bins, data = model2_data, mean)
print(mean_stars_by_compliment_note)

model2_data$compliment_plain_bins <- cut(model2_data$compliment_plain, 
                                         breaks = c(-Inf, 0, 5, 20, 50, Inf), 
                                         include.lowest = TRUE, 
                                         labels = c("0", "1-5", "6-20", "21-50", "50+"))
mean_stars_by_compliment_plain <- aggregate(stars ~ compliment_plain_bins, data = model2_data, mean)
print(mean_stars_by_compliment_plain)

model2_data$compliment_cool_bins <- cut(model2_data$compliment_cool, 
                                        breaks = c(-Inf, 0, 5, 20, 50, Inf), 
                                        include.lowest = TRUE, 
                                        labels = c("0", "1-5", "6-20", "21-50", "50+"))
mean_stars_by_compliment_cool <- aggregate(stars ~ compliment_cool_bins, data = model2_data, mean)
print(mean_stars_by_compliment_cool)

model2_data$compliment_funny_bins <- cut(model2_data$compliment_funny, 
                                         breaks = c(-Inf, 0, 5, 20, 50, Inf), 
                                         include.lowest = TRUE, 
                                         labels = c("0", "1-5", "6-20", "21-50", "50+"))
mean_stars_by_compliment_funny <- aggregate(stars ~ compliment_funny_bins, data = model2_data, mean)
print(mean_stars_by_compliment_funny)

model2_data$compliment_writer_bins <- cut(model2_data$compliment_writer, 
                                          breaks = c(-Inf, 0, 5, 20, 50, Inf), 
                                          include.lowest = TRUE, 
                                          labels = c("0", "1-5", "6-20", "21-50", "50+"))
mean_stars_by_compliment_writer <- aggregate(stars ~ compliment_writer_bins, data = model2_data, mean)
print(mean_stars_by_compliment_writer)

model2_data$compliment_photos_bins <- cut(model2_data$compliment_photos, 
                                          breaks = c(-Inf, 0, 5, 20, 50, Inf), 
                                          include.lowest = TRUE, 
                                          labels = c("0", "1-5", "6-20", "21-50", "50+"))
mean_stars_by_compliment_photos <- aggregate(stars ~ compliment_photos_bins, data = model2_data, mean)
print(mean_stars_by_compliment_photos)

#Correlations within compliment data (checking if I can use multiple without multicollinearity)
# Subset the relevant columns from your data
compliment_data <- model2_data[, c("compliment_hot","compliment_more","compliment_plain","compliment_cool","compliment_writer","compliment_photos")]
corr_compliment_matrix <- cor(compliment_data, use = "complete.obs")
print(corr_compliment_matrix)

#binning business data
model2_data$business_stars_bins <- cut(model2_data$business_stars, 
                                       breaks = c(1, 2, 3, 4, 5), 
                                       include.lowest = TRUE, 
                                       right = FALSE)
mean_stars_by_business_stars <- aggregate(stars ~business_stars_bins, data = model2_data, mean)
print(mean_stars_by_business_stars)

model2_data$review_count_bins <- cut(model2_data$review_count, 
                                     breaks = c(-Inf, 50, 150, 400, 1000, Inf), 
                                     include.lowest = TRUE, 
                                     labels = c("0-50", "51-150", "151-400", "401-1000", "1001+"))
mean_stars_by_review_count <- aggregate(stars ~ review_count_bins, data = model2_data, mean)
print(mean_stars_by_review_count)

mean_stars_by_is_open <- aggregate(stars ~ is_open, data = model2_data, mean)
print(mean_stars_by_is_open)

mean_stars_by_appointment <- aggregate(stars ~ ByAppointmentOnly, data = model2_data, mean)
print(mean_stars_by_appointment)

mean_stars_by_credit_cards <- aggregate(stars ~ BusinessAcceptsCreditCards, data = model2_data, mean)
print(mean_stars_by_credit_cards)

mean_stars_by_bikeparking <- aggregate(stars ~ BikeParking, data = model2_data, mean)
print(mean_stars_by_bikeparking)

mean_stars_by_coatcheck <- aggregate(stars ~ CoatCheck, data = model2_data, mean)
print(mean_stars_by_coatcheck)

mean_stars_by_takeout <- aggregate(stars ~ RestaurantsTakeOut, data = model2_data, mean)
print(mean_stars_by_takeout)

mean_stars_by_delivery <- aggregate(stars ~ RestaurantsDelivery, data = model2_data, mean)
print(mean_stars_by_delivery)

mean_stars_by_caters <- aggregate(stars ~ Caters, data = model2_data, mean)
print(mean_stars_by_caters)

mean_stars_by_wheelchair <- aggregate(stars ~ WheelchairAccessible, data = model2_data, mean)
print(mean_stars_by_wheelchair)

mean_stars_by_happyhour <- aggregate(stars ~ HappyHour, data = model2_data, mean)
print(mean_stars_by_happyhour)

mean_stars_by_outdoor <- aggregate(stars ~ OutdoorSeating, data = model2_data, mean)
print(mean_stars_by_outdoor)

mean_stars_by_hastv <- aggregate(stars ~ HasTV, data = model2_data, mean)
print(mean_stars_by_hastv)

mean_stars_by_reservations <- aggregate(stars ~ RestaurantsReservations, data = model2_data, mean)
print(mean_stars_by_reservations)

mean_stars_by_dogsallowed <- aggregate(stars ~ DogsAllowed, data = model2_data, mean)
print(mean_stars_by_dogsallowed)

mean_stars_by_goodforkids <- aggregate(stars ~ GoodForKids, data = model2_data, mean)
print(mean_stars_by_goodforkids)

mean_stars_by_tableservice <- aggregate(stars ~ RestaurantsTableService, data = model2_data, mean)
print(mean_stars_by_tableservice)

mean_stars_by_goodforgroups <- aggregate(stars ~ RestaurantsGoodForGroups, data = model2_data, mean)
print(mean_stars_by_goodforgroups)

mean_stars_by_drivethru <- aggregate(stars ~ DriveThru, data = model2_data, mean)
print(mean_stars_by_drivethru)

mean_stars_by_bitcoin <- aggregate(stars ~ BusinessAcceptsBitcoin, data = model2_data, mean)
print(mean_stars_by_bitcoin)

mean_stars_by_dancing <- aggregate(stars ~ GoodForDancing, data = model2_data, mean)
print(mean_stars_by_dancing)

mean_stars_by_insurance <- aggregate(stars ~ AcceptsInsurance, data = model2_data, mean)
print(mean_stars_by_insurance)

mean_stars_by_byob <- aggregate(stars ~ BYOB, data = model2_data, mean)
print(mean_stars_by_byob)

mean_stars_by_corkage <- aggregate(stars ~ Corkage, data = model2_data, mean)
print(mean_stars_by_corkage)

mean_stars_by_open24hours <- aggregate(stars ~ Open24Hours, data = model2_data, mean)
print(mean_stars_by_open24hours)

mean_stars_by_counterservice <- aggregate(stars ~ RestaurantsCounterService, data = model2_data, mean)
print(mean_stars_by_counterservice)

mean_stars_by_price_range <- aggregate(stars ~ RestaurantsPriceRange2, data = model2_data, mean)
print(mean_stars_by_price_range)

#correlations for attributes
binary <- model2_data[, c("Open24Hours", "DriveThru", "AcceptsInsurance", 
                          "GoodForDancing", "HasTV", "HappyHour", "BikeParking", 
                          "WheelchairAccessible", "OutdoorSeating", "RestaurantsCounterService", 
                          "RestaurantsPriceRange2", "BusinessAcceptsCreditCards", 
                          "BusinessAcceptsBitcoin", "RestaurantsTakeOut", "BYOB")]
corr_binary <- cor(binary, use = "pairwise.complete.obs") 
print(corr_binary)

#removing bins
model2_data$review_count_bins <- NULL
model2_data$business_stars_bins <- NULL
model2_data$compliment_hot_bins <- NULL
model2_data$compliment_photos_bins <- NULL
model2_data$compliment_writer_bins <- NULL
model2_data$compliment_funny_bins <- NULL
model2_data$compliment_cool_bins <- NULL
model2_data$compliment_plain_bins <- NULL
model2_data$compliment_note_bins <- NULL
model2_data$compliment_list_bins <- NULL
model2_data$compliment_cute_bins <- NULL
model2_data$compliment_profile_bins <- NULL
model2_data$compliment_more_bins <- NULL
model2_data$average_stars_bins <- NULL
model2_data$fans_bins <- NULL
model2_data$user_cool_bins <- NULL
model2_data$user_funny_bins <- NULL
model2_data$user_useful_bins <- NULL
model2_data$user_review_count_bins <- NULL


# Logit Model 2 
logit_model21 <- polr(stars ~ average_stars + business_stars + Open24Hours
                      +cool + user_review_count + fans + compliment_photos +
                        review_count, data=train_data2, method='logistic')
logit_model22 <- polr(stars ~ average_stars + business_stars + Open24Hours
                      +cool + user_review_count + fans + compliment_photos +
                        review_count+Open24Hours+HappyHour+Caters+
                        HasTV + BikeParking, data=train_data2, method='logistic')

#logit_model22 <- polr(stars ~ average_stars + business_stars + Open24Hours
#                      +cool + user_review_count + fans + compliment_photos +
 #                       review_count + DriveThru + AcceptsInsurance+ GoodForDancing+ 
  #                      HasTV + BikeParking, data=train_data2, method='logistic')

#logit_model23 <- polr(stars ~ average_stars + business_stars + Open24Hours
   #                   +cool + user_review_count + fans + compliment_photos +
    #                    review_count + Open24Hours +AcceptsInsurance +GoodForDancing+
     #                   HappyHour+WheelchairAccessible+RestaurantsPriceRange2+
      #                  BusinessAcceptsCreditCards, data=train_data2, method='logistic')

summary(logit_model21)
summary(logit_model22)

# Validation of logit_model22

#predict most likely class
test_predictions22 <- predict(logit_model22, newdata = test_data2, type = "class")
#ensuring test_predictions22 has the same levels as test_data2$stars
test_predictions22 <- factor(test_predictions22, levels = levels(test_data2$stars))
# Convert test_predictions22 to an ordered factor
test_predictions22 <- factor(test_predictions22, 
                             levels = levels(test_data2$stars), 
                             ordered = TRUE)
accuracy22 <- sum(test_predictions22 == test_data2$stars) / nrow(test_data2)
print(accuracy22) #get NA, so exclude missing values:
# Calculate accuracy excluding NA values
valid_cases2 <- !is.na(test_predictions22) & !is.na(test_data2$stars)
accuracy22 <- sum(test_predictions22[valid_cases2] == test_data2$stars[valid_cases2]) / sum(valid_cases2)
print(accuracy22)
#compare to baseline
#most common class in the traindata2
most_common_class2 <- names(sort(table(train_data2$stars), decreasing = TRUE))[1]

# Create baseline predictions
baseline_predictions2 <- rep(most_common_class2, nrow(test_data2))

# Calculate baseline accuracy
baseline_accuracy2 <- sum(baseline_predictions2 == test_data2$stars) / nrow(test_data2)
print(baseline_accuracy2)

#confusion matrix
confusionMatrix(test_predictions22, test_data2$stars)

summary(logit_model22)
str(train_data2)
str(test_data2)
summary(train_data2)
summary(test_data2)

#for logit_model21
#predict most likely class
test_predictions21 <- predict(logit_model21, newdata = test_data2, type = "class")
#ensuring test_predictions22 has the same levels as test_data2$stars
test_predictions21 <- factor(test_predictions21, levels = levels(test_data2$stars))
# Convert test_predictions22 to an ordered factor
test_predictions21 <- factor(test_predictions21, 
                             levels = levels(test_data2$stars), 
                             ordered = TRUE)
accuracy21 <- sum(test_predictions21 == test_data2$stars) / nrow(test_data2)
print(accuracy21) #get NA, so exclude missing values:
# Calculate accuracy excluding NA values
valid_cases21 <- !is.na(test_predictions21) & !is.na(test_data2$stars)
accuracy21 <- sum(test_predictions21[valid_cases21] == test_data2$stars[valid_cases21]) / sum(valid_cases21)
print(accuracy21)
#compare to baseline
#most common class in the traindata2
most_common_class21 <- names(sort(table(train_data2$stars), decreasing = TRUE))[1]

# Create baseline predictions
baseline_predictions21 <- rep(most_common_class21, nrow(test_data2))

# Calculate baseline accuracy
baseline_accuracy21 <- sum(baseline_predictions21 == test_data2$stars) / nrow(test_data2)
print(baseline_accuracy21)

#confusion matrix
confusionMatrix(test_predictions21, test_data2$stars)

summary(test_predictions21)
summary(test_predictions11)

#attempting decision tree
install.packages('tree')
library(tree)
#modelling
decisiontree1<-tree(stars ~ average_stars + business_stars + Open24Hours, data=train_data2)
#2nd one accuracy of 0.5127 
decisiontree1<-tree(stars~average_stars + business_stars + Open24Hours
                   +cool + user_review_count + fans + compliment_photos +
                     review_count, data=train_data2)
#first one accuracy of 0.4746 decisiontree1<-tree(stars ~ average_stars + business_stars + Open24Hours
 #                   +cool + user_review_count + fans + compliment_photos +
  #                    review_count+Open24Hours+HappyHour+Caters+
   #                   HasTV + BikeParking, data=train_data2)
plot(decisiontree1)
text(decisiontree1,pretty=1)
#validation
test_predictions3 <-predict(decisiontree1,newdata=test_data2,type='class')
summary(test_predictions3)
test_predictions3 <-factor(test_predictions3, levels = levels(test_data2$stars), ordered = TRUE)
accuracy3<-sum(test_predictions3==test_data2$stars)/nrow(test_data2)
print(accuracy3)

#attempting random forest
install.packages('randomForest')
library(randomForest)

randomforest1<-randomForest(stars ~ business_stars +cool + review_count +is_open
                            +RestaurantsPriceRange2,
                            data=train_data2,
                            ntree=50,
                            mtry=2)

test_predictions4<-predict(randomforest1,newdata=test_data2)
test_predictions4 <-factor(test_predictions4, levels = levels(test_data2$stars), ordered = TRUE)
accuracy4<-sum(test_predictions4==test_data2$stars)/nrow(test_data2)
print(accuracy4)
library(caret)
confusionMatrix <-table(Predicted=test_predictions4, Actual = test_data2$stars)
print(confusionMatrix)
#too many missing values 
#Use missForest instead
#missForest assumes Missing At Random - testing if it is the case
#seeing if it is Missing Completely At Random
install.packages("naniar")
library(naniar)
mcar_test <- naniar::mcar_test(train_data2)
print(mcar_test)
#p.value of 0 shows that the null hypothesis of MCAR is rejected. 
#seeing if it is just Missing At Random
install.packages("ggplot2")
library(ggplot2)
#visualising pattern of missing data
gg_miss_var(train_data2)
vis_miss(train_data2)#data too large so downsampling
train_data2_sample <- train_data2 %>% 
  slice_sample(n = 10000)
vis_miss(train_data2_sample)

#proportion of missing data relative to observed
colSums(is.na(train_data2))/nrow(train_data2)
#using correlation analysis 

imputed_data <- missForest(train_data2)



