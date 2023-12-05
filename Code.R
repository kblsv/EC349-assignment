#Installed homebrew, installed git, initialised git, pushed the project onto GitHub repository.
setwd("/Users/ekaterina/Documents/EC349 project")

install.packages('jsonlite')
install.packages('ggplot2')
install.packages('caret')
install.packages("MASS")
install.packages('tree')
install.packages('randomForest')
install.packages("naniar")
install.packages('xgboost')

library(jsonlite)
library(ggplot2)
library(dplyr)
library(caret)
library(MASS)
library(tree)
library(randomForest)
library(naniar)
library(xgboost)


#LOADING DATA


# Clear
cat("\014")  
# rm(list=ls())

# Set Directory as appropriate
setwd("/Users/ekaterina/Documents/EC349 project/data")

load("yelp_review_small.Rda")
load("yelp_user_small.Rda")
business_data <- stream_in(file("yelp_academic_dataset_business.json")) 
checkin_data  <- stream_in(file("yelp_academic_dataset_checkin.json")) 
tip_data  <- stream_in(file("yelp_academic_dataset_tip.json"))


#ANALYTIC APPROACH


#DESCRIPTIVE

str(review_data_small)
summary(review_data_small$stars)
summary(review_data_small$useful)
summary(review_data_small$funny)
summary(review_data_small$cool)
summary(user_data_small$review_count)
summary(user_data_small$average_stars)

#stars histogram
ggplot(review_data_small, aes(x=stars)) +
  geom_histogram(binwidth=1, fill='blue', color='black') +
  ggtitle("Distribution of Stars Ratings")

#review_count boxplot
ggplot(user_data_small, aes(y = review_count)) +
  geom_boxplot(fill = "blue", color = "black") +
  ggtitle("Boxplot of Review Count")

#average_stars boxplot
ggplot(user_data_small, aes(y = average_stars)) +
  geom_boxplot(fill = "green", color = "black") +
  ggtitle("Boxplot of Average Stars")

# DIAGNOSTIC

#Correlations: review data only 
corr_review_variables<-c("useful","funny","cool")
corr_review<- sapply(corr_review_variables,function(var) 
  {cor(review_data_small$stars,review_data_small[[var]],use = "complete.obs")})

#Correlations: review data and **user data**

#renaming conflicting columns in user_data_small before merging
user_data_small<-user_data_small %>%
  rename(user_useful=useful,user_funny= funny,user_cool =cool,user_review_count=review_count)

#merging review_data_small with user_data_small
combined_user_review_data<-merge(review_data_small,user_data_small, by = 'user_id')

corr_user_variables<-c("review_count","user_useful","user_funny","user_cool","fans","average_stars")
corr_user <-sapply(corr_user_variables,function(var) {cor(combined_user_review_data$stars,combined_user_review_data[[var]], use ="complete.obs")})

# Correlations: review data and **tip data**

#rename conflicting columns
tip_data<-tip_data %>%
  rename(tip_business_id=business_id,tip_text =text,tip_date=date)
#merge
combined_tip_review_data<-merge(review_data_small,tip_data,by="user_id")
corr_tip_variables<-c("compliment_count") 
corr_tip<-sapply(corr_tip_variables,function(var) {cor(combined_tip_review_data$stars,combined_tip_review_data[[var]],use="complete.obs")})

# Correlations: review data and *business data** 
business_data<-business_data %>%
  rename(business_stars =stars )
combined_review_business_data<-merge(review_data_small,business_data,by ='business_id')

#corrrelations for business numerical/int variables 
corr_numerical_business_variables<-c("latitude", "longitude", "business_stars", 'review_count', 'is_open')
corr_numerical_business<-sapply(corr_numerical_business_variables,function(var) {cor(combined_review_business_data$stars,combined_review_business_data[[var]], use="complete.obs")})

#correlations for business non-numerical variables
#confirming unique categories for each columns
unique_values_in_attributes<-lapply(business_data$attributes,unique)
View(unique_values_in_attributes)
write.csv(unique_values_in_attributes,"unique_values_in_attributes.csv",row.names=FALSE)
attributes_df<-data.frame(lapply(unique_values_in_attributes,function(x) paste(x,collapse=", ")))
write.csv(attributes_df,"unique_values_in_attributes.csv",row.names=FALSE)

#Converting True/False to binary attributes 

#extract attributes that are nested and converting to dataframe
attributes_df<-purrr::map_df(combined_review_business_data$attributes, ~.x)
#Remove non binary
attributes_df<-attributes_df %>% select(all_of(attributes_to_correlate))
#convert to Numeric; True = 1,False = 0, NA/NULL remains NA
attributes_df<-attributes_df %>%
  mutate(across(everything(),~ifelse(. =="True", 1, ifelse(. =="False", 0, ifelse(. =="None",NA,NA)))))
# confirming all values are 1, 0, or NA: 
unique_values_in_attributes_df<-lapply(attributes_df,unique)
names(unique_values_in_attributes_df)<-names(attributes_df)
View(unique_values_in_attributes_df)
#adding stars to attributes_df
attributes_df$stars<-combined_review_business_data$stars
corr_binary_business_variables<-setdiff(names(attributes_df),"stars")
corr_binary_business<-sapply(corr_binary_business_variables,function(attr) {cor(attributes_df[[attr]],attributes_df$stars,use='complete.obs')})


# MODELLING


#creating separate dataframe with data for model 1 
model1_data<-review_data_small %>%
  left_join(user_data_small,by='user_id')%>%
  left_join(business_data %>%mutate(DriveThru=attributes$DriveThru,Open24Hours= attributes$Open24Hours),by = 'business_id')

model1_data<-model1_data%>%
  mutate(DriveThru =ifelse(is.na(attributes$DriveThru),NA,attributes$DriveThru),Open24Hours = ifelse(is.na(attributes$Open24Hours),NA,attributes$Open24Hours))

model1_data<-model1_data %>% dplyr::select('stars','average_stars','business_stars','DriveThru','Open24Hours')

model1_data<-model1_data %>%
  mutate(Open24Hours = case_when(Open24Hours=="True"~1,Open24Hours =="False"~0,TRUE~NA_real_),
          DriveThru= case_when9DriveThru=="True"~ 1,DriveThru=="False"~0,DriveThru=="None"~NA_real_,TRUE ~NA_real_)

#splitting the data, with the test dataset containing 10,000 observations
set.seed(1)
train <- sample(1:nrow(model1_data),nrow(model1_data)-10000)
train_data<-model1_data[train,]
test_data<-model1_data[-train,]

#making 'stars' an ordered factor
train_data$stars<-factor(train_data$stars,levels=c(1,2,3,4,5),ordered=TRUE)
test_data$stars<-factor(test_data$stars, levels=c(1,2, 3, 4,5),ordered=TRUE)

#MODEL 1: Ordered multinomial logit

logit_model11<-polr(stars ~ average_stars + business_stars + Open24Hours, data =train_data, method ='logistic')
logit_model12<-polr(stars ~ average_stars + business_stars + DriveThru, data =train_data, method ='logistic')
logit_model13<-polr(stars ~ average_stars + business_stars + DriveThru +Open24Hours, data =train_data, method ='logistic') # causes an error 
logit_model14<-polr(stars ~ average_stars + business_stars, data =train_data, method ='logistic')
logit_model15<-polr(stars ~ average_stars, data = train_data, method ='logistic')
logit_model16<-polr(stars ~business_stars, data = train_data, method = 'logistic')

summary(logit_model11) 
summary(logit_model12)
summary(logit_model13)
summary(logit_model14)
summary(logit_model15)
summary(logit_model16)

#observing missing data more closely 
install.packages("naniar")
library(naniar)
gg_miss_var(model1_data)
miss_var_summary(model1_data)

#model2_data
# creating a dataframe containining all variables (broken down in steps due to memory usage)
#rename clashing variables 
user_data_small<-user_data_small %>%
  rename(user_useful=useful,user_funny= funny,user_cool =cool,user_review_count=review_count)
tip_data<-tip_data %>%
  rename(tip_business_id=business_id,tip_text =text,tip_date=date)
business_data<-business_data %>%
  rename(business_stars =stars )
#merge the necessary datasets 
model2_data<-review_data_small %>%
  left_join(user_data_small,by="user_id") %>%
  left_join(business_data %>% mutate(
                ByAppointmentOnly= attributes$ByAppointmentOnly,
                BusinessAcceptsCreditCards= attributes$BusinessAcceptsCreditCards,
                BikeParking= attributes$BikeParking ,
                RestaurantsPriceRange2= attributes$RestaurantsPriceRange2,
                CoatCheck= attributes$CoatCheck,
                RestaurantsTakeOut= attributes$RestaurantsTakeOut,
                RestaurantsDelivery= attributes$RestaurantsDelivery,
                Caters = attributes$Caters,
                WheelchairAccessible= attributes$WheelchairAccessible,
                HappyHour=attributes$HappyHour,
                OutdoorSeating= attributes$OutdoorSeating,
                HasTV=attributes$HasTV,
                RestaurantsReservations = attributes$RestaurantsReservations,
                DogsAllowed =attributes$DogsAllowed,
                GoodForKids=attributes$GoodForKids,
                RestaurantsTableService= attributes$RestaurantsTableService,
                RestaurantsGoodForGroups= attributes$RestaurantsGoodForGroups,
                DriveThru=attributes$DriveThru,
                BusinessAcceptsBitcoin= attributes$BusinessAcceptsBitcoin,
                GoodForDancing= attributes$GoodForDancing,
                AcceptsInsurance= attributes$AcceptsInsurance,
                BYOB =attributes$BYOB,
                Corkage=attributes$Corkage,
                Open24Hours=attributes$Open24Hours,
                RestaurantsCounterService=attributes$RestaurantsCounterService),by="business_id")

#selecting only necessary columns (non-character)
model2_data<- model2_data %>%
  select(stars,useful,funny,cool, user_review_count,user_useful,user_funny,user_cool, fans, average_stars,
         compliment_hot, compliment_more,compliment_profile,compliment_cute,compliment_list,compliment_note,
         compliment_plain,compliment_cool,compliment_funny, compliment_writer,compliment_photos,   latitude,
         longitude,business_stars,review_count,is_open,ByAppointmentOnly,BusinessAcceptsCreditCards,BikeParking,
          RestaurantsPriceRange2, CoatCheck,RestaurantsTakeOut,RestaurantsDelivery, Caters, WheelchairAccessible,
         HappyHour, OutdoorSeating,HasTV,RestaurantsReservations,DogsAllowed,GoodForKids, RestaurantsTableService,
         RestaurantsGoodForGroups,DriveThru,BusinessAcceptsBitcoin,GoodForDancing,AcceptsInsurance, BYOB, 
         Corkage,Open24Hours,RestaurantsCounterService)

#transforming True, False to 1,0, None to NA, chr in RestaurantsPriceRange2 to int
model2_data<-model2_data %>%
  mutate(ByAppointmentOnly=case_when(ByAppointmentOnly=="True" ~ 1, ByAppointmentOnly=="False" ~ 0,TRUE ~ NA_real_),
    BusinessAcceptsCreditCards=case_when(BusinessAcceptsCreditCards=='True' ~ 1, BusinessAcceptsCreditCards=='False' ~ 0, TRUE~ NA_real_),
    BikeParking=case_when(BikeParking=="True" ~ 1, BikeParking=='False'~ 0,TRUE~ NA_real_),
    RestaurantsPriceRange2=as.integer(as.character(RestaurantsPriceRange2)),
    CoatCheck=case_when(CoatCheck=="True" ~ 1, oatCheck=="False"~ 0, TRUE~ NA_real_),
    RestaurantsTakeOut=case_when(RestaurantsTakeOut=="True" ~1,RestaurantsTakeOut=="False"~ 0, TRUE ~ NA_real_),
    RestaurantsDelivery=case_when(RestaurantsDelivery=="True"~ 1,RestaurantsDelivery=="False"  ~ 0, TRUE~ NA_real_),
    Caters=case_when(Caters=="True" ~ 1,Caters=="False" ~ 0, TRUE ~ NA_real_),
    WheelchairAccessible=case_when(WheelchairAccessible=="True"~ 1,WheelchairAccessible=="False"~ 0, TRUE ~ NA_real_),
    HappyHour=case_when(HappyHour=="True" ~ 1,HappyHour=="False" ~ 0, TRUE ~ NA_real_),
    OutdoorSeating=case_when(OutdoorSeating=="True"~ 1,OutdoorSeating=="False"~ 0, TRUE ~ NA_real_),
    HasTV =case_when(HasTV=="True" ~1,HasTV=="False"~ 0, TRUE ~ NA_real_),
    RestaurantsReservations =case_when(RestaurantsReservations== "True" ~ 1, RestaurantsReservations=="False" ~ 0,TRUE~ NA_real_),
    DogsAllowed=case_when(DogsAllowed=="True"~  1, DogsAllowed=="False"~ 0, TRUE ~ NA_real_),
    GoodForKids=case_when(GoodForKids=="True"~ 1, GoodForKids=="False"~ 0, TRUE ~ NA_real_),
    RestaurantsTableService=case_when(RestaurantsTableService=="True" ~ 1, RestaurantsTableService== "False"~ 0, TRUE ~ NA_real_),
    RestaurantsGoodForGroups=case_when(RestaurantsGoodForGroups== "True"~ 1, RestaurantsGoodForGroups=="False" ~ 0, TRUE ~ NA_real_),
    DriveThru=case_when(DriveThru=="True"~ 1,DriveThru=="False" ~ 0, TRUE ~ NA_real_),
    BusinessAcceptsBitcoin=case_when(BusinessAcceptsBitcoin=="True" ~ 1, BusinessAcceptsBitcoin=="False" ~ 0, TRUE ~ NA_real_),
    GoodForDancing=case_when(GoodForDancing=="True"~ 1,GoodForDancing=="False" ~ 0, TRUE ~ NA_real_),
    AcceptsInsurance=case_when(AcceptsInsurance=="True" ~ 1,AcceptsInsurance=="False" ~ 0, TRUE ~ NA_real_),
    BYOB=case_when(BYOB=="True"~ 1,BYOB=="False"~ 0, TRUE ~ NA_real_),
    Corkage =case_when(Corkage=="True" ~1,Corkage=="False" ~ 0, TRUE ~ NA_real_),
    Open24Hours=case_when(Open24Hours=="True" ~ 1,Open24Hours=="False" ~0,TRUE~NA_real_),
    RestaurantsCounterService=case_when(RestaurantsCounterService=="True" ~ 1, RestaurantsCounterService=="False"~ 0, TRUE ~ NA_real_))

# model2_data<- model2_data %>%
#  left_join(tip_data, by = "user_id") - causes many-to-many relationship error

# splitting into train adn test again
set.seed(1)
train1<-sample(1:nrow(model2_data),nrow(model2_data)-10000)
train_data2<-model2_data[train1,]
test_data2<-model2_data[-train1,]

#convert 'stars' to factor
train_data2$stars<-factor(train_data2$stars,levels=c(1,2,3,4,5),ordered=TRUE)
test_data2$stars<-factor(test_data2$stars,levels=c(1,2,3,4,5),ordered=TRUE)


#Attempting another logit model with new data 
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


# VALIDATION 

# logit_model11 
#predict most likely class
test_predictions11 <-predict(logit_model11,newdata=test_data,type="class")
#ensuring test_predictions11 has the same levels as test_data$stars
test_predictions11 <-factor(test_predictions11,levels= levels(test_data$stars))
#convert test_predictions11 to an ordered factor
test_predictions11 <-factor(test_predictions11,levels= levels(test_data$stars),ordered = TRUE)
accuracy11<-sum(test_predictions11==test_data$stars)/nrow(test_data)
print(accuracy11) #get NA, so exclude missing values:
# Calculate accuracy excluding NA values
valid_cases<-!is.na(test_predictions11) & !is.na(test_data$stars)
accuracy11<-sum(test_predictions11[valid_cases]==test_data$stars[valid_cases])/sum(valid_cases)
print(accuracy11)

#compare to baseline accuracy:

#most common class in the traindata
most_common_class <- names(sort(table(train_data$stars), decreasing = TRUE))[1]

#create baseline predictions
baseline_predictions <- rep(most_common_class, nrow(test_data))
#calculate baseline accuracy
baseline_accuracy <- sum(baseline_predictions == test_data$stars) / nrow(test_data)
print(baseline_accuracy)

#confusion matrix
confusionMatrix(test_predictions11, test_data$stars)


# EXPLORING OTHER FEATURES


#UNIVARIATE ANALYSIS : splitting variables into groups and calculating mean ratings

#binning continuous variables to group them

#binning useful, funny, cool with custom bins due to large amount of values near 0 
model2_data$useful_bins<- cut(model2_data$useful, breaks=c(-Inf,0,5,20,Inf),include.lowest=TRUE,labels=c("0","1-5","6-20","21+"))
mean_stars_by_useful<-aggregate(stars ~ useful_bins,data=model2_data,mean)
print(mean_stars_by_useful)

model2_data$funny_bins<-cut(model2_data$funny, breaks=c(-Inf,0,5,20,Inf), include.lowest=TRUE, labels=c("0","1-5","6-20","21+"))
mean_stars_by_funny<-aggregate(stars ~ funny_bins,data=model2_data,mean)
print(mean_stars_by_funny)

model2_data$cool_bins<-cut(model2_data$cool,breaks=c(-Inf,0,5,20,Inf),include.lowest=TRUE,labels=c("0","1-5","6-20","21+"))
mean_stars_by_cool <-aggregate(stars ~ cool_bins,data=model2_data,mean)
print(mean_stars_by_cool)
#cool has more distinct star ratings
#reviews marked as 'cool' are more likely to have higher star ratings
#reviews not marked as useful or funny tend to have higher star ratings


#correlations for usefuly, funny, cool
corr_review_data<-model2_data[,c("useful","cool","funny")]
corr_review_matrix<-cor(corr_review_data,use = "complete.obs")
print(corr_review_matrix)
#all three capture a similar aspect of the data and including all three may introduce multicollinearity, so just include 'cool'


#binning user data
#observing the distributions to decide on which points to cut the data
summary(model2_data$user_review_count)
summary(model2_data$user_useful)
summary(model2_data$user_funny)
summary(model2_data$user_cool)
summary(model2_data$fans)
summary(model2_data$average_stars)

model2_data$user_review_count_bins<- cut(model2_data$user_review_count, breaks=c(-Inf,0,10,50,100,Inf),include.lowest=TRUE,labels=c("0","1-10","11-50","51-100","101+"))
mean_stars_by_user_review_count<- aggregate(stars~user_review_count_bins, data=model2_data, mean)
print(mean_stars_by_user_review_count)
#A user's review count has an increasing trend with higher stars

model2_data$user_useful_bins<-cut(model2_data$user_useful, breaks=c(-Inf,50,500,1000,5000,Inf),include.lowest=TRUE,labels=c("0-50","51-500","501-1000","1001-5000","5000+"))
mean_stars_by_user_useful<-aggregate(stars~user_useful_bins, data=model2_data, mean)
print(mean_stars_by_user_useful)

model2_data$user_funny_bins<-cut(model2_data$user_funny, breaks=c(-Inf,5,25,50,100,Inf),include.lowest=TRUE,labels=c("0-5","6-25","26-50","51-100","100+"))
mean_stars_by_user_funny<-aggregate(stars~user_funny_bins, data=model2_data, mean)
print(mean_stars_by_user_funny)

model2_data$user_cool_bins<-cut(model2_data$user_cool, breaks=c(-Inf,5,25,50,100,Inf),include.lowest=TRUE,labels=c("0-5","6-25","26-50","51-100","100+"))
mean_stars_by_user_cool<-aggregate(stars~user_cool_bins, data=model2_data, mean)
print(mean_stars_by_user_cool)

#Users with more cool, funny, useful marks also have higher stars 
#however, this has a smoother relationship than previously described 'cool' and it may also cause multicolliniearity with 'cool'. 
#because user engagement metrics may be related to ratings for individual reviews.

model2_data$fans_bins<-cut(model2_data$fans, breaks=c(-Inf,1,5,10,50,Inf),include.lowest=TRUE,labels=c("0","1-5","6-10","11-50","50+"))
mean_stars_by_fans<-aggregate(stars~fans_bins, data=model2_data, mean)
print(mean_stars_by_fans)
#increasing trend with fans too

model2_data$average_stars_bins<-cut(model2_data$average_stars, breaks=c(1,2,3,4,5),include.lowest=TRUE,right=FALSE)
mean_stars_by_average_stars<-aggregate(stars~average_stars_bins, data=model2_data, mean)
print(mean_stars_by_average_stars)

#correlations for user_data
corr_user_data<-model2_data[, c("user_review_count","user_useful","user_funny","user_cool","fans")]
corr_user_matrix<-cor(corr_user_data,use="complete.obs")
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

model2_data$compliment_hot_bins<-cut(model2_data$compliment_hot,breaks=c(-Inf,0,1,5,20,Inf),include.lowest=TRUE,labels=c("0","1","2-5","6-20","20+"))
mean_stars_by_compliment_hot<-aggregate(stars~compliment_hot_bins,data=model2_data,mean)
print(mean_stars_by_compliment_hot)

model2_data$compliment_more_bins<-cut(model2_data$compliment_more,breaks=c(-Inf,0,1,10,50,Inf),include.lowest=TRUE,labels=c("0","1","2-10","11-50","50+"))
mean_stars_by_compliment_more<-aggregate(stars~compliment_more_bins,data=model2_data,mean)
print(mean_stars_by_compliment_more)

model2_data$compliment_profile_bins<-cut(model2_data$compliment_profile,breaks=c(-Inf,0,1,5,20,Inf),include.lowest=TRUE,labels=c("0","1","2-5","6-20","20+"))
mean_stars_by_compliment_profile<-aggregate(stars~compliment_profile_bins,data=model2_data,mean)
print(mean_stars_by_compliment_profile)

model2_data$compliment_cute_bins<-cut(model2_data$compliment_cute,breaks=c(-Inf,0,1,5,20,Inf),include.lowest=TRUE,labels=c("0","1","2-5","6-20","20+"))
mean_stars_by_compliment_cute<-aggregate(stars~compliment_cute_bins,data=model2_data,mean)
print(mean_stars_by_compliment_cute)

model2_data$compliment_list_bins<-cut(model2_data$compliment_list,breaks=c(-Inf,0,1,5,10,Inf),include.lowest=TRUE,labels=c("0","1","2-5","6-10","10+"))
mean_stars_by_compliment_list<-aggregate(stars~compliment_list_bins,data=model2_data,mean)
print(mean_stars_by_compliment_list)

model2_data$compliment_note_bins<-cut(model2_data$compliment_note,breaks=c(-Inf,0,5,20,50,Inf),include.lowest=TRUE,labels=c("0","1-5","6-20","21-50","50+"))
mean_stars_by_compliment_note<-aggregate(stars~compliment_note_bins,data=model2_data,mean)
print(mean_stars_by_compliment_note)

model2_data$compliment_plain_bins<-cut(model2_data$compliment_plain,breaks=c(-Inf,0,5,20,50,Inf),include.lowest=TRUE,labels=c("0","1-5","6-20","21-50","50+"))
mean_stars_by_compliment_plain<-aggregate(stars~compliment_plain_bins,data=model2_data,mean)
print(mean_stars_by_compliment_plain)

model2_data$compliment_cool_bins<-cut(model2_data$compliment_cool,breaks=c(-Inf,0,5,20,50,Inf),include.lowest=TRUE,labels=c("0","1-5","6-20","21-50","50+"))
mean_stars_by_compliment_cool<-aggregate(stars~compliment_cool_bins,data=model2_data,mean)
print(mean_stars_by_compliment_cool)

model2_data$compliment_funny_bins<-cut(model2_data$compliment_funny,breaks=c(-Inf,0,5,20,50,Inf),include.lowest=TRUE,labels=c("0","1-5","6-20","21-50","50+"))
mean_stars_by_compliment_funny<-aggregate(stars~compliment_funny_bins,data=model2_data,mean)
print(mean_stars_by_compliment_funny)

model2_data$compliment_writer_bins<-cut(model2_data$compliment_writer,breaks=c(-Inf,0,5,20,50,Inf),include.lowest=TRUE,labels=c("0","1-5","6-20","21-50","50+"))
mean_stars_by_compliment_writer<-aggregate(stars~compliment_writer_bins,data=model2_data,mean)
print(mean_stars_by_compliment_writer)

model2_data$compliment_photos_bins<-cut(model2_data$compliment_photos,breaks=c(-Inf,0,5,20,50,Inf),include.lowest=TRUE,labels=c("0","1-5","6-20","21-50","50+"))
mean_stars_by_compliment_photos<-aggregate(stars~compliment_photos_bins,data=model2_data,mean)
print(mean_stars_by_compliment_photos)

#Average star rating increases as the compliment count increases for all types of compliments
#The trends vary by how strong/distinct they are
#I pick the ones with the strongest trends and calculate correlations so I can use multiple wihout causing multicolliniearity

#Correlations within compliment data (checking if I can use multiple without multicollinearity)
# Subset the relevant columns from your data
compliment_data<-model2_data[,c("compliment_hot","compliment_more","compliment_plain","compliment_cool","compliment_writer","compliment_photos")]
corr_compliment_matrix<-cor(compliment_data,use ="complete.obs")
print(corr_compliment_matrix)
#All of them have moderate to strong correlation, so I pick one variable with the strongest trend (i.e. highest range of mean ratings of 0.2918) is compliment_photos. 

#binning business data
model2_data$business_stars_bins<-cut(model2_data$business_stars,breaks=c(1,2,3,4,5),include.lowest=TRUE, right=FALSE)
mean_stars_by_business_stars<-aggregate(stars ~ business_stars_bins,data=model2_data,mean)
print(mean_stars_by_business_stars)
#business_data shows a direct close trend, confirming its predictive power significance in logit_model11

model2_data$review_count_bins<-cut(model2_data$review_count,breaks=c(-Inf,50,150,400,1000,Inf),include.lowest=TRUE, labels=c("0-50","51-150","151-400","401-1000","1001+"))

mean_stars_by_review_count<-aggregate(stars ~ review_count_bins,data=model2_data, mean)
print(mean_stars_by_review_count)
mean_stars_by_is_open<-aggregate(stars~is_open,data=model2_data,mean)
print(mean_stars_by_is_open)
mean_stars_by_appointment<-aggregate(stars~ByAppointmentOnly,data=model2_data,mean)
print(mean_stars_by_appointment)
mean_stars_by_credit_cards<-aggregate(stars~BusinessAcceptsCreditCards,data=model2_data,mean)
print(mean_stars_by_credit_cards)
mean_stars_by_bikeparking<-aggregate(stars~BikeParking,data=model2_data,mean)
print(mean_stars_by_bikeparking)
mean_stars_by_coatcheck<-aggregate(stars~CoatCheck,data=model2_data,mean)
print(mean_stars_by_coatcheck)
mean_stars_by_takeout<-aggregate(stars~RestaurantsTakeOut,data=model2_data,mean)
print(mean_stars_by_takeout)
mean_stars_by_delivery<-aggregate(stars~RestaurantsDelivery,data=model2_data,mean)
print(mean_stars_by_delivery)
mean_stars_by_caters<-aggregate(stars~Caters,data=model2_data,mean)
print(mean_stars_by_caters)
mean_stars_by_wheelchair<-aggregate(stars~WheelchairAccessible,data=model2_data,mean)
print(mean_stars_by_wheelchair)
mean_stars_by_happyhour<-aggregate(stars~HappyHour,data=model2_data,mean)
print(mean_stars_by_happyhour)
mean_stars_by_outdoor<-aggregate(stars~OutdoorSeating,data=model2_data,mean)
print(mean_stars_by_outdoor)
mean_stars_by_hastv<-aggregate(stars~HasTV,data=model2_data,mean)
print(mean_stars_by_hastv)
mean_stars_by_reservations<-aggregate(stars~RestaurantsReservations,data=model2_data,mean)
print(mean_stars_by_reservations)
mean_stars_by_dogsallowed<-aggregate(stars~DogsAllowed,data=model2_data,mean)
print(mean_stars_by_dogsallowed)
mean_stars_by_goodforkids<-aggregate(stars~GoodForKids,data=model2_data,mean)
print(mean_stars_by_goodforkids)
mean_stars_by_tableservice<-aggregate(stars~RestaurantsTableService,data=model2_data,mean)
print(mean_stars_by_tableservice)
mean_stars_by_goodforgroups<-aggregate(stars~RestaurantsGoodForGroups,data=model2_data,mean)
print(mean_stars_by_goodforgroups)
mean_stars_by_drivethru<-aggregate(stars~DriveThru,data=model2_data,mean)
print(mean_stars_by_drivethru)
mean_stars_by_bitcoin<-aggregate(stars~BusinessAcceptsBitcoin,data=model2_data,mean)
print(mean_stars_by_bitcoin)
mean_stars_by_dancing<-aggregate(stars~GoodForDancing,data=model2_data,mean)
print(mean_stars_by_dancing)
mean_stars_by_insurance<-aggregate(stars~AcceptsInsurance,data=model2_data,mean)
print(mean_stars_by_insurance)
mean_stars_by_byob<-aggregate(stars~BYOB,data=model2_data,mean)
print(mean_stars_by_byob )
mean_stars_by_corkage<-aggregate(stars~Corkage,data=model2_data,mean)
print(mean_stars_by_corkage)
mean_stars_by_open24hours<-aggregate(stars~Open24Hours,data=model2_data,mean)
print(mean_stars_by_open24hours)
mean_stars_by_counterservice<-aggregate(stars~RestaurantsCounterService,data=model2_data,mean)
print(mean_stars_by_counterservice)
mean_stars_by_price_range<-aggregate(stars~RestaurantsPriceRange2,data=model2_data,mean)
print(mean_stars_by_price_range)

#correlations for attributes
binary<-model2_data[, c("Open24Hours","DriveThru","AcceptsInsurance", 
                          "GoodForDancing","HasTV","HappyHour", "BikeParking", 
                          "WheelchairAccessible", "OutdoorSeating","RestaurantsCounterService", 
                          "RestaurantsPriceRange2","BusinessAcceptsCreditCards", 
                          "BusinessAcceptsBitcoin","RestaurantsTakeOut", "BYOB")]
corr_binary <-cor(binary,use="pairwise.complete.obs") 
print(corr_binary)
# I select attributes that have a high change in mean star ratings, and also variables that are least correlated with each other and other variables


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
#issues with adding variables and I have to iteratively select and deselect some
logit_model21 <- polr(stars ~ average_stars + business_stars + Open24Hours
                      +cool + user_review_count + fans + compliment_photos +
                        review_count, data=train_data2, method='logistic')
#logit_model22 <- polr(stars ~ average_stars + business_stars + Open24Hours
#                      +cool + user_review_count + fans + compliment_photos +
#                        review_count+Open24Hours+HappyHour+Caters+
#                        HasTV + BikeParking, data=train_data2, method='logistic')

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
test_predictions22 <- predict(logit_model22, newdata=test_data2,type = "class")
#ensuring test_predictions22 has the same levels as test_data2$stars
test_predictions22 <- factor(test_predictions22,levels=levels(test_data2$stars))
# Convert test_predictions22 to an ordered factor
test_predictions22 <- factor(test_predictions22,levels=levels(test_data2$stars), ordered = TRUE)
accuracy22 <- sum(test_predictions22 == test_data2$stars)/nrow(test_data2)
print(accuracy22) 

#confusion matrix
confusionMatrix(test_predictions22,test_data2$stars)

#for logit_model21
#predict most likely class
test_predictions21 <-predict(logit_model21,newdata = test_data2, type = "class")
#ensuring test_predictions22 has the same levels as test_data2$stars
test_predictions21 <-factor(test_predictions21, levels = levels(test_data2$stars))
# Convert test_predictions22 to an ordered factor
test_predictions21 <-factor(test_predictions21,levels=levels(test_data2$stars),ordered = TRUE)
accuracy21 <- sum(test_predictions21==test_data2$stars)/nrow(test_data2)
print(accuracy21) #get NA, so exclude missing values:
# Calculate accuracy excluding NA values
valid_cases21 <- !is.na(test_predictions21) & !is.na(test_data2$stars)
accuracy21 <- sum(test_predictions21[valid_cases21]==test_data2$stars[valid_cases21])/sum(valid_cases21)
print(accuracy21)
#compare to baseline
#most common class in the traindata2
most_common_class21 <- names(sort(table(train_data2$stars), decreasing = TRUE))[1]
baseline_predictions21 <- rep(most_common_class21,nrow(test_data2))
baseline_accuracy21 <- sum(baseline_predictions21==test_data2$stars)/nrow(test_data2)
print(baseline_accuracy21)

#confusion matrix
confusionMatrix(test_predictions21, test_data2$stars)

#including more variables in logit causes more issues and less improvements in predictions so I explore other models 


#EXPLORING OTHER MODELS 


#attempting multiple variations of decision tree
# errors caused when including too much data with missing values, but a few variables is ok 
#however, accuracy isn't improving so I move on to random forests below 

#modelling

#New model with more variables, test accuracy  0.5108
decisiontree1<-tree(stars ~ average_stars+business_stars+cool+user_review_count+
                      +fans+compliment_photos+
                      +GoodForDancing+HasTV+BikeParking, data=train_data2)

# #2nd model only using relevant (based on range of mean star ratings across groups) variables with less than 50% missing data  
# #test accuracy of 0.4653
# #decisiontree1<-tree(stars~ business_stars+cool+BusinessAcceptsCreditCards+BikeParking+
#                       RestaurantsPriceRange2+HasTV, data=train_data2)
# 
# #3rd model only using variables (not neccessarily relevant) with less than 50% missing data
# #accuracy 0.4653
# decisiontree1<-tree(stars~useful+funny+cool+business_stars+review_count+is_open+BusinessAcceptsCreditCards+BikeParking+
#                       RestaurantsPriceRange2+HasTV, data=train_data2)
#4th accuracy of 0.4746 decisiontree1<-tree(stars ~ average_stars + business_stars + Open24Hours
 #                   +cool + user_review_count + fans + compliment_photos +
  #                    review_count+Open24Hours+HappyHour+Caters+
   #                   HasTV + BikeParking, data=train_data2)
decisiontree1 <-tree(stars~average_stars + business_stars + DriveThru + GoodForDancing + HasTV+BikeParking, data=train_data2)
plot(decisiontree1)
text(decisiontree1,pretty=1)
#validation - comparing training and test:
predictions_train <-predict(decisiontree1, train_data2, type='class')
predictions_test <-predict(decisiontree1, test_data2, type='class')
predictions_train <-factor(predictions_train, levels = levels(train_data2$stars), ordered = TRUE)
predictions_test <-factor(predictions_test, levels = levels(test_data2$stars), ordered = TRUE)

accuracy_train<-sum(predictions_train==train_data2$stars)/nrow(train_data2)
accuracy_test<-sum(predictions_test==test_data2$stars)/nrow(test_data2)
print(accuracy_train)
print(accuracy_test)
#validation
test_predictions3 <-predict(decisiontree1,newdata=test_data2,type='class')
summary(test_predictions3)
test_predictions3 <-factor(test_predictions3, levels = levels(test_data2$stars), ordered = TRUE)
accuracy3<-sum(test_predictions3==test_data2$stars)/nrow(test_data2)
print(accuracy3)

#attempting random forest - doesn't deal with NA values so I test a model with variables that have no missing values
randomforest1<-randomForest(stars ~ useful +cool + funny+ latitude+longitude+review_count+business_stars+is_open,
                            data=train_data2,ntree=50,mtry=2)
#seeing importance of each feature
randomForest::importance(randomforest1)
test_predictions4<-predict(randomforest1,newdata=test_data2)
test_predictions4 <-factor(test_predictions4, levels = levels(test_data2$stars),ordered= TRUE)
accuracy4<-sum(test_predictions4==test_data2$stars)/nrow(test_data2)
print(accuracy4)
print(test_predictions4)
confusionMatrix <-table(Predicted=test_predictions4, Actual = test_data2$stars)
print(confusionMatrix)

#visualising pattern of missing data
gg_miss_var(train_data2)
vis_miss(train_data2)#data too large so downsampling
train_data2_sample<-train_data2 %>% 
  slice_sample(n=10000)
vis_miss(train_data2_sample)
#proportion of missing data relative to observed
colSums(is.na(train_data2))/nrow(train_data2)

#attempted to use missForest but below code causes a memory issue on my device
# imputed_data <- missForest(train_data2)

#normal forest package doesn't deal with missing data, so i use the 'xgboost' package

#Using the whole train_data  
train_matrix<-xgb.DMatrix(data=as.matrix(train_data2[,-which(names(train_data2)=='stars')]), label=as.numeric(train_data2$stars)-1)
test_matrix<-xgb.DMatrix(data=as.matrix(test_data2[,-which(names(test_data2)=='stars')]), label=as.numeric(test_data2$stars)-1)

#looking at the first 15 important features only to see if accuracy improves - it doesn't 
xgbfeatures1<-c('business_stars','average_stars','cool','useful','funny','RestaurantsGoodForGroups','user_review_count','RestaurantsPriceRange2','GoodForKids','review_count','latitude','longitude','ByAppointmentOnly','OutdoorSeating','is_open')
  
#xgb1 - first15 
train_matrix<-xgb.DMatrix(data =as.matrix(train_data2[xgbfeatures1]),label=as.numeric(train_data2$stars)-1)
test_matrix<-xgb.DMatrix(data =as.matrix(test_data2[xgbfeatures1]),label=as.numeric(test_data2$stars)-1)

#initial model 
xgb1<-xgboost(data=train_matrix,max.depth=3,eta=1,nrounds=50,objective='multi:softprob',num_class=5)

#validating
train_predictions5<-predict(xgb1, train_matrix, type='response')
train_predictions5_class<-max.col(matrix(train_predictions5,nrow=nrow(train_matrix), byrow=TRUE))
train_accuracy5<-sum(train_predictions5_class==as.numeric(train_data2$stars))/length(train_predictions5_class)
test_predictions5 <-predict(xgb1, test_matrix)
test_predictions5_class<-max.col(matrix(test_predictions5,nrow=nrow(test_matrix),byrow=TRUE))
accuracy5<-sum(test_predictions5_class==as.numeric(test_data2$stars))/length(test_predictions5_class)
print(accuracy5)
print(train_accuracy5)
cm5<-confusionMatrix(factor(test_predictions5_class),factor(test_data2$stars))
print(cm5)
#check importance for each feature
importance_xgb1 <- xgb.importance(feature_names = colnames(train_data2[, colnames(train_data2) != "stars"]), model = xgb1)
print(importance_xgb1)
xgb.plot.importance(importance_xgb1)

# Best accuracy so far 0.5444 with xgb0 model which includes all train data variables
#No sign of overfitting
#Thus ttempt to use new potentially relevant variables to improve accuracy

#data cleaning on new variables

#elite
print(unique(user_data_small$elite)) #notice error - 2020 written as 20,20
elite_num<-function(elite){
  elite<-gsub("20,20","2020",elite)
  if (elite=="0"){
    return(0)
  } else{
    num<-unlist(strsplit(elite,","))
    return(length(num))
  }
}
user_data_small$elite<-sapply(user_data_small$elite,elite_num)

#friends
print(unique(user_data_small$friends))

friends_num<-function(friends){
  if (friends=="None"){
    return(0)
  } else{
    return(length(unlist(strsplit(friends,","))))
  }
}
user_data_small$friends<-sapply(user_data_small$friends, friends_num)

#state
print(unique(business_data$state))
business_data$state<-factor(business_data$state)

#wifi
print(unique(business_data$attribute$WiFi))
wifi_clean<-function(wifi){
  wifi<-gsub("u'","",wifi)
  wifi<-gsub("'","",wifi)
  wifi<-ifelse(wifi %in% c("None","no"),"none",wifi)
  return(wifi)
}
business_data$attributes$WiFi<-sapply(business_data$attributes$WiFi, wifi_clean)
business_data$attributes$WiFi<-factor(business_data$attributes$WiFi)

#alcohol
print(unique(business_data$attribute$Alcohol))
alcohol_clean<-function(alcohol){
  alcohol<-gsub("u'","",alcohol)
  alcohol<-gsub("'","",alcohol)
  alcohol<-ifelse(alcohol %in% c("None","none"),"none",alcohol)
}
business_data$attributes$Alcohol<-sapply(business_data$attributes$Alcohol, alcohol_clean)
business_data$attributes$Alcohol<-factor(business_data$attributes$Alcohol)

#noiseLevel
print(unique(business_data$attributes$NoiseLevel))
noiselevel_clean<-function(noise) {
  noise<-gsub("u'","",noise)
  noise<-gsub("'","",noise)
  noise<-gsub("None","none",noise)
  return(noise)
}
business_data$attributes$NoiseLevel<-sapply(business_data$attributes$NoiseLevel, noiselevel_clean)
business_data$attributes$NoiseLevel<-factor(business_data$attributes$NoiseLevel)

# Model 3 data - all the model 2 data plus the new variables that were cleaned above 

#renaming
user_data_small<-user_data_small %>%
  rename(user_useful=useful,user_funny=funny,user_cool=cool,user_review_count=review_count)
business_data<-business_data %>%
  rename(business_stars =stars)
#new model
model3_data <-review_data_small %>%
  left_join(user_data_small, by = "user_id") %>%
  left_join(business_data %>%
              mutate(ByAppointmentOnly= attributes$ByAppointmentOnly,
                BusinessAcceptsCreditCards= attributes$BusinessAcceptsCreditCards,
                BikeParking= attributes$BikeParking ,
                RestaurantsPriceRange2= attributes$RestaurantsPriceRange2,
                CoatCheck= attributes$CoatCheck,
                RestaurantsTakeOut= attributes$RestaurantsTakeOut,
                RestaurantsDelivery= attributes$RestaurantsDelivery,
                Caters = attributes$Caters,
                WheelchairAccessible= attributes$WheelchairAccessible,
                HappyHour=attributes$HappyHour,
                OutdoorSeating= attributes$OutdoorSeating,
                HasTV=attributes$HasTV,
                RestaurantsReservations = attributes$RestaurantsReservations,
                DogsAllowed =attributes$DogsAllowed,
                GoodForKids=attributes$GoodForKids,
                RestaurantsTableService= attributes$RestaurantsTableService,
                RestaurantsGoodForGroups= attributes$RestaurantsGoodForGroups,
                DriveThru=attributes$DriveThru,
                BusinessAcceptsBitcoin= attributes$BusinessAcceptsBitcoin,
                GoodForDancing= attributes$GoodForDancing,
                AcceptsInsurance= attributes$AcceptsInsurance,
                BYOB =attributes$BYOB,
                Corkage=attributes$Corkage,
                Open24Hours=attributes$Open24Hours,
                RestaurantsCounterService=attributes$RestaurantsCounterService,
                WiFi = attributes$WiFi, #new variables 
                Alcohol = attributes$Alcohol,
                Ambience = attributes$Ambience,
                NoiseLevel = attributes$NoiseLevel), by = "business_id")

#selecting only necessary columns (non-character) in model2_data due to Error in `vctrs::vec_locate_matches()`
model3_data<- model3_data %>%
  select(stars,useful,funny,cool, user_review_count,user_useful,user_funny,user_cool, elite, friends,fans, average_stars,
         compliment_hot, compliment_more,compliment_profile,compliment_cute,compliment_list,compliment_note,
         compliment_plain,compliment_cool,compliment_funny, compliment_writer,compliment_photos,   latitude,
         longitude,business_stars,review_count,is_open,ByAppointmentOnly,BusinessAcceptsCreditCards,BikeParking,
          RestaurantsPriceRange2, CoatCheck,RestaurantsTakeOut,RestaurantsDelivery, Caters, WheelchairAccessible,
         HappyHour, OutdoorSeating,HasTV,RestaurantsReservations,DogsAllowed,GoodForKids, RestaurantsTableService,
         RestaurantsGoodForGroups,DriveThru,BusinessAcceptsBitcoin,GoodForDancing,AcceptsInsurance, BYOB, 
         Corkage,Open24Hours,RestaurantsCounterService, WiFi, Alcohol, NoiseLevel)
colnames(model3_data)
#transforming True, False to 1,0, None to NA, chr in RestaurantsPriceRange2 to int
model3_data <- model3_data %>%
  mutate(ByAppointmentOnly=case_when(ByAppointmentOnly=="True" ~ 1, ByAppointmentOnly=="False" ~ 0,TRUE ~ NA_real_),
    BusinessAcceptsCreditCards=case_when(BusinessAcceptsCreditCards=='True' ~ 1, BusinessAcceptsCreditCards=='False' ~ 0, TRUE~ NA_real_),
    BikeParking=case_when(BikeParking=="True" ~ 1, BikeParking=='False'~ 0,TRUE~ NA_real_),
    RestaurantsPriceRange2=as.integer(as.character(RestaurantsPriceRange2)),
    CoatCheck=case_when(CoatCheck=="True" ~ 1, oatCheck=="False"~ 0, TRUE~ NA_real_),
    RestaurantsTakeOut=case_when(RestaurantsTakeOut=="True" ~1,RestaurantsTakeOut=="False"~ 0, TRUE ~ NA_real_),
    RestaurantsDelivery=case_when(RestaurantsDelivery=="True"~ 1,RestaurantsDelivery=="False"  ~ 0, TRUE~ NA_real_),
    Caters=case_when(Caters=="True" ~ 1,Caters=="False" ~ 0, TRUE ~ NA_real_),
    WheelchairAccessible=case_when(WheelchairAccessible=="True"~ 1,WheelchairAccessible=="False"~ 0, TRUE ~ NA_real_),
    HappyHour=case_when(HappyHour=="True" ~ 1,HappyHour=="False" ~ 0, TRUE ~ NA_real_),
    OutdoorSeating=case_when(OutdoorSeating=="True"~ 1,OutdoorSeating=="False"~ 0, TRUE ~ NA_real_),
    HasTV =case_when(HasTV=="True" ~1,HasTV=="False"~ 0, TRUE ~ NA_real_),
    RestaurantsReservations =case_when(RestaurantsReservations== "True" ~ 1, RestaurantsReservations=="False" ~ 0,TRUE~ NA_real_),
    DogsAllowed=case_when(DogsAllowed=="True"~  1, DogsAllowed=="False"~ 0, TRUE ~ NA_real_),
    GoodForKids=case_when(GoodForKids=="True"~ 1, GoodForKids=="False"~ 0, TRUE ~ NA_real_),
    RestaurantsTableService=case_when(RestaurantsTableService=="True" ~ 1, RestaurantsTableService== "False"~ 0, TRUE ~ NA_real_),
    RestaurantsGoodForGroups=case_when(RestaurantsGoodForGroups== "True"~ 1, RestaurantsGoodForGroups=="False" ~ 0, TRUE ~ NA_real_),
    DriveThru=case_when(DriveThru=="True"~ 1,DriveThru=="False" ~ 0, TRUE ~ NA_real_),
    BusinessAcceptsBitcoin=case_when(BusinessAcceptsBitcoin=="True" ~ 1, BusinessAcceptsBitcoin=="False" ~ 0, TRUE ~ NA_real_),
    GoodForDancing=case_when(GoodForDancing=="True"~ 1,GoodForDancing=="False" ~ 0, TRUE ~ NA_real_),
    AcceptsInsurance=case_when(AcceptsInsurance=="True" ~ 1,AcceptsInsurance=="False" ~ 0, TRUE ~ NA_real_),
    BYOB=case_when(BYOB=="True"~ 1,BYOB=="False"~ 0, TRUE ~ NA_real_),
    Corkage =case_when(Corkage=="True" ~1,Corkage=="False" ~ 0, TRUE ~ NA_real_),
    Open24Hours=case_when(Open24Hours=="True" ~ 1,Open24Hours=="False" ~0,TRUE~NA_real_),
    RestaurantsCounterService=case_when(RestaurantsCounterService=="True" ~ 1, RestaurantsCounterService=="False"~ 0, TRUE ~ NA_real_))
#creating dummies for factor variables state, WiFi, Alcohol, NoiseLevel
#naming NAs as unknown because of row mismatch error (arguments imply differing number of rows)
model3_data$state<-factor(model3_data$state, levels = c(levels(model3_data$state),'unknown'))
model3_data$WiFi<-factor(model3_data$WiFi, levels = c(levels(model3_data$WiFi),'unknown'))
model3_data$Alcohol<-factor(model3_data$Alcohol, levels = c(levels(model3_data$Alcohol),'unknown'))
model3_data$NoiseLevel<-factor(model3_data$NoiseLevel, levels = c(levels(model3_data$NoiseLevel),'unknown'))

model3_data$state[is.na(model3_data$state)] <- 'unknown'
model3_data$WiFi[is.na(model3_data$WiFi)] <- 'unknown'
model3_data$Alcohol[is.na(model3_data$Alcohol)] <- 'unknown'
model3_data$NoiseLevel[is.na(model3_data$NoiseLevel)] <- 'unknown'

statedum<-model.matrix(~ state - 1, data = model3_data)
colnames(statedum)<-paste("state",colnames(statedum),sep = "_")
wifidum<-model.matrix(~ WiFi - 1, data = model3_data)
colnames(wifidum)<-paste("WiFi",colnames(wifidum),sep = "_")
alcoholdum<-model.matrix(~ Alcohol - 1, data = model3_data)
colnames(alcoholdum)<-paste("Alcohol",colnames(alcoholdum),sep = "_")
noiseleveldum<-model.matrix(~ NoiseLevel - 1, data = model3_data)
colnames(noiseleveldum)<-paste("NoiseLevel",colnames(noiseleveldum),sep = "_")

#Removing original factors and inserting dummy variables
model3_data<-model3_data[, !(names(model3_data) %in% c("state","WiFi","Alcohol","NoiseLevel"))]
model3_data<-cbind(model3_data, statedum,wifidum,alcoholdum,noiseleveldum)

# splitting into train and test again
set.seed(1)
train3<-sample(1:nrow(model3_data),nrow(model3_data)-10000)
train_data3<-model3_data[train3,]
test_data3<-model3_data[-train3,]

train_data3$stars<-factor(train_data3$stars,levels=c(1,2,3,4,5),ordered=TRUE)
test_data3$stars<-factor(test_data3$stars,levels=c(1,2,3,4,5),ordered=TRUE)

#xgb0 - on the whole train_data  
train_matrix2<-xgb.DMatrix(data=as.matrix(train_data3[,-which(names(train_data3)=='stars')]), label=as.numeric(train_data3$stars)-1)
test_matrix2<-xgb.DMatrix(data=as.matrix(test_data3[,-which(names(test_data3)=='stars')]), label=as.numeric(test_data3$stars)-1)

#final model after tuning hyperparameters
xgb2<-xgboost(data=train_matrix2,max.depth=9,eta=0.1,nrounds=100,subsample=0.8,objective='multi:softprob',num_class=5)

#validating
train_predictions6<-predict(xgb2, train_matrix2, type='response')
train_predictions6_class<-max.col(matrix(train_predictions6,nrow=nrow(train_matrix2), byrow=TRUE))
train_accuracy6<-sum(train_predictions6_class==as.numeric(train_data3$stars))/length(train_predictions6_class)
test_predictions6 <-predict(xgb2, test_matrix2)
test_predictions6_class<-max.col(matrix(test_predictions6,nrow=nrow(test_matrix2),byrow=TRUE))
accuracy6<-sum(test_predictions6_class==as.numeric(test_data3$stars))/length(test_predictions6_class)
print(accuracy6)
print(train_accuracy6)
cm6<-confusionMatrix(factor(test_predictions6_class),factor(test_data3$stars))
print(cm6)
#check importance for each feature
importance_xgb2 <- xgb.importance(feature_names = colnames(train_data3[, colnames(train_data3) != "stars"]), model = xgb2)
print(importance_xgb2)
xgb.plot.importance(importance_xgb2)
