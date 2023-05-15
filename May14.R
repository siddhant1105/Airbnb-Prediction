install.packages("predictrace")
install.packages("tidytext")
install.packages("wordcloud")
install.packages("stopwords")
install.packages("xgboost")
install.packages("geosphere")
install.packages("ggmap")


#load libraries
library(tidyverse)
library(dplyr)
library(predictrace)
library(caret)
library(ROCR)
library(randomForest)
library(pROC)
library(tidytext)
library(wordcloud)
library(stopwords)
library(xgboost)
library(dbscan)
library(geosphere)
library(ggmap)

#load data files
train_x <- read_csv("airbnb_train_x_2023.csv")
train_y <- read_csv("airbnb_train_y_2023.csv")
test_x <- read_csv("airbnb_test_x_2023.csv")
airport_info <- read_csv("airport_domestic.csv")
wage <- read_csv("wage_domestic.csv")

#creating labels
train_x$label <- 'train'
test_x$label <- 'test'

#stacking train_x and test_x
train <- rbind(train_x, test_x)
summary(train)

# FUNCTIONS 

# For checking amenities 
check_internet <- function(x) {
  if ("Internet" %in% unlist(strsplit(x, ","))) {
    return("YES")
  } else {
    return("NO")
  }
}

check_pets <- function(x) {
  if (grepl("pets", x, ignore.case = TRUE)) {
    return("YES")
  } else {
    return("NO")
  }
}

check_parking <- function(x) {
  if (grepl("parking", x, ignore.case = TRUE)) {
    return("YES")
  } else {
    return("NO")
  }
}

check_kid <- function(x) {
  if (grepl("kid", x, ignore.case = TRUE)) {
    return("YES")
  } else {
    return("NO")
  }
}

check_tv <- function(x) {
  if (grepl("TV", x, ignore.case = TRUE)) {
    return("YES")
  } else {
    return("NO")
  }
}

check_gym <- function(x) {
  if (grepl("gym", x, ignore.case = TRUE)) {
    return("YES")
  } else {
    return("NO")
  }
}
check_laptop <- function(x) {
  if (grepl("laptop", x, ignore.case = TRUE)) {
    return("YES")
  } else {
    return("NO")
  }
}

check_amenity <- function(x, amenity) {
  if (amenity %in% unlist(strsplit(x, ","))) {
    return("YES")
  } else {
    return("NO")
  }
}

calc_distance <- function(lat1, long1, lat2, long2) {
  dist <- distGeo(c(long1, lat1), c(long2, lat2))/1000
  return(dist)
}

get_zipcode <- function(lat, long) {
  result <- tryCatch({
    location <- geocode(paste(lat, long), output = "more")
    return(location$postal_code)
  }, error = function(e) {
    return(NA)
  })
  return(result)
}


# CLEANING

#cleaning the newly created data set
train_perfect <- train %>%
  mutate(id = row_number(),
         cancellation_policy = ifelse(cancellation_policy %in% c("strict", "super_strict_30","super_strict_60"), 'strict', cancellation_policy),
         cleaning_fee = parse_number(cleaning_fee),
         price = parse_number(price),
         cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),
         price = ifelse(is.na(price), 0, price))%>%
  group_by(market)%>%
  mutate(count_market=n())%>%
  ungroup()%>%
  mutate(market= ifelse(is.na(market),"OTHER",market),
         market=as.factor(ifelse(count_market<500,"OTHER", market)))%>%
  mutate(neighbourhood = as.factor(neighbourhood)) %>%
  group_by(market,neighbourhood)%>%
  mutate(price = ifelse(price==0, median(price),price))%>%
  ungroup()%>%
  mutate(bedrooms = ifelse(is.na(bedrooms), mean(bedrooms, na.rm = TRUE), bedrooms),
         beds = ifelse(is.na(beds), mean(beds, na.rm = TRUE), beds),
         host_total_listings_count = ifelse(is.na(host_total_listings_count), mean(host_total_listings_count, na.rm = TRUE), host_total_listings_count))%>%
  mutate(price_per_person = price/accommodates,
         has_cleaning_fee = as.factor(ifelse(cleaning_fee != 0, "YES","NO")),
         bed_category = ifelse(bed_type=="Real Bed", "bed","other"),
         property_category = as.factor(case_when(property_type %in% c("Apartment","Serviced apartment","Loft")~"apartment",
                                                 property_type %in% c("Bed & Breakfast","Boutique hotel","Hostel")~"hotel",
                                                 property_type %in% c("Townhouse","Condominium")~"condo",
                                                 property_type %in% c("Bungalow","House")~"house",
                                                 TRUE ~ "other"))) %>%
  group_by(property_category) %>%
  mutate(median_by_property_category = median(price_per_person, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(ppp_ind = as.factor(ifelse(price_per_person > median_by_property_category, 1, 0)))%>%
  mutate(bed_category=as.factor(bed_category),
         cancellation_policy=as.factor(cancellation_policy),
         room_type = as.factor(room_type))%>%
  mutate(bathrooms = ifelse(is.na(bathrooms), median(bathrooms, na.rm=TRUE), bathrooms),
         host_is_superhost = ifelse(is.na(host_is_superhost),FALSE, host_is_superhost),
         extra_people = as.numeric(gsub("\\$","",extra_people)),
         charges_for_extra = as.factor(ifelse(extra_people > 0, "YES","NO")),
         host_acceptance_rate = as.numeric(gsub("\\%","",host_acceptance_rate)),
         host_acceptance = as.factor(case_when(host_acceptance_rate == 100 ~ "ALL",
                                               host_acceptance_rate < 100 ~ "SOME",
                                               is.na(host_acceptance_rate) ~ "MISSING")),
         host_response_rate = as.numeric(gsub("\\%","",host_response_rate)),
         host_response = as.factor(case_when(host_response_rate == 100 ~ "ALL",
                                             host_response_rate < 100 ~ "SOME",
                                             is.na(host_response_rate) ~ "MISSING")),
         has_min_nights = as.factor(ifelse(minimum_nights>1,"YES","NO")))%>%
  mutate(occupancy_type = as.factor(case_when(accommodates == 1 ~ "SINGLE",
                                              accommodates == 2 ~ "DOUBLE",
                                              accommodates > 2 & accommodates <= 5 ~ "SMALL",
                                              accommodates >= 6 & accommodates <= 10 ~ "MEDIUM",
                                              accommodates > 10 ~ "LARGE")),
         percent_availability_365 = round((availability_365/365)*100,2),
         booking_availability_365 = as.factor(case_when(percent_availability_365 <= 30 ~ "LOW",
                                                        percent_availability_365 >30 & percent_availability_365 <=60 ~ 'MEDIUM',
                                                        percent_availability_365 >60 ~ 'HIGH')),
         percent_availability_30 = round((availability_30/30)*100,2),
         booking_availability_30 = as.factor(case_when(percent_availability_30 <= 26.67 ~ "LOW",
                                                       percent_availability_30 >26.67 & percent_availability_30 <=35.64 ~ 'MEDIUM',
                                                       percent_availability_30 >35.64 ~ 'HIGH')),
         percent_availability_60 = round((availability_60/60)*100,2),
         booking_availability_60 = as.factor(case_when(percent_availability_60 <= 36.67 ~ "LOW",
                                                       percent_availability_60 >36.67 & percent_availability_60 <=41.29 ~ 'MEDIUM',
                                                       percent_availability_60 >41.29 ~ 'HIGH')),
         percent_availability_90 = round((availability_90/90)*100,2),
         booking_availability_90 = as.factor(case_when(percent_availability_90 <= 5.56 ~ "LOW",
                                                       percent_availability_90 >5.56 & percent_availability_90 <=45.57 ~ 'MEDIUM',
                                                       percent_availability_90 >45.57 ~ 'HIGH')),
         multiple_bathrooms = as.factor(ifelse(bathrooms>1,"YES","NO")),
         bed_type = as.factor(bed_type),
         real_bed = as.factor(ifelse(grepl("Real Bed", bed_type), 1, 0)),
         multiple_bedrooms = as.factor(ifelse(bedrooms>1,"YES","NO")))%>%
  mutate(host_name = iconv(host_name, to = "ASCII//TRANSLIT"),
         host_name = gsub("\\-", " ", host_name),
         host_name = gsub("\\'", " ", host_name),
         host_name = gsub('\\"', " ", host_name),
         host_name = gsub("\\(.*\\)", "", host_name),
         host_name = gsub("\\@", "", host_name),
         host_name = gsub("\\*", "", host_name),
         host_name = gsub("\\/", "", host_name),
         host_name = gsub("[0-9]", "", host_name),
         host_name = sapply(strsplit(as.character(host_name), "&"), function(x) x[1]),
         host_name = sapply(strsplit(as.character(host_name), ","), function(x) x[1]),
         host_name = sapply(strsplit(as.character(host_name), "And"), function(x) x[1]),
         host_name = ifelse(is.na(host_name),"UNKNOWN",host_name),
         host_name = ifelse(host_name== "", "UNKNOWN", host_name)) %>%
  mutate(cleaning_fee_ratio = round(cleaning_fee / price,2)*100,
         cleaning_fee_type = as.factor(case_when(cleaning_fee_ratio<15~"LOW",
                                                 cleaning_fee_ratio<39~"MEDIUM",
                                                 cleaning_fee_ratio<55~"HIGH",
                                                 cleaning_fee_ratio>=55~"VERY HIGH")),
         security_deposit = ifelse(is.na(security_deposit),0, security_deposit),
         security_deposit =parse_number(gsub("\\$","",security_deposit)),
         security_deposit = as.factor(ifelse(security_deposit==0,"NO","YES")),
         instant_bookable = as.factor(instant_bookable),
         is_location_exact = as.factor(is_location_exact))%>%
  mutate(amenities_count = sapply(amenities, function(x) length(unlist(strsplit(as.character(x), ',')))),
         host_verifications_count = sapply(host_verifications, function(x) length(unlist(strsplit(as.character(x), ',')))))

summary(train_perfect)
train_perfect <- train_perfect %>%
  mutate(amenities = gsub('\\"', "", amenities),
         amenities = gsub('\\{', "", amenities),
         amenities = gsub('\\}', "", amenities)) 

amenities_to_check <- c("Internet", "Air conditioning", "Kitchen", "Pool", "Heating")

# Loop through each amenity and create a new column for each amenity
for (amenity in amenities_to_check) {
  col_name <- paste0(amenity, "_availability") # Create column name
  train_perfect <- train_perfect %>%
    mutate(!!col_name := as.factor(sapply(amenities, check_amenity, amenity = amenity))) # Apply function and create column
}
train_perfect <- train_perfect %>%
  mutate(pets_allowed = as.factor(sapply(amenities, check_pets)),
         parking_allowed = as.factor(sapply(amenities, check_parking)),
         kid_allowed = as.factor(sapply(amenities, check_kid)),
         tv_available = as.factor(sapply(amenities, check_tv)),
         gym_available = as.factor(sapply(amenities,check_gym)),
         laptop_allowed = as.factor(sapply(amenities, check_laptop)))

summary(train_perfect)

train_perfect<-train_perfect %>%
  mutate(monthly_price =parse_number(gsub("\\$","",monthly_price)),
         weekly_price = parse_number(gsub("\\$","",weekly_price)),
         monthly_price = ifelse(is.na(monthly_price),price*30,monthly_price),
         weekly_price = ifelse(is.na(weekly_price),price*7,weekly_price),
         monthly_price_available = as.factor(ifelse((monthly_price-(price*30))==0,"YES","NO")),
         weekly_price_avaiable = as.factor(ifelse((weekly_price-(price*7))==0,"YES","NO")),
         weekly_price_discount = as.factor(ifelse(weekly_price< price*7,"YES","NO")),
         monthly_price_discount = as.factor(ifelse(monthly_price<price*30,"YES","NO")),
         nightly_rate = price/minimum_nights)

train_perfect<-train_perfect %>%
  mutate(host_since = as.Date(host_since),
         host_age = as.integer(difftime(Sys.Date(),host_since, units = "weeks")),
         host_age = ifelse(is.na(host_age),mean(host_age, na.rm = TRUE), host_age),
         host_age = host_age/52)
summary(train_perfect)

airbnb_gender<- predict_gender(train_perfect$host_name)

airbnb_gender <- airbnb_gender %>%
  mutate(likely_gender = as.factor(case_when(likely_gender == "female"~"FEMALE",
                                             likely_gender == "male"~"MALE",
                                             likely_gender == "female, male"~ "OTHER",
                                             is.na(likely_gender)~"OTHER")))
summary(airbnb_gender)
train_perfect$host_gender<-airbnb_gender$likely_gender

train_perfect <- train_perfect %>%
  mutate(host_response_time = if_else(is.na(host_response_time), "Unknown", host_response_time)) %>%
  mutate(host_response_time = as.factor(host_response_time))

# To identify most common words from neighborhood_overview
df_words <- train_perfect %>%
  unnest_tokens(word, neighborhood_overview)

stop_words <- tibble(word = stopwords("en"))

df_words <- df_words %>%
  anti_join(stop_words)

word_freq <- df_words %>%
  count(word, sort = TRUE)

# count the frequency of each word
word_count <- df_words %>%
  count(word, sort = TRUE)

# display the top 10 most common words
head(word_count, 10)

# Create a list of common words
common_words <- c("restaurants", "park" ,"bars","downtown")

# Create a function to check if common words are present
check_common_words <- function(x) {
  if (any(grepl(paste(common_words, collapse = "|"), x, ignore.case = TRUE))) {
    return("YES")
  } else {
    return("NO")
  }
}

# Apply function to create common_word_present column
train_perfect <- train_perfect %>%
  mutate(common_word_neighborhood = as.factor(sapply(neighborhood_overview, check_common_words)))


# To identify most common words from column name
df_words_name <- train_perfect %>%
  unnest_tokens(word2, name)

stop_words <- tibble(word2 = stopwords("en"))

df_words_name <- df_words_name %>%
  anti_join(stop_words)

word_freq <- df_words_name %>%
  count(word2, sort = TRUE)


# count the frequency of each word
word_count2 <- df_words_name %>%
  count(word2, sort = TRUE)

# display the top 10 most common words
head(word_count2, 10)

# Create a list of common words
common_words2 <- c("room", "private" ,"bedroom", "cozy", "apartment", "home")

# Create a function to check if common words are present
check_common_words2 <- function(x) {
  if (any(grepl(paste(common_words2, collapse = "|"), x, ignore.case = TRUE))) {
    return("YES")
  } else {
    return("NO")
  }
}

# Apply function to create common_word_present column
train_perfect <- train_perfect %>%
  mutate(common_word_name = as.factor(sapply(name, check_common_words2)))

#Creating interaction terms
train_perfect <- train_perfect %>%
  mutate(accommodates_bedrooms = accommodates * bedrooms,
         accomodates_beds = accommodates * beds,
         accomodates_yearly = accommodates * percent_availability_365,
         bedrooms_bathrooms = bedrooms * bathrooms, 
         beds_bathrooms = beds * bathrooms,
         beds_yearly = beds * percent_availability_365
         
  )










#Access column sentiment
df_sentiment <- train_perfect %>%
  select(id, access) %>%
  unnest_tokens(word, access) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(sentiment_score = sum(value)) %>%
  mutate(sentiment = ifelse(sentiment_score > 0, "Positive",
                            ifelse(sentiment_score < 0, "Negative", "Neutral")))

sentiment_ids <- unique(df_sentiment$id)

missing_sentiment_ids <- setdiff(unique(train_perfect$id), sentiment_ids)

missing_sentiments <- data.frame(id = missing_sentiment_ids, sentiment_score = 0, sentiment = "Not Available", stringsAsFactors = FALSE)

sentiment_analysis <- rbind(df_sentiment, missing_sentiments)

sentiment_scores <- sentiment_analysis[, c("sentiment", "sentiment_score", "id")]

sentiment_scores$id <- as.numeric(sentiment_scores$id)

sentiment_scores <- sentiment_scores %>%
  arrange(id)

train_perfect <- train_perfect %>%
  left_join(sentiment_scores, by = "id")
####################################################################################
# Sentiment analysis - description
library(tidytext)

# Define a function to perform sentiment analysis on a column
sentiment_analysis <- function(df, column_name) {
  df_sentiment <- df %>%
    select(id, {{column_name}}) %>%
    unnest_tokens(word, {{column_name}}) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(id) %>%
    summarize(sentiment_score = sum(value)) %>%
    mutate(sentiment = ifelse(sentiment_score > 0, "Positive",
                              ifelse(sentiment_score < 0, "Negative", "Neutral")))
  
  sentiment_ids <- unique(df_sentiment$id)
  
  missing_sentiment_ids <- setdiff(unique(df$id), sentiment_ids)
  
  missing_sentiments <- data.frame(id = missing_sentiment_ids, sentiment_score = 0, sentiment = "Not Available", stringsAsFactors = FALSE)
  
  sentiment_analysis <- rbind(df_sentiment, missing_sentiments)
  
  sentiment_scores <- sentiment_analysis[, c("sentiment", "sentiment_score", "id")]
  
  sentiment_scores$id <- as.numeric(sentiment_scores$id)
  
  sentiment_scores <- sentiment_scores %>%
    arrange(id)
  
  df <- df %>%
    left_join(sentiment_scores, by = "id")
  
  return(df)
}

# Apply the sentiment_analysis function to the columns of interest
train_perfect <- train_perfect %>%
  sentiment_analysis(description) %>%
  sentiment_analysis(host_about) %>%
  sentiment_analysis(house_rules) %>%
  sentiment_analysis(interaction) %>%
  sentiment_analysis(neighborhood_overview) %>%
  sentiment_analysis(summary) %>%
  sentiment_analysis(transit)




#READABILITY SCORE FOR DESCRIPTION
library(quanteda.textstats)
library(data.table)
# subset data to necessary columns
train_subset <- train_perfect[, c("id", "house_rules", "description", "neighborhood_overview", "notes", "space", "summary", "transit")]

# convert to data table
train_dt <- data.table(train_subset)

# calculate readability using the Flesch-Kincaid index for each column
readability_scores_house_rules <- train_dt[, textstat_readability(house_rules, measure = "Flesch.Kincaid")]
readability_scores_description <- train_dt[, textstat_readability(description, measure = "Flesch.Kincaid")]
readability_scores_neighborhood_overview <- train_dt[, textstat_readability(neighborhood_overview, measure = "Flesch.Kincaid")]
readability_scores_notes <- train_dt[, textstat_readability(notes, measure = "Flesch.Kincaid")]
readability_scores_space <- train_dt[, textstat_readability(space, measure = "Flesch.Kincaid")]
readability_scores_summary <- train_dt[, textstat_readability(summary, measure = "Flesch.Kincaid")]
readability_scores_transit <- train_dt[, textstat_readability(transit, measure = "Flesch.Kincaid")]

# add the readability scores as new columns to the original dataset
train_perfect$house_rules_fk_grade <- readability_scores_house_rules$Flesch.Kincaid
train_perfect$description_fk_grade <- readability_scores_description$Flesch.Kincaid
train_perfect$neighborhood_overview_fk_grade <- readability_scores_neighborhood_overview$Flesch.Kincaid
train_perfect$notes_fk_grade <- readability_scores_notes$Flesch.Kincaid
train_perfect$space_fk_grade <- readability_scores_space$Flesch.Kincaid
train_perfect$summary_fk_grade <- readability_scores_summary$Flesch.Kincaid
train_perfect$transit_fk_grade <- readability_scores_transit$Flesch.Kincaid


### Clustering and Interaction terms
cluster_results <- dbscan(cbind(train_perfect$longitude,train_perfect$latitude),eps=0.01, minPts = 5)
cluster_results
train_perfect <- train_perfect %>%
  mutate(cluster_results = cluster_results$cluster)
summary(train_perfect$cluster_results)


train_perfect <- train_perfect %>%
  mutate(host_interact = as.factor(ifelse(is.na(interaction),0,1)))
summary(train_perfect)

# Gender Ratio
train_perfect <- train_perfect %>%
  group_by(market,host_gender)%>%
  mutate(price_gender = mean(price)) %>%
  ungroup()


summary(train_perfect)

## AIRPORT INFORMATION
library(tidyverse)


# read in the FAA Airport Data CSV file
airport_info <- airport_info %>%
  rename(state = iso_region) %>%
  mutate(state = gsub("-.*", "", state))
airport_info <- airport_info %>%
  select(name, latitude_deg, longitude_deg, state)

airbnb_coords <- train_perfect %>%
  select(latitude, longitude,id)

airport_coords <-airport_info %>%
  select(latitude_deg, longitude_deg, name)

# create an empty vector to store the distances
distances <- vector(mode = "numeric", length = nrow(airbnb_coords))

for (i in 1:nrow(airbnb_coords)) {
  distances[i] <- min(distHaversine(p1 = airbnb_coords[i, c("longitude", "latitude")], 
                                    p2 = airport_coords[, c("longitude_deg", "latitude_deg")]))
}

airbnb_coords$distance_to_nearest_airport <- distances

train_perfect$distance_to_nearest_airport <- distances

train_perfect <- train_perfect %>%
  mutate(noise_level = as.factor(case_when(distance_to_nearest_airport >2384.935 ~ "LOW",
                                           distance_to_nearest_airport <= 2384.935 & distance_to_nearest_airport > 1000 ~ "MEDIUM",
                                           distance_to_nearest_airport <1000 ~ "HIGH")))
### WAGE DATA
summary(train_perfect$zipcode)
# group by state and calculate mean wage
wage_mean <- wage %>% 
  group_by(State) %>% 
  mutate(mean_wage = mean(TotalWages)) %>% 
  ungroup()

# find top 10% high-wage areas
wage_quantile <- quantile(wage_mean$mean_wage, probs = 0.5)
high_wage_areas <- wage_mean %>% 
  filter(mean_wage >= wage_quantile)

train_perfect$dist_to_high_wage <- NA

for (i in 1:nrow(train_perfect)) {
  distances <- distHaversine(p1 = train_perfect[i, c("longitude", "latitude")], 
                             p2 = high_wage_areas[, c("Long", "Lat")])
  train_perfect$dist_to_high_wage[i] <- min(distances)
}

train_perfect <- train_perfect %>%
  mutate(closeness_high_wage = as.factor(case_when(dist_to_high_wage <=2851 ~ "NEAR",
                                                   dist_to_high_wage > 2851 & dist_to_high_wage <=1356374 ~ "FAR",
                                                   dist_to_high_wage >1356374 ~ "VERY FAR")))

train_perfect <- train_perfect %>%
  mutate(transportation_options = as.factor(ifelse(distance_to_nearest_airport <= 1949.791, "Available", "Not available")))


summary(train_perfect$distance_to_nearest_airport)

#MODELS
airbnb_features <- train_perfect %>%
  select(accommodates, bedrooms, beds, cancellation_policy,has_cleaning_fee, host_total_listings_count, 
         price, ppp_ind, property_category, bed_type, bathrooms, charges_for_extra, host_acceptance, 
         host_response, has_min_nights, market,host_is_superhost, host_gender,occupancy_type, percent_availability_30, 
         percent_availability_60,percent_availability_90,percent_availability_365, booking_availability_365,
         multiple_bathrooms, cleaning_fee_type, security_deposit, room_type, instant_bookable, amenities_count,
         Internet_availability,Kitchen_availability, Pool_availability, Heating_availability, pets_allowed, parking_allowed, 
         kid_allowed,tv_available, gym_available, laptop_allowed, monthly_price, 
         weekly_price_avaiable, monthly_price_discount, nightly_rate, host_age,host_response_time,
         common_word_name, common_word_neighborhood, accommodates_bedrooms, 
         cluster_results, host_interact, sentiment_score.x,
         sentiment_score.y, sentiment_score.x.x, sentiment_score.y.y, sentiment_score.x.x.x, sentiment_score.y.y.y,
         sentiment_score.x.x.x.x, sentiment_score.y.y.y.y,price_gender, 
         house_rules_fk_grade, summary_fk_grade, distance_to_nearest_airport, noise_level,closeness_high_wage,
         label)









summary(airbnb_features)
dummy <- dummyVars( ~ . , data=airbnb_features, fullRank = TRUE)
airbnb_X <- data.frame(predict(dummy, newdata = airbnb_features))


airbnb_new_train <- airbnb_X[airbnb_X$labeltrain == 1, ]
airbnb_new_test <- airbnb_X[airbnb_X$labeltrain == 0, ]

train_with_labels <- cbind(airbnb_new_train, train_y) %>%
  mutate(perfect_rating_score = as.factor(perfect_rating_score),
         high_booking_rate = as.factor(high_booking_rate)) 
summary(train_with_labels)

train_high_booking_rate <- train_with_labels %>%
  select(-c(perfect_rating_score), -c(labeltrain))
summary(train_high_booking_rate)

# Apply the learned dummy parameters to the original data to turn it into dummy variables
train_index <- sample(seq_len(nrow(train_high_booking_rate)), size = floor(0.7*nrow(train_high_booking_rate)))
train_data <- train_high_booking_rate[train_index, ]
valid_data <- train_high_booking_rate[-train_index, ]

logmodel <- glm(high_booking_rate~., data = train_data, family = "binomial")
summary(logmodel)


logit_preds <- predict(logmodel, newdata = valid_data, type = "response")

classifications_perfect <- ifelse(logit_preds > .5, "YES", "NO")
classifications_perfect <- ifelse(is.na(classifications_perfect), "NO", classifications_perfect)
summary(classifications_perfect)
actual_pred_lm= as.factor(valid_data$high_booking_rate)

class_valid_lg = factor(classifications_perfect, level=unique(actual_pred_lm))
logit_cm <- confusionMatrix(data = class_valid_lg, reference = actual_pred_lm)
logit_cm$table

accuracy <- logit_cm$overall['Accuracy']
print(paste("Accuracy:", round(accuracy, 3)*100))

auc_tr <- auc(roc(actual_pred_lm, logit_preds))
print(auc_tr)

#full training data set - Logistic model (no splitting)

logmodel_full <- glm(high_booking_rate~., data = train_high_booking_rate, family = "binomial")
summary(logmodel_full)

logit_preds_full <- predict(logmodel_full, newdata = train_high_booking_rate, type = "response")

classifications_perfect_full <- ifelse(logit_preds_full > .5, "YES", "NO")
classifications_perfect_full <- ifelse(is.na(classifications_perfect_full), "NO", classifications_perfect_full)
summary(classifications_perfect_full)
actual_pred_lm_full= as.factor(train_high_booking_rate$high_booking_rate)

class_valid_lg_full = factor(classifications_perfect_full, level=unique(actual_pred_lm_full))
logit_cm_full <- confusionMatrix(data = class_valid_lg_full, reference =actual_pred_lm_full)
logit_cm_full$table

accuracy_full <- logit_cm_full$overall['Accuracy']
print(paste("Accuracy:", round(accuracy_full, 3)*100))

probs_rate <- predict(logmodel, newdata = airbnb_new_test, type = "response")
classifications_perfect_test <- ifelse(probs_rate > .5, "YES", "NO")
classifications_perfect_test <- ifelse(is.na(classifications_perfect_test), "NO", classifications_perfect_test)
summary(classifications_perfect_test)
write.table(probs_rate, "high_booking_rate_group9.csv", row.names = FALSE)

auc_logfull <- auc(roc(actual_pred_lm_full, logit_preds_full))
print(auc_logfull)


# Random Forest - Model- Work in progress
rf_model <- randomForest(high_booking_rate ~ ., data = train_data, ntree = 500, mtry = 2, importance = TRUE)

predictions_rf <- predict(rf_model, newdata=valid_data, type = "prob")[,2]
predictions_rf
classifications_perfect_rf <- ifelse(predictions_rf > .5, "YES", "NO")
classifications_perfect_rf <- as.factor(ifelse(is.na(classifications_perfect_rf), "NO", classifications_perfect_rf))

summary(classifications_perfect_rf)


actual_pred_rf= as.factor(valid_data$high_booking_rate)
summary(actual_pred_rf)



length(actual_pred_rf)
length(predictions_rf)

rf_cm <- confusionMatrix(data = classifications_perfect_rf, reference =actual_pred_rf)
rf_cm$table

accuracy_rf <- rf_cm$overall['Accuracy']
print(paste("Accuracy:", round(accuracy_rf, 3)*100))

probs_rate <- predict(rf_model, newdata = airbnb_new_test, type = "response")
write.table(probs_rate, "high_booking_rate_group9.csv", row.names = FALSE)


auc_rf <- auc(roc(actual_pred_rf, predictions_rf))
print(auc_rf)


# XG BOOST
#coverting columns to numeric
#install.packages("xgboost")
library("xgboost")

X_train = train_high_booking_rate %>% select(-high_booking_rate)
Y_train = as.numeric(ifelse(train_high_booking_rate$high_booking_rate=="YES",1,0))
train_data
X_test = airbnb_new_test
Y_test = as.numeric(ifelse(train_y$high_booking_rate=="YES",1,0))

train_index <- sample(seq_len(nrow(train_high_booking_rate)), size = floor(0.7*nrow(train_high_booking_rate)))
X_train_train <-X_train[train_index,]
y_train_train <- Y_train[train_index]

X_train_val <- X_train[-train_index,]
y_train_val <- Y_train[-train_index]


#train_matrix <- xgb.DMatrix(data = as.matrix(X_train_train), label = as.matrix(Y_train))
#test_matrix <- xgb.DMatrix(data = as.matrix(X_train_val), label = as.matrix(y_train_val))


train_matrix <- as.matrix(X_train_train)
valid_matrix <- as.matrix(X_train_val)

train_index <- sample(seq_len(nrow(airbnb_new_train)), size = floor(0.7*nrow(airbnb_new_train)))
train_data <- train_high_booking_rate[train_index, ]
valid_data <- train_high_booking_rate[-train_index, ]

xgb_model <- xgboost(data = train_matrix, label = y_train_train, nrounds = 500, max_depth = 6, eta = 0.1, objective = "binary:logistic")


importance <- xgb.importance(feature_names = colnames(train_matrix), model = xgb_model)
importance


predictions <- predict(xgb_model, valid_matrix, type = "response")
predictions

classifications <- as.factor(ifelse(predictions > .5,1,0))
length(classifications)

actual_pred_xg <- as.factor(y_train_val)
levels(Y_train)
rf_xg <- confusionMatrix(data = classifications, reference =actual_pred_xg)
rf_xg$table

accuracy_xg <- rf_xg$overall['Accuracy']
print(paste("Accuracy:", round(accuracy_xg, 3)*100))
# Calculate AUC
auc_xg <- auc(roc(actual_pred_xg, predictions))
print(auc_xg)

dim(test_matrix)
dim(airbnb_new_test)
dim(train_matrix)
dim(valid_matrix)

summary(X_train_train)
summary(airbnb_new_test)
summary(test_xg_matrix)

test_xg_matrix = airbnb_new_test %>% select(-labeltrain)

probs_rate_xg <- predict(xgb_model, newdata = as.matrix(test_xg_matrix), type = "response")
probs_rate_xg
write.table(probs_rate_xg, "high_booking_rate_group9.csv", row.names = FALSE)






library(caret)
# Load the data
data <- train_high_booking_rate
X <- data %>% select(-high_booking_rate)
Y <- ifelse(data$high_booking_rate == "YES", 1, 0)

# Define the training control
ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE)

# Define the sample sizes for the learning curve
sample_sizes <- seq(0.1, 1, by = 0.1) * nrow(X)

# Define the tuning grid for xgboost
xgb_grid <- expand.grid(nrounds = 100,
                        max_depth = c(3, 6, 9),
                        eta = c(0.01, 0.1, 0.3),
                        gamma = 0,
                        colsample_bytree = 1,
                        min_child_weight = 1,
                        subsample = 1)

# Train the xgboost model with different sample sizes
xgb_model <- lapply(sample_sizes, function(n) {
  index <- sample(seq_len(nrow(X)), size = n)
  train(X[index, ], Y[index], 
        method = "xgbTree", 
        trControl = ctrl, 
        tuneGrid = xgb_grid, 
        metric = "ROC")
})

# Extract the ROC AUC for each model
auc <- sapply(xgb_model, function(m) max(m$results$ROC))

# Plot the learning curve
plot(sample_sizes, auc, type = "l", xlab = "Sample Size", ylab = "ROC AUC")








#RANGER MODEL
install.packages("ranger")
library(ranger)


X_train <- train_high_booking_rate %>% select(-high_booking_rate)
Y_train <- as.numeric(ifelse(train_high_booking_rate$high_booking_rate == "YES", 1, 0))

train_index <- sample(seq_len(nrow(train_high_booking_rate)), size = floor(0.7*nrow(train_high_booking_rate)))
X_train_train <- X_train[train_index, ]
y_train_train <- Y_train[train_index]

X_train_ranger <- train_high_booking_rate[train_index, ]

X_train_val <- X_train[-train_index, ]
y_train_val <- Y_train[-train_index]

# Train the ranger model
ranger_model <- ranger(high_booking_rate ~ ., data = X_train_ranger, num.trees = 500, mtry = 6, probability = TRUE)

# Predictions on validation set
validation_predictions <- predict(ranger_model, data = X_train_val, type = "response")$predictions[,2]
validation_actual <- as.numeric(ifelse(y_train_val == 1, 1, 0))

# Confusion matrix and accuracy
conf_matrix <- confusionMatrix(validation_predictions, validation_actual)
accuracy_ranger <- conf_matrix$overall['Accuracy']
cat("Accuracy:", round(accuracy_ranger, 3)*100, "%\n")

levels(validation_actual)
levels(validation_predictions)

length(validation_actual)
length(validation_predictions)
# Calculate AUC
library(pROC)
auc_ranger <- auc(roc(validation_actual, validation_predictions))
cat("AUC:", auc_ranger, "\n")

# Predictions on test set
probs_rate_ranger <- predict(ranger_model, data = airbnb_new_test, type = "response")$predictions





