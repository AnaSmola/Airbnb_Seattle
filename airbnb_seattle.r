library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(kableExtra)
library(purrr)
library(zoo)
library(Hmisc)
library(e1071)
library(leaflet)
library(shiny)
library(shinydashboard)
library(caret)
library(dummy)
library(corrplot)
library(ggcorrplot)
library(ggpubr)
library(mice)
library(moments)
library(e1071)
library(plotly)
library(reshape2)
library(jsonlite)
library(devtools)

# Install dplyr version 1.0.0
# Install and load devtools for version of dplyr 1.0.0



#devtools::install_version("dplyr", version = "1.0.0")
#install.packages("devtools")
#install.packages("jsonlite")
#install.packages("plotly")
#install.packages("reshape2")
#install.packages("e1071")
#install.packages("moments")
#install.packages("mice")
#install.packages("ggpubr")
#install.packages("shinydashboard")
#install.packages("dummies")
#install.packages("carrplot")
#install.packages("caret")
#install.packages("dummy")
#install.packages("e1071")
#install.packages("purrr")
#install.packages("zoo")
#install.packages("Hmisc")

#Seattle - dataset


#-------------------------------------------#
###########         1            ############
#-------------------------------------------#
#załadowanie danych ze źródła internetowego do ramki danych, z uwzględnieniem nagłówków, kodowania zbioru, separatorów itd.


file_path <- "C:\\Users\\anastazja\\Downloads\\listings_seattle\\listings.csv"


#file_path <- "C:\\Users\\dell\\Downloads\\listings.csv"

# Read the CSV file into a dataframe
airbnb <- read.csv(file = file_path, sep = ",",stringsAsFactors = F, na.strings = c(""))

airbnb
view(airbnb)

#-------------------------------------------#
###########          2           ############
#-------------------------------------------#

#Poznanie rozmiaru zbioru danych (liczby obserwacji i liczby zmiennych, które je opisują) i oszacowanie czasochłonności procesu analizy



# Print a vector containing the number of rows and columns
dim(airbnb)

# There are 6882 observations and 75 features

#the structure of the dataframe
str(airbnb)

# Extracting the number of rows and columns

nrow(airbnb)
ncol(airbnb)


#name of columns
names(airbnb)

# Memory usage
memory_usage <- function(airbnb) {
  column_sizes <- sapply(airbnb, function(col) sum(object.size(col)))
  total_memory_usage <- sum(column_sizes)
  return(total_memory_usage)
}


total_memory <- memory_usage(airbnb)
total_memory 
cat("Total memory usage:", total_memory, "bytes\n")


#-------------------------------------------#
###########           3          ############
#-------------------------------------------#
#Wyświetlenie próbki surowych danych w celu wyrobienia sobie wyobrażenia o nich – poznania struktury danych i wstępnej oceny przydatności poszczególnych zmiennych





#display head of df
head_of_airbnb <- head(airbnb)
head_of_airbnb

# Display the head of the data frame
head(airbnb) %>% kable() %>% kable_styling()




#Following columns can be deleted since they don’t have any useful information and hence wont’ be used in predictive models

#Choose columns to delete - ID, Listing.Url, Scrape.ID, Last.Scraped, Experiences.Offered, Thumbnail.Url, Medium.Url, Picture.Url, XL.Picture.Url, Host.URL, Host.ID, Host.Since,Host.Thumbnail.Url, Host.Picture.Url, Calendar.last.Scraped,First.Review, Last.Review, 	License, Jurisdiction.Names, Cancellation.Policy

names_to_delete <- c("listing_url", "source", "scrape_id", "last_scraped", "experiences_offered", 
                     "description", "picture_url", "neighborhood_overview", "picture_url", "host_id", 
                     "host_url", "host_since", "host_about", "host_picture_url", "host_thumbnail_url", "host_verifications", 
                     "host_has_profile_pic", "host_identity_verified", "host_is_superhost" ,
                     "minimum_minimum_nights","minimum_maximum_nights", "maximum_maximum_nights", "maximum_minimum_nights",
                      "last_review", "calendar_updated", "has_availability",
                     "calendar_last_scraped", "license", "instant_bookable")

# Modify the original data frame 
# Delete specified columns
airbnb <- airbnb[, !(names(airbnb) %in% names_to_delete)]
names(airbnb)

view(airbnb)



# Leaflet map of Settle
leaflet(airbnb %>% select(longitude, neighbourhood_group_cleansed, neighbourhood, latitude, price)) %>%
  setView(lng = -122.33, lat = 47.60, zoom = 12) %>%
  addTiles() %>% 
  addMarkers(
    clusterOptions = markerClusterOptions()
  )



#-------------------------------------------#
###########           4          ############
#-------------------------------------------#


#Weryfikacja i korekta typów zmiennych w danych jest ważna, aby zapewnić poprawność analizy i modelowania

#--------------------------------------#
#######        Data Types        #######
#--------------------------------------#

#again verification of datatypes

glimpse(airbnb)

str(airbnb)


#*names of numerical variables 
numerical_variables <- sapply(airbnb, is.numeric)
print(names(airbnb)[numerical_variables])




#*names of categorical variables
categorical_variables <- sapply(airbnb, is.factor)
print(names(airbnb)[categorical_variables])




#Change the type to factors



columns <- c('host_location', 'host_response_time', 'host_neighbourhood',
             'neighbourhood', 'neighbourhood_cleansed', 'neighbourhood_group_cleansed',
             'property_type', 'room_type')

airbnb[, columns] <- airbnb %>% select(all_of(columns)) %>% lapply(as.factor)


# check to make sure it worked
airbnb %>% select(all_of(columns)) %>% str()



#*check again the names of categorical variables
categorical_variables <- sapply(airbnb, is.factor)
print(names(airbnb)[categorical_variables])


#-------------------------------------------#
###########          5           ############
#-------------------------------------------#


#Zbudowanie podsumowania zmiennych numerycznych opisujących zbiór

#summary of dataset
summary(airbnb) 

#a compact summary of a data frame, displaying information about the data types 
glimpse(airbnb)

describe(airbnb)

# describe only numerical variables
numerical_data <- select_if(airbnb, is.numeric)

# prepare summary table
summary_table <- numerical_data %>%
  summarise(
    Variable = colnames(numerical_data),
    Min = apply(numerical_data, 2, min, na.rm = TRUE),
    Max = apply(numerical_data, 2, max, na.rm = TRUE),
    Mean = apply(numerical_data, 2, mean, na.rm = TRUE),
    Median = apply(numerical_data, 2, median, na.rm = TRUE),
    Q1 = apply(numerical_data, 2, quantile, prob = 0.25, na.rm = TRUE),
    Q3 = apply(numerical_data, 2, quantile, prob = 0.75, na.rm = TRUE),
    SD = apply(numerical_data, 2, sd, na.rm = TRUE),
    Missing = sapply(numerical_data, function(x) sum(is.na(x)))
  )


print(summary_table)
#-------------------------------------------#
###########           6          ############
#-------------------------------------------#


#Sprawdzenie, czy w zbiorze występują braki danych/duplikaty.

any(duplicated(airbnb))





#Missing Data 


colSums(is.na(airbnb))

# Missing data in %
missing_airbnb <- airbnb %>% summarise_all(~(sum(is.na(.))/n()))

missing_airbnb <- gather(missing_airbnb, key = "variables", value = "percent_missing")

# Sort by the percentage of missing values in descending order
missing_airbnb <- missing_airbnb %>% arrange(desc(percent_missing))
missing_airbnb


# Create a bar plot the NAs

ggplot(missing_airbnb, aes(x = reorder(variables, -percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Missing Values by Variable",
       x = "Variables",
       y = "Percentage Missing") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#columns with 50-100 % of missing should be deleted

cols_to_remove <- c("bathrooms", "bedrooms")

# Remove specified columns



airbnb <- airbnb %>% select(-one_of(cols_to_remove))

#Choose the columns to leave in the dataset



airbnb


# Statistic summary for factors
categorical_data <- select_if(airbnb, is.factor)

# prepare summary table
categorical_summary <- categorical_data %>%
  summarise(
    Variable = colnames(categorical_data),
    Categories = sapply(categorical_data, function(x) length(unique(x))),
    Most_Frequent_Category = sapply(categorical_data, function(x) {
      freq_table <- table(x)
      names(freq_table)[which.max(freq_table)]
    }),
    Most_Frequent_Category_Frequency = sapply(categorical_data, function(x) {
      freq_table <- table(x)
      max(freq_table) / length(x) * 100
    }),
    Unique_Values = sapply(categorical_data, function(x) length(unique(x))),
    Missing = sapply(categorical_data, function(x) sum(is.na(x)))
  )

print(categorical_summary)


#-------------------------------------------#
###########          8           ############
#-------------------------------------------#

#Przeprowadzenie czyszczenia danych 

str(airbnb)


#check for missing values


colSums(is.na(airbnb))
sum(colSums(is.na(airbnb)) > 0)
view(airbnb)
# Select chosen numeric variables

names(airbnb)

# Select chosen numeric variables
selected_numeric_variables <- airbnb[, c("accommodates", 
                                         "availability_30", "availability_60", "availability_90", "availability_365",
                                         "beds",  "minimum_nights", "maximum_nights",
                                         "number_of_reviews", "review_scores_rating", "review_scores_value", "reviews_per_month")]

# Print the first few rows to check if the selection worked
head(selected_numeric_variables)

# Pivot data from wide to long format
melted_data <- pivot_longer(selected_numeric_variables, cols = everything(), names_to = "variable", values_to = "value")

# Histograms for numeric variables
ggplot(melted_data, aes(x = value)) +
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black", alpha = 0.7) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Histograms of Selected Numeric Variables",
       x = "Variable Value", y = "Number of Observations") +
  theme_minimal()


# Calculate skewness and kurtosis for numeric variables
skew_kurt <- as.data.frame(sapply(selected_numeric_variables, function(x) c(skewness(x), kurtosis(x))))

# Display the results
print(skew_kurt)

# Boxplot for all selected numeric variables
boxplot(selected_numeric_variables,
        main = "Boxplot of Selected Numeric Variables",
        col = "skyblue",
        border = "black")




# Function to count missing values for each attribute in the dataset
missing_values_table <- function(airbnb) {
  mis_val <- colSums(is.na(airbnb))
  mis_val_percent <- 100 * colSums(is.na(airbnb)) / nrow(airbnb)
  mis_val_table <- data.frame(Missing_Values = mis_val, Total_Values_Percent = mis_val_percent)
  mis_val_table <- mis_val_table[mis_val_table$Total_Values_Percent != 0, ]
  mis_val_table <- mis_val_table[order(-mis_val_table$Total_Values_Percent), ]
  cat("Your selected dataframe has", ncol(airbnb), "columns.\n",
      "There are", nrow(mis_val_table), "columns that have missing values.\n")
  return(mis_val_table)
}

# Call the function with subset_airbnb
missing_values_table(airbnb)


## DATA PREPROCCESING ##

view(airbnb)
###    --  1.Price  ---- ####





#missing variables price checking
sum(is.na(airbnb$price))
class(airbnb$price)




# clean 'price' from $ symbol and change the type from char to float
airbnb <- airbnb %>%
  mutate(price = as.integer(gsub('[$,]', '', price)))
airbnb$price

#checking skewness before inputting NA

# Remove missing values
price_without_na <- na.omit(airbnb$price)

# Calculate skewness
skew <- skewness(price_without_na)
print(skew)
#-----------------------------------------------#
#####   Identify & Remove outliers - Price  #####
#-----------------------------------------------#





Q <- quantile(airbnb$price, probs=c(.25, .75), na.rm = T)
iqr <- IQR(airbnb$price, na.rm = T)
airbnb2 <- airbnb %>% filter(price > (Q[1] - 1.5*iqr) & 
                       price < (Q[2] + 1.5*iqr))  

# visualize the new dataset without outliers
options(repr.plot.width=10, repr.plot.height=6)
par(mfrow=c(2,1))
boxplot(airbnb$price, col = "grey40", horizontal = T, 
        main = "Price - Before Removing Outliers")
boxplot(airbnb2$price, col = "thistle2", horizontal = T, 
        main = "Price - After Removing Outliers")


##Missing values inputting

airbnb$price[is.na(airbnb$price)] <- median(airbnb$price, na.rm = TRUE)
sum(is.na(airbnb$price))


###    --  2.bathroom_text  ---- ####

#check the unique values 
unique(airbnb$bathrooms_text)

table(airbnb$bathrooms_text)

# Make a new column 'bathrooms' with numeric values from 'bathrooms_text'
airbnb$bathrooms <- as.numeric(gsub("[^0-9.]", "", airbnb$bathrooms_text))

# change 'half' to 0.5
airbnb$bathrooms[grepl("half", airbnb$bathrooms_text, ignore.case = TRUE)] <- 0.5



#check the unique values

unique(airbnb$bathrooms)
table(airbnb$bathrooms)


#missing variables bathrooms checking
sum(is.na(airbnb$bathrooms))
class(airbnb$bathrooms)

# After data cleansing, remove the original 'bathrooms_text' column
airbnb <- subset(airbnb, select = -bathrooms_text)


airbnb

###  --  3. host_response_rate/host_acceptance_rate  ---- ###
airbnb$host_response_rate


unique(airbnb$host_response_rate)
table(airbnb$host_response_rate)

# Replace NAs in "host_response_rate" with zeros
airbnb$host_response_rate <- ifelse(grepl("N/A", airbnb$host_response_rate), 0, airbnb$host_response_rate)
airbnb$host_response_rate <- gsub("%", "", airbnb$host_response_rate)

sum(is.na(airbnb$host_response_rate))

unique(airbnb$host_response_rate)

airbnb$host_acceptance_rate


unique(airbnb$host_acceptance_rate)
table(airbnb$host_acceptance_rate)

# Replace NAs in "host_response_rate" with zeros
airbnb$host_acceptance_rate <- ifelse(grepl("N/A", airbnb$host_acceptance_rate), 0, airbnb$host_acceptance_rate)
airbnb$host_acceptance_rate <- gsub("%", "", airbnb$host_acceptance_rate)

sum(is.na(airbnb$host_acceptance_rate))

unique(airbnb$host_acceptance_rate)


###    --  4."reviews_per_month", "review_scores_value", "review_scores_rating",              ---- ####-------
###           "review_scores_accuracy", "review_scores_cleanliness", "review_scores_checkin",  ---- ####-------
###            "review_scores_communication", "review_scores_location"                          ---- ####-------

# Choose columns for missing value inputting

columns <- c("reviews_per_month", "review_scores_value", "review_scores_rating", 
                      "review_scores_accuracy", "review_scores_cleanliness", "review_scores_checkin", 
                      "review_scores_communication", "review_scores_location")

# Calculate mean, median, minimum, maximum, and missing values for selected columns
summary_stats <- data.frame(
  Column = character(),
  Mean = numeric(),
  Median = numeric(),
  Min = numeric(),
  Max = numeric(),
  Missing = numeric(),
  stringsAsFactors = FALSE
)

for (col in columns) {
  summary_stats <- rbind(summary_stats, c(
    Column = col,
    Mean = mean(airbnb[[col]], na.rm = TRUE),
    Median = median(airbnb[[col]], na.rm = TRUE),
    Min = min(airbnb[[col]], na.rm = TRUE),
    Max = max(airbnb[[col]], na.rm = TRUE),
    Missing = sum(is.na(airbnb[[col]]))
  ))
}

# Display the results
print(summary_stats)


# Types of columns
column_types <- sapply(airbnb[, columns], class)
print(column_types)

# Impute missing values using MICE for selected columns
imp <- mice(airbnb[, columns], method = "pmm", m = 5)
imputed_data <- complete(imp)


# Update  the selected columns with imputed data
airbnb[, columns] <- imputed_data

# Check if missing values are correctly imputed
colSums(is.na(airbnb[, columns]))

###    --  5.Beds  ---- ####

sum(is.na(airbnb$beds))

unique(airbnb$beds)

airbnb <- airbnb %>%
  mutate(beds = coalesce(beds, 0))

sum(is.na(airbnb$beds))

typeof(airbnb$beds)

# change type double to int

airbnb$beds <- as.integer(airbnb$beds)




write.csv(airbnb, "C:\\Users\\anastazja\\Downloads\\clean_airbnb.csv", row.names = FALSE)


# read again the csv
airbnb <- read.csv("C:\\Users\\anastazja\\Downloads\\clean_airbnb.csv")


view(airbnb)

class(airbnb$price)


glimpse(airbnb)

#-------------------------------------------#
###########          7           ############
#-------------------------------------------#


#Wizualizacja rozkładu (wybranych) zmiennych (zarówno numerycznych, jak i kategorycznych) 


##____target is price______##


# Create a histogram and density plot for the "price" variable
ggplot(airbnb, aes(x = price)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "purple") + 
  geom_density(alpha = 0.2, fill = "purple") +
  theme_minimal() +
  ggtitle("Distribution of price",
          subtitle = "Right-skewed distribution") +
  theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_vline(xintercept = round(mean(airbnb$price), 2), size = 2, linetype = 3)





#logarithmic transformation can be used to gain better insight into data


ggplot(airbnb, aes(price)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "purple") + 
  geom_density(alpha = 0.2, fill = "purple") +
  
  ggtitle("Transformed distribution of price",
          subtitle = expression("With" ~'log'[10] ~ "transformation of x-axis")) +
  #theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_vline(xintercept = round(mean(airbnb$price), 2), size = 2, linetype = 3) +
  scale_x_log10() +
  annotate("text", x = 1800, y = 0.75,label = paste("Mean price = ", paste0(round(mean(airbnb$price), 2), "$")),
           color =  "#32CD32", size = 8)


##_________#Neighbourhood#_________##

##Histogram & Density with log10 transformation for neighbourhood areas
airbnb$neighbourhood_group_cleansed


airbnb_nh <- airbnb %>%
  group_by(neighbourhood_group_cleansed) %>%
  summarise(price = round(mean(price), 2))


ggplot(airbnb, aes(price)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "purple") + 
  geom_density(alpha = 0.2, fill = "purple") +

  ggtitle("Transformed distribution of price\n by neighbourhood groups",
          subtitle = expression("With" ~'log'[10] ~ "transformation of x-axis")) +
  geom_vline(data = airbnb_nh, aes(xintercept = price), size = 2, linetype = 3) +
  geom_text(data = airbnb_nh,y = 1.5, aes(x = price + 1400, label = paste("Mean  = ",price)), color = "darkgreen", size = 4) +
  facet_wrap(~neighbourhood_group_cleansed) +
  scale_x_log10() 




##____#Price and the type of room#______##
airbnb$property_type


#unique variables or room-type

unique(airbnb$property_type)

class(airbnb$property_type)



# Check the frequency of each property type
property_type_counts <- table(airbnb$property_type)

# Select the top 20 property types
top_20_property_types <- names(sort(property_type_counts, decreasing = TRUE)[1:20])
top_20_property_types

# Filter rows that contain these top 20 property types
airbnb_top_20_types <- airbnb[airbnb$property_type %in% top_20_property_types, ]

# Create a boxplot
ggplot(airbnb_top_20_types, aes(x = property_type, y = price, fill = property_type)) +
  geom_boxplot() +
  labs(title = "Price vs Type of Room",
       x = "Room Type",
       y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Filter rows that contain these top 20 property types
airbnb_top_20_types_2 <- airbnb2[airbnb2$property_type %in% top_20_property_types, ]


# Create a boxplot with transformed price 
ggplot(airbnb_top_20_types_2, aes(x = property_type, y = price, fill = property_type)) +
  geom_boxplot() +
  labs(title = "Transformed Price vs Type of Room",
       x = "Room Type",
       y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



airbnb$neighbourhood_cleansed

#Above Average Price Objects by Neighourhood Areas


airbnb$room_type
airbnb$neighbourhood_group_cleansed


airbnb %>%
  filter(price >= mean(price)) %>%
  group_by(neighbourhood_group_cleansed, room_type) %>%
  tally() %>%
  ggplot(aes(reorder(neighbourhood_group_cleansed, desc(n)), n, fill = room_type)) +
  theme_minimal() +  # Replace 'th' with the desired theme, e.g., theme_minimal()
  xlab(NULL) +
  ylab("Number of objects") +
  ggtitle("Number of above-average price objects",
          subtitle = "Most of them are entire homes or apartments") +
  geom_bar(stat = "identity")


# Select the top 10 neighborhoods


# filtered plot
airbnb %>%
  filter(price >= mean(price)) %>%
  group_by(neighbourhood_group_cleansed, room_type) %>%
  tally() %>%
  slice(1:10) %>%  # Move slice before group_by
  ggplot(aes(reorder(neighbourhood_group_cleansed, desc(n)), n, fill = room_type)) +
  theme_minimal() +
  xlab(NULL) +
  ylab("Number of objects") +
  ggtitle("Number of above-average price objects",
          subtitle = "Most of them are entire homes or apartments") +
  geom_bar(stat = "identity")

plot_data <- airbnb %>%
  filter(price >= mean(price)) %>%
  group_by(neighbourhood_group_cleansed) %>%
  summarise(mean_price = mean(price)) %>%
  arrange(mean_price, decreasing = FALSE) %>%
  top_n(10)

# Plot the mean price for the top 10 neighborhoods
ggplot(plot_data, aes(x = reorder(neighbourhood_group_cleansed, -mean_price), y = mean_price)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Top 10 Neighborhoods by Mean Price",
       x = "Neighborhood",
       y = "Mean Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


colnames(airbnb)


# Select the top five neighborhoods
top5_neighbourhoods <- airbnb %>%
  group_by(neighbourhood_group_cleansed) %>%
  summarise(mean_price = mean(price)) %>%
  arrange(desc(mean_price)) %>%
  head(5) %>%
  select(neighbourhood_group_cleansed)

# Filter data only for the top five neighborhoods
airbnb_top5 <- airbnb %>%
  filter(neighbourhood_group_cleansed %in% top5_neighbourhoods$neighbourhood_group_cleansed)

# Create the plot
ggplot(airbnb_top5, aes(y = price, x = minimum_nights, color = neighbourhood_group_cleansed)) +
  geom_point(alpha = 0.5) +
  geom_smooth(colour = "red", method = "lm") +
  xlim(0, max(airbnb_top5$minimum_nights)) +
  ggtitle("Price vs Minimum Nights for the Top 5 Neighborhoods")





# Group by 'host_name', calculate the mean of 'price', and sort in descending order
df_top_hosts <- airbnb %>%
  group_by(host_name) %>%
  summarise(avg_price = mean(price)) %>%
  arrange(desc(avg_price)) %>%
  head(20) %>%
  mutate(avg_price = round(avg_price, 2))

# Create a bar plot using ggplot2
ggplot(df_top_hosts, aes(x = reorder(host_name, -avg_price), y = avg_price, fill = avg_price, label = avg_price)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(position = position_dodge(0.9), vjust = -0.5) +
  labs(x = 'Name of Host', y = 'Average Earnings', title = 'Top Earners with Name and Average Price') +
  theme_minimal() +
  scale_fill_gradient("Average Price", low = "aquamarine", high = "dodgerblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






# Location of hosts with high prices for rooms
# Group by 'host_location', calculate the mean of 'price', and sort in descending order
df_top_loc <- airbnb %>%
  group_by(host_location) %>%
  summarise(avg_price = mean(price)) %>%
  arrange(desc(avg_price)) %>%
  head(20) %>%
  mutate(avg_price = round(avg_price, 2))

# Print the resulting dataframe
print(df_top_loc)

# Create a bar plot using ggplot2 with alternative colors
ggplot(df_top_loc, aes(x = reorder(host_location, -avg_price), y = avg_price, fill = avg_price, label = avg_price)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(position = position_dodge(0.9), vjust = -0.5) +
  labs(x = 'Location of Host', y = 'Average Earnings', title = 'Top Locations of Hosts and Average Prices') +
  theme_minimal() +
  scale_fill_gradient("Average Price", low = "gold", high = "darkorange") +  # Alternative colors
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


names(airbnb)


# Group by 'neighbourhood_cleansed', calculate the mean of 'number_of_reviews', and sort in descending order
df_maxbooking <- airbnb %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(avg_reviews = mean(number_of_reviews)) %>%
  arrange(desc(avg_reviews)) %>%
  head(20) %>%
  mutate(avg_reviews = round(avg_reviews, 2))


print(df_maxbooking)

# Create a bar plot using ggplot2 
ggplot(df_maxbooking, aes(x = reorder(neighbourhood_cleansed, -avg_reviews), y = avg_reviews, fill = avg_reviews, label = avg_reviews)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(position = position_dodge(0.9), vjust = -0.5) +
  labs(x = 'Neighbourhood Cleansed', y = 'Average Number of Reviews', title = 'Top Neighbourhoods by Average Number of Reviews') +
  theme_minimal() +
  scale_fill_gradient("Average Reviews", low = "lightblue", high = "darkblue") +  # Adjusted colors
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####Find cheapest and most expensive offers

# Sort the data frame by price in ascending order to get the cheapest offers
cheapest_offers <- airbnb[order(airbnb$price), ]

# Select the top 100 cheapest offers
top_100_cheapest <- head(cheapest_offers, 100)

# Create a leaflet map centered around Seattle
map_cheapest <- leaflet(top_100_cheapest) %>%
  setView(lng = -122.3321, lat = 47.6062, zoom = 12) %>%
  addTiles() %>%
  addMarkers(
    lng = ~longitude,
    lat = ~latitude,
    label = ~paste("Price: $", price),
    clusterOptions = markerClusterOptions()
  )

# Display the map
print(map_cheapest)

# Sort the data frame by price in descending order to get the most expensive offers
most_expensive_offers <- airbnb[order(-airbnb$price), ]

# Select the top 50 most expensive offers
top_50_most_expensive <- head(most_expensive_offers, 50)

# Create a leaflet map centered around Seattle
map_most_expensive <- leaflet(top_50_most_expensive) %>%
  setView(lng = -122.3321, lat = 47.6062, zoom = 12) %>%
  addTiles() %>%
  addMarkers(
    lng = ~longitude,
    lat = ~latitude,
    label = ~paste("Price: $", price),
    clusterOptions = markerClusterOptions()
  )

# Display the map
print(map_most_expensive)

airbnb$accommodates

#Price VS Accommodates
top_20_names <- airbnb %>%
  group_by(name) %>%
  summarise(mean_price = mean(price),
            mean_accommodates = mean(accommodates),
            mean_review_scores_value = mean(review_scores_value)) %>%
  top_n(20, wt = mean_price) %>%
  pull(name)

df_top_20 <- airbnb %>%
  filter(name %in% top_20_names)

# Display the structure of df_top_20
str(df_top_20)

# Create a scatter plot
ggplot(df_top_20, aes(x = name, y = price, color = accommodates, size = mean_price)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Prices vs Accommodates",
       x = "Location",
       y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(size = "none")





#Relationship between price and satisfaction
df_overall_satisfaction <- airbnb %>%
  group_by(price) %>%
  summarise(mean_review_scores_rating = mean(review_scores_rating)) %>%
  arrange(desc(mean_review_scores_rating)) %>%
  slice_head(n = 20) %>%
  mutate(mean_review_scores_rating = round(mean_review_scores_rating, 2))

df_overall_satisfaction


# Create a scatter plot


fig <- plot_ly(data = df_overall_satisfaction, x = ~price, y = ~mean_review_scores_rating, color = ~mean_review_scores_rating,
               type = 'scatter', mode = 'lines+markers')

# Update layout
fig <- fig %>% layout(title = "Relationship between price and satisfaction",
                      xaxis = list(title = 'Price'),
                      yaxis = list(title = 'Review Scores (Rating)'))

# Show the plot
fig


#Listings Sum by Accommodation Total


airbnb %>% group_by(accommodates) %>% 
  summarize(sum_acc = length(accommodates)) %>% 
  ggplot(aes(x = factor(accommodates), y = sum_acc))+
  geom_bar(stat = "identity", color = "navyblue", fill = "deepskyblue", size = 1.5)+
  xlab("Number of Guests Accommodated")+
  ylab("Total Number of Listings")+
  ggtitle("Listings Sum by Accommodation Total")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

#Number of Guests Accommodated & Mean Price

airbnb %>% group_by(accommodates) %>% summarize(mean_price = mean(price)) %>%
  ggplot(aes(accommodates, mean_price))+ 
  geom_bar(stat = "identity", color = "orange", 
           size = 2, fill = "pink")+
  xlab("Accommodates")+
  ylab("Mean Price")+
  ggtitle("Number of Guests Accommodated & Mean Price")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))



airbnb %>% group_by(bathrooms) %>% summarize(mean_price = mean(price)) %>%
  ggplot(aes(bathrooms, mean_price))+
  geom_bar(stat = "identity", fill = "deepskyblue", color = "navyblue", size = 1.2)+
  xlab("Number of Bathrooms")+
  ylab("Mean Price per Night")+
  ggtitle("Mean Price per Number of Bathrooms")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))


#Mean Price per Number of Bathrooms

airbnb %>% group_by(bathrooms) %>% summarize(sum_bath = length(bathrooms)) %>%
  ggplot(aes(reorder(bathrooms, sum_bath), y=sum_bath, label = sum_bath))+
  geom_bar(stat = "identity", fill = "deepskyblue", color = "cyan", size = 1.2)+
  coord_flip()+
  geom_text(size = 5, color = "navyblue",
            position = position_stack(vjust = 0.5))+
  xlab("Number of Bathrooms")+
  ylab("Total Number of Listings")+
  ggtitle("Listing Distribution by Bathroom Total")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

#Listing Distribution by Bathroom Total
airbnb %>% group_by(bathrooms) %>% summarize(sum_bath = length(bathrooms)) %>%
  ggplot(aes(reorder(bathrooms, sum_bath), y=sum_bath, label = sum_bath))+
  geom_bar(stat = "identity", fill = "deepskyblue", color = "yellow", size = 1.2)+
  coord_flip()+
  geom_text(size = 5, color = "navyblue",
            position = position_stack(vjust = 0.5))+
  xlab("Number of Bathrooms")+
  ylab("Total Number of Listings")+
  ggtitle("Listing Distribution by Bathroom Total")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))


#details the mean price per listing versus the number of beds available
airbnb %>% group_by(beds) %>% 
  summarize(mean_price = mean(price)) %>%
  ggplot(aes(beds, mean_price))+
  geom_bar(stat = "identity", 
           color = "navyblue", fill = "green", size = 1.5)+
  xlab("Number of Beds")+
  ylab("Mean Price per Night")+
  ggtitle("Mean Price Distribution by Beds Total")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))



#Listing quantity by number of beds

airbnb %>% group_by(beds) %>% summarize(sum_beds = length(beds)) %>%
  ggplot(aes(reorder(beds, sum_beds), y = sum_beds, label = sum_beds))+
  geom_bar(stat = "identity", 
           color = "cyan", fill = "yellow", size = 1.5)+
  coord_flip()+
  geom_text(size = 5, color = "navyblue",
            position = position_stack(vjust = 0.5))+
  xlab("Total Number of Listings")+
  ylab("Listing Distribution by Bedroom Total")+
  ggtitle("Number of Beds")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))


# Total number of reviews and different price

airbnb %>% filter(price <= 1000) %>% 
  ggplot(aes(x = number_of_reviews, y = price))+ 
  geom_point(color="red", alpha = 0.6, size = 1.5)+
  xlab("Number of Reviews")+
  ylab("Price")+
  ggtitle("Review vs Price Distribution")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))



#Compare mean price and rating of different locations
str(airbnb)

rating_comp <- airbnb %>%
  filter(!is.na(review_scores_rating) & !is.na(price)) %>%
  group_by(neighbourhood_group_cleansed) %>%
  summarize(mean_rating = mean(review_scores_rating), mean_price = mean(price)) %>%
  select(neighbourhood_group_cleansed, mean_rating, mean_price)

# Set the parameters for the dual-axis plot:
ylim_1 <- c(0, 100)
ylim_2 <- c(70, 400)
b <- diff(ylim_1) / diff(ylim_2)
a <- b * (ylim_1[1] - ylim_2[1])

# Plot the Barplot (Rating) with Overlapping Line (Price):
ggplot(rating_comp, aes(neighbourhood_group_cleansed, group = 1)) +
  geom_bar(aes(y = mean_rating), stat = "identity", color = "navyblue", alpha = 0.7) +
  geom_line(aes(y = a + mean_price * b), color = "red", size = 2) +
  scale_y_continuous(
    name = "Mean Review Score",
    sec.axis = sec_axis(~(. - a) / b, name = "Mean Price")
  ) +
  xlab("Neighbourhood Group") +
  ggtitle("Review Score & Price Comparison by Neighbourhood Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))



# Create the scatter plot
#-------------------------------------------#
###########          9           ############
#-------------------------------------------#

#Zbadanie zależności pomiędzy zmiennym

########################################
#--------------------------------------#
######         Correlations       ######
#--------------------------------------#
########################################

#correlation matrix for price




# Select features for normalization
selected_features <- airbnb[, c('accommodates', 'latitude', 'longitude', 'calculated_host_listings_count', 
                                'availability_30', 'availability_60', 'availability_90', 'availability_365',
                                'beds', 'price', 'minimum_nights', 'maximum_nights','bathrooms',
                                'number_of_reviews', 'review_scores_rating', 'review_scores_value', 'reviews_per_month')]

# Logarithmic transformation of 'price'
selected_features$log_price <- log(selected_features$price + 1)

# Normalize selected features including 'price'
features_to_normalize <- selected_features[, c('price', 'accommodates', 'latitude', 'longitude', 'calculated_host_listings_count', 
                                               'availability_30', 'availability_60', 'availability_90', 'availability_365',
                                               'beds', 'minimum_nights', 'maximum_nights',
                                               'number_of_reviews', 'review_scores_rating', 'review_scores_value', 'reviews_per_month', 'bathrooms')]

# Normalize all features
normalized_data <- scale(features_to_normalize)

# Calculate correlation matrix for normalized features
cor_matrix_normalized <- cor(normalized_data)

# Create a correlation heatmap for normalized features
options(repr.plot.width = 50, repr.plot.height = 30)
corrplot(cor_matrix_normalized , method = "color", col = colorRampPalette(c("white", "red"))(20), 
         addCoef.col = "black", number.cex = 0.8, tl.cex = 0.8)

# Calculate correlation matrix for logarithmically transformed 'price'
cor_matrix_log_price <- cor(cbind(selected_features$log_price, features_to_normalize))


# Print correlation matrix as a table
print(cor_matrix_normalized)




