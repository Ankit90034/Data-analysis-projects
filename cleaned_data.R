# load the required packages

install.packages(c("readxl","dplyr","lubridate","tidyverse"))
library(readxl)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)

# import the csv file

file_path <- "C:/Users/DELL/Downloads/DA_files/Citi-Bike-Trip-Data.csv"

df <- read_csv(file_path)

# check the structure of the file

str(df)
head(df)
colnames(df)

# check for missing values
colSums(is.na(df))

# drop the '...2' column as it is empty
df <- df %>%
  select(-`...2`)


# fill the missing values with median

df$tripduration[is.na(df$tripduration)] <- median(df$tripduration, na.rm = TRUE)
df$starttime[is.na(df$starttime)] <- median(df$starttime, na.rm = TRUE)


# filling the empty values with mode
# mode function
get_mode <- function(v) {
  uniq_values <- unique(v)
  uniq_values[which.max(tabulate(match(v, uniq_values)))] 
}

# filling the missing values with mode
mode_user_type <- get_mode(df$usertype) 
df$usertype[is.na(df$usertype)] <- mode_user_type  

# median is used when missing values are numerical while mode is 
# used when missing values are categorical

# checking if there are still any missing values
colSums(is.na(df))

# rename the columns for readability

colnames(df) <- c("Trip_Duration", "Start_Time", "Stop_Time","Start_Station_id","Start_station_name",
                  "Start_Station_latitude","Start_Station_longitude", "End_Station_id","End_station_name",
                  "End_Station_latitude","End_Station_longitude", "Bike_ID", "User_Type", 
                  "Birth_Year", "Gender")

colnames(df)

View(df)

# convert the datatype
# convert to datetime columns
df$Start_Time <- as.POSIXct(df$Start_Time, format="%Y-%m-%d %H:%M:%S")
df$Stop_Time <- as.POSIXct(df$Stop_Time, format="%Y-%m-%d %H:%M:%S")

# convert the User_Type column
df$User_Type <- as.factor(df$User_Type)

# create new columns
# extract hour day and month
df$Start_Hour <- hour(df$Start_Time)
df$Start_Day <- day(df$Start_Time)
df$Start_Month <- month(df$Start_Time, label = TRUE)

# trip duration in minutes
df$Trip_Duration_Minutes <- as.numeric(difftime(df$Stop_Time, df$Start_Time, units = "mins"))

colnames(df)

# remove the rows where Start_Time is greater than Stop_Time
df <- df %>% filter(Trip_Duration_Minutes > 0)

View(df)

# Data analysis
# summary of data
summary(df)

# most popular stations
most_papular_stations <- df %>%
  count(Start_station_name, sort = TRUE)

print(most_papular_stations)

# most popular start and end stations
most_popular_Start_Station <- df %>%
  count(Start_station_name, sort = TRUE) %>%
  head(1)

print(most_popular_Start_Station)

# most popular end station
most_popular_end_station <- df %>%
  count(End_station_name, sort = TRUE) %>%
  head(1)

print(most_popular_end_station)

# peak uses hour

peak_uses_hour <- df %>%
  group_by(Start_Hour) %>%
  summarise(Trips = n()) %>%
  arrange(desc(Trips))

print(peak_uses_hour)

# user type by gender
user_type_by_gender <- df %>%
  group_by(User_Type, Gender) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

print(user_type_by_gender)

# bike uses analysis

bike_uses<- df %>%
  count(Bike_ID, sort = TRUE) %>%
  head(10)
print(bike_uses)


# data visualization

# User_Type -- Whether user is Customer or Subscriber
ggplot(data = df) + geom_bar(mapping = aes(x =User_Type,
  fill = User_Type)) +
  theme_minimal() +
  labs(title = "Distribution of User Types",x = 'User Type', y = 'Strength')

# trips over time
ggplot(data = df) + geom_histogram(mapping = aes(x = Start_Time), fill = "blue",
  bins = 50, alpha = 0.6) +
  theme_minimal() +
  labs(title = "Trips Over Time", x = "Start Time", y = "Trips")

# bike uses by hour and day
df %>%
  group_by(Start_Day, Start_Hour) %>%
  summarise(Trips = n()) %>%
  ggplot(aes(Start_Hour, Start_Day, fill = Trips)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Bike Usage Heatmap", x = "Hour", y = "Day")

# bike uses column chart
df %>%
  count(Bike_ID) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(Bike_ID, n), y = n)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(title = "Top 10 Most Used Bikes", x = "Bike ID", y = "Number of Trips")

# save the file
write.csv(df, "Cleaned_Citi_Bike_Data.csv", row.names = FALSE)











