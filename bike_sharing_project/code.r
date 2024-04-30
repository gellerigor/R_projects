# Run all nessesary packages
library(tidyverse)
library(lubridate)

#Upload all data 
tripdata_2022_05 <- read_csv("/kaggle/input/cyclistic-bike-share-may-2022-though-april-2023/202205-divvy-tripdata.csv")
tripdata_2022_06 <- read_csv("/kaggle/input/cyclistic-bike-share-may-2022-though-april-2023/202206-divvy-tripdata.csv")
tripdata_2022_07 <- read_csv("/kaggle/input/cyclistic-bike-share-may-2022-though-april-2023/202207-divvy-tripdata.csv")
tripdata_2022_08 <- read_csv("/kaggle/input/cyclistic-bike-share-may-2022-though-april-2023/202208-divvy-tripdata.csv")
tripdata_2022_09 <- read_csv("/kaggle/input/cyclistic-bike-share-may-2022-though-april-2023/202209-divvy-publictripdata.csv")
tripdata_2022_10 <- read_csv("/kaggle/input/cyclistic-bike-share-may-2022-though-april-2023/202210-divvy-tripdata.csv")
tripdata_2022_11 <- read_csv("/kaggle/input/cyclistic-bike-share-may-2022-though-april-2023/202211-divvy-tripdata.csv")
tripdata_2022_12 <- read_csv("/kaggle/input/cyclistic-bike-share-may-2022-though-april-2023/202212-divvy-tripdata.csv")
tripdata_2023_01 <- read_csv("/kaggle/input/cyclistic-bike-share-may-2022-though-april-2023/202301-divvy-tripdata.csv")
tripdata_2023_02 <- read_csv("/kaggle/input/cyclistic-bike-share-may-2022-though-april-2023/202302-divvy-tripdata.csv")
tripdata_2023_03 <- read_csv("/kaggle/input/cyclistic-bike-share-may-2022-though-april-2023/202303-divvy-tripdata.csv")
tripdata_2023_04 <- read_csv("/kaggle/input/cyclistic-bike-share-may-2022-though-april-2023/202304-divvy-tripdata.csv")

#inspect column names of each month 
colnames(tripdata_2022_05)
colnames(tripdata_2022_06)
colnames(tripdata_2022_07)
colnames(tripdata_2022_08)
colnames(tripdata_2022_09)
colnames(tripdata_2022_10)
colnames(tripdata_2022_11)
colnames(tripdata_2022_12)
colnames(tripdata_2023_01)
colnames(tripdata_2023_02)
colnames(tripdata_2023_03)
colnames(tripdata_2023_04)


#combine each month into 1 dataframe  
all_trips <- bind_rows(tripdata_2022_05, tripdata_2022_06, tripdata_2022_07, tripdata_2022_08, tripdata_2022_09, tripdata_2022_10, 
                       tripdata_2022_11, tripdata_2022_12, tripdata_2023_01, tripdata_2023_02, tripdata_2023_03,
                       tripdata_2023_04,)

colnames(all_trips)
glimpse(all_trips)
dim(all_trips)
summary(all_trips)
head(all_trips)


all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

colnames(all_trips)



all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

str(all_trips)

# Convert "ride_length" 
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)


all_trips_v2 <- all_trips[!(all_trips$ride_length<0 | all_trips$ride_length>8640),]
#Inspect new data
dim(all_trips_v2)


summary(all_trips_v2)
# compare users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)


# average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


# order days
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


# Now, let's run again the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


#Assign the correct order to each month of the year
all_trips_v2$month <-
    ordered(all_trips_v2$month, levels = c('05', '06', '07', '08', '09', '10', '11', '12', '01', '02', '03', '04'))


all_trips_v2 %>%
    group_by(member_casual, month) %>%
    summarise(number_of_ride = n(), .groups = 'keep') %>%
    arrange(month)



all_trips_v2 %>%
    group_by(member_casual, month) %>%
    summarise(average_ride_length = mean(ride_length), .groups = 'drop') %>%
    arrange(month)



all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length), .groups = "keep") %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts



# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length), .groups = "keep") %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")




all_trips_v2 %>%
mutate(weekday = wday(started_at, label = TRUE)) %>% 
    group_by(member_casual, month) %>%
    summarise(number_of_rides = n(), .groups = 'drop') %>%
    ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) + 
    geom_bar(position = "dodge", stat = "identity")




# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length), .groups = "keep") %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")




all_trips_v2 %>%
    group_by(member_casual, month) %>%
    summarise(average_duration = mean(ride_length), .groups = 'keep') %>%
    ggplot(aes(x = month, y = average_duration, fill = member_casual)) + 
    geom_bar(position = "dodge", stat = "identity")







