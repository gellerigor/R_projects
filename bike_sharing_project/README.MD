# **About Dataset**

*Data collected from [https://divvy-tripdata.s3.amazonaws.com/index.html](https://divvy-tripdata.s3.amazonaws.com/index.html) which is Chicago's 
public bike trip data. The data compiled corresponds to 12 months, namely the period from May 2022 through April 2023.* 

**Cyclistic/Divvy Data**

Cyclistic is a fictional company, the original datasets have the name Divvy. This data consists of public historical bike trip data from the city of Chicago.

The data has been made available by Motivate International Inc. under [this license](https://divvybikes.com/data-license-agreement)

This is public data that you can use to explore how different customer types are using Cyclistic bikes. But note that data-privacy issues prohibit you from using riders’ personally identifiable information. This means that you won’t be able to connect pass purchases to credit card numbers to determine if casual riders live in the Cyclistic service area or if they have purchased multiple single passes.

# **Background**

 **Case Study: How Does a Bike-Share Navigate Speedy Success?**

Imagine fictional company Cyclistic a bike-share company in Chicago. The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. Therefore, your team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, your team will design a new marketing strategy to convert casual riders into annual members. But first, Cyclistic executives must approve your recommendations, so they must be backed up with compelling data insights and professional data visualizations.

 **About the company**

In 2016, Cyclistic launched a successful bike-share offering. Since then, the program has grown to a fleet of 5,824 bicycles that are geotracked and locked into a network of 692 stations across Chicago. The bikes can be unlocked from one station and returned to any other station in the system anytime.
Until now, Cyclistic’s marketing strategy relied on building general awareness and appealing to broad consumer segments. One approach that helped make these things possible was the flexibility of its pricing plans: single-ride passes, full-day passes, and annual memberships. Customers who purchase single-ride or full-day passes are referred to as casual riders. Customers who purchase annual memberships are Cyclistic members.

Cyclistic’s finance analysts have concluded that annual members are much more profitable than casual riders. Although the pricing flexibility helps Cyclistic attract more customers, Moreno believes that maximizing the number of annual members will be key to future growth. Rather than creating a marketing campaign that targets all-new customers, Moreno believes there is a very good chance to convert casual riders into members. She notes that casual riders are already aware of the Cyclistic program and have chosen Cyclistic for their mobility needs.

Moreno has set a clear goal: Design marketing strategies aimed at converting casual riders into annual members. In order to do that, however, the marketing analyst team needs to better understand how annual members and casual riders differ, why casual riders would buy a membership, and how digital media could affect their marketing tactics. Moreno and her team are interested in analyzing the Cyclistic historical bike trip data to identify trends.

**The task is to figure out:**
1. How do annual members and casual riders use Cyclistic bikes differently?
2. Why would casual riders buy Cyclistic annual memberships?
3. How can Cyclistic use digital media to influence casual riders to become members?

# Process

For this exploratory analysis, we'll use R along with additional packages. The most important of them will be packages collected in the "tidyverse" and "lubridate" packages, which allow for easy manipulation, preparation, analysis and visualization of data and also lubridate that make dealing with dates a little easier.

```
#combine each month into 1 dataframe  
all_trips <- bind_rows(tripdata_2022_05, tripdata_2022_06, tripdata_2022_07, tripdata_2022_08, tripdata_2022_09, tripdata_2022_10, 
                       tripdata_2022_11, tripdata_2022_12, tripdata_2023_01, tripdata_2023_02, tripdata_2023_03,
                       tripdata_2023_04,)

colnames(all_trips)
glimpse(all_trips)
dim(all_trips)
summary(all_trips)
head(all_trips)
```
![](/bike_sharing_project/images/img_2.png)

Add columns that list the date, month, day, and year of each ride in order to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level more on date format in R found at that link.

```
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

colnames(all_trips)
```

Add a "ride_length" calculation to all_trips (in seconds) , than convert "ride_length" from double to numeric so we can run calculations on the data.

```
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

str(all_trips)

# Convert "ride_length" 
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
```

With summary() function we can check the ride_length column and see that we have negative values and outliers , remove wrong data such as negative values(<= 0), outliers like values more than 8640 seconds (24 hours) Total number of rows 5799014 (drop from 5829030 decrease by 0.5% compare to amount of data its not much but very important)

```
all_trips_v2 <- all_trips[!(all_trips$ride_length<0 | all_trips$ride_length>8640),]
#Inspect new data
dim(all_trips_v2)
```

# Analysis

Lets make some descriptive analysis:
1. compare members and casual users
2. see the average ride time by each day for members vs casual users.

Notice that the days of the week are out of order , let's fix that to.

Now lets analyze ridership data by type and weekday

```
summary(all_trips_v2)
# compare users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
```
![](/bike_sharing_project/images/img_3.png)

```
# average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
```
![](/bike_sharing_project/images/img_4.png)

```
# order days
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run again the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
```
![](/bike_sharing_project/images/img_5.png)

```
#Assign the correct order to each month of the year
all_trips_v2$month <-
    ordered(all_trips_v2$month, levels = c('05', '06', '07', '08', '09', '10', '11', '12', '01', '02', '03', '04'))
```

Lets see difference between members and casual riders by month.

```
all_trips_v2 %>%
    group_by(member_casual, month) %>%
    summarise(number_of_ride = n(), .groups = 'keep') %>%
    arrange(month)
```
![](/bike_sharing_project/images/img_6.png)


```
all_trips_v2 %>%
    group_by(member_casual, month) %>%
    summarise(average_ride_length = mean(ride_length), .groups = 'drop') %>%
    arrange(month)
```
![](/bike_sharing_project/images/img_7.png)


Analyze ridership data by type and weekday.

```
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length), .groups = "keep") %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts
```
![](/bike_sharing_project/images/img_8.png)



# Visualization 

Let's visualize the number of rides by rider type to clarify our findings.

```
# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length), .groups = "keep") %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
```
![](/bike_sharing_project/images/rides_by_type.png)


```
all_trips_v2 %>%
mutate(weekday = wday(started_at, label = TRUE)) %>% 
    group_by(member_casual, month) %>%
    summarise(number_of_rides = n(), .groups = 'drop') %>%
    ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) + 
    geom_bar(position = "dodge", stat = "identity")
```
![](/bike_sharing_project/images/monthly_rides.png)

**Finding**

1. Casual riders are more likely to take a ride on weekend while membership riders use on weekday more often.
2. Spring is the peak season for both rider types.


Let's create a visualization for average duration


```
# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length), .groups = "keep") %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
```
![](/bike_sharing_project/images/avg_duration.png)


```
all_trips_v2 %>%
    group_by(member_casual, month) %>%
    summarise(average_duration = mean(ride_length), .groups = 'keep') %>%
    ggplot(aes(x = month, y = average_duration, fill = member_casual)) + 
    geom_bar(position = "dodge", stat = "identity")
```
![](/bike_sharing_project/images/duration.png)


**Finding**

1. Casual rider's trip is longer than members ones regardless of the season or day.
2. All users take longer trips over weekend and summer.


**Conclusion**

Overall, these ridership patterns suggest that bike sharing is a popular mode of transportation for both casual and membership riders. It is especially popular during the spring months, when the weather is more favorable for biking. Casual riders tend to take longer trips than members riders, regardless of the season or day. However, casual riders take slightly longer trips  membership riders tand to take rides more often.







