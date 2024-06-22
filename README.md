# <span style="color:#575656">Load Packages </span>

```{r libraries, results='hide', warning=FALSE, error=FALSE}
library(dplyr)
library(lubridate)
library(tidyverse)
setwd('DATA_PATH')
gctorture(on = FALSE)
```
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

```

# <span style="color:#575656">Business Task (Ask): </span>

Cyclistic’s finance analysts have concluded that annual members are much more profitable than casual riders. Consequently, maximizing the number of annual members will be key to future growth. According to the information of the current situation, it would be the best to take necessary measures to convert casual user into members.

In this report, it's going to be defined how does casual and member users differ in the use of the Cyclistic's services.

- How do annual members and casual riders use Cyclistic bikes differently?

- Find Insights about users trends

- Define how to apply those findings to business Marketing campaign 

## <span style="color:#408000">Import Data sets</span>

For this analysis, I was granted access to a database of users of the bike sharing system. This data contains information about stations, ride time, distance, etc, and most important there is a variable that determines whether the user is a member or a casual user. Data from: [*Index of bucket "divvy-tripdata"*](https://divvy-tripdata.s3.amazonaws.com/index.html)

```{r Importing Data sets}
nov_2022 <- read.csv("Cyclist_data/202211-divvy-tripdata.csv")
dec_2022 <- read.csv("Cyclist_data/202212-divvy-tripdata.csv")
jan_2023 <- read.csv("Cyclist_data/202301-divvy-tripdata.csv")
feb_2023 <- read.csv("Cyclist_data/202302-divvy-tripdata.csv")
mar_2023 <- read.csv("Cyclist_data/202303-divvy-tripdata.csv")
apr_2023 <- read.csv("Cyclist_data/202304-divvy-tripdata.csv")
may_2023 <- read.csv("Cyclist_data/202305-divvy-tripdata.csv")

```

# <span style="color:#575656">Prepare</span>

## <span style="color:#408000">Merge data</span>

Making sure the column names are equivalent, so there wont be any error when joining the data sets.
```{r display column names, results='hide'}
colnames(nov_2022)
colnames(dec_2022)
colnames(jan_2023)
colnames(feb_2023)
colnames(mar_2023)
colnames(apr_2023)
colnames(may_2023)
```

## <span style="color:#408000">Inspect Variable formats</span>

```{r Inspect column content, results=FALSE}
# Return a vertical column preview for each data set
str(nov_2022)
str(dec_2022)
str(jan_2023)
str(feb_2023)
str(mar_2023)
str(apr_2023)
str(may_2023)
```
By observing the column information, a conclusion is that the columns formats are consistent even if the end_station_name and end_station_id for April and May 2023 have empty cells.

Now that column names are consistent, its time to combine the data sets into one data frame so calculations can be applied.

## <span style="color:#408000"> Combine quarter's data frames into one big data frame </span>

```{r fusion of month sheets}
all_trips <- bind_rows(nov_2022, dec_2022, jan_2023, feb_2023, mar_2023, apr_2023, may_2023)
```

## <span style="color:#408000"> Eliminatin unuseful/discontinuated data columns to optimize and clarify the analysiss </span>

```{r eliminate unuseful columns}
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

```
# <span style="color:#575656">Process</span>

## <span style="color:#408000">Inspect the new filtered data</span>

```{r Inspect, results=FALSE, warning=FALSE}
colnames(all_trips) #Column names
nrow(all_trips)#Number of rows
dim(all_trips)#Dimensions
summary(all_trips)#summary
head(all_trips)#preview

```

## <span style="color:#408000">Inspect the different values in columns</span>
How many users of each type for the last seven months? 
```{r column values inspection}
table(all_trips$member_casual)

```
As we can see, the column casual_member doesn't has incorrect values, since the only two values are "member" or "casual" describing which type of user contains each row.

## <span style="color:#408000">Split datetime column into date, month, day, and year</span>

This will allow us to aggregate ride data for each month, day, or year

```{r split date column, warning=FALSE}
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")#month
all_trips$day <- format(as.Date(all_trips$date), "%d")#day
all_trips$year <- format(as.Date(all_trips$date), "%Y")#year
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")#day of the week

```

## <span style="color:#408000"> Calculate ride duration in seconds, and add results in a new columns</span>

```{r ride length}
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
is.numeric(all_trips$ride_length)#ride duration must be numerical data, this line verifies it
```

Now there is a new column with the ride duration information, but it is not in numeric format, so now its necessary to convert the column into numeric.

## <span style="color:#408000"> Convert ride_length into numeric </span>
```{r convert to numeric}
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)#verify if the column is numeric
```

The data frame includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative. It is necessary to create a new version of the data since data is being removed.

## <span style="color:#408000"> Create a new version of the data without "bad" data</span>

In the data set, there are many negative values on ride_length column, this may affect the results, so it must be deleted from the data.

```{r all_trips_v2}
all_trips_v2 <- all_trips[!(all_trips$ride_length<0),]
head(all_trips_v2 %>% filter(ride_length<0))#verify that the column ride_length does,'t has negative values
```
As we can see, the new data (all_trips_v2) doesn't any ride length in negative.
 
# <span style="color:#575656">Descriptive Analysis</span>

Applying statistical measures to analyse the trip duration in a descriptive way.
```{r statistical measures on ride_length}
ride_length_clean <- all_trips_v2$ride_length
print(paste("ride_length's Min value",min(ride_length_clean))) #shortest ride
summary(ride_length_clean)
```

## <span style="color:#408000"> Compare members and casual users </span>

Compare users data by applying statistical measure to each type of user.
```{r Compare members, warning=FALSE}
aggregate(ride_length_clean ~ all_trips_v2$member_casual, FUN = mean)#group by member_casual variants (casual and member) then apply the FUN function to calculate the mean of each group
aggregate(ride_length_clean ~ all_trips_v2$member_casual, FUN = median)#the median of each group
aggregate(ride_length_clean ~ all_trips_v2$member_casual, FUN = max)
aggregate(ride_length_clean ~ all_trips_v2$member_casual, FUN = min)

```

## <span style="color:#408000"> See the average ride time by each day for members vs casual users </span>

```{r The average ride time by day}
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))#make sure the days are in right order

all_trips_v2 %>% 
  aggregate(ride_length ~ member_casual + day_of_week, FUN = mean)

```

As it is observed in the table, the average ride time by the casual users is significantly higher the the members users.

## <span style="color:#408000"> Analyze ridershp by user type and day-of-week </span>

```{r ridershp by user type and day-of-week, warning=FALSE, error=FALSE}
library(lubridate) #for wday() to find day of week then organize by day order
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday() from lubridate
  group_by(member_casual, weekday) %>%  #groups by user-type and weekday
  summarise(number_of_rides = n()							#calculates the number of rides
  ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

```
By analyzing the results table, we can notice that: 1) The general number of rides are significantly higher at weekends, 2) the average duration is higher for casual users even if they made less rides, this mean that the casual users trips are longer that members trips.

# <span style="color:#575656">Viz Analysis</span>

## <span style="color:#408000"> Number of rides by rider type </span>

```{r Bar Graph of Number of rides by rider type, warning=FALSE}
colour_scl <- c("#306000", "#f7a92c")
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge")+scale_fill_manual(values = colour_scl)
```

## <span style="color#408000"> Average trip duration per day-of-week </span>

```{r, trip duration per day-of-week, warning=FALSE}
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + geom_col(position = "dodge")+scale_fill_manual(values = colour_scl)

```


```{r echo=FALSE}
member_users <- all_trips %>% 
  filter(member_casual == "member") %>% 
  count(member_casual)
casual_users <- all_trips %>% 
  filter(member_casual == "casual") %>% 
  count(member_casual)


member_table <- all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n() ,duration = sum(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  filter(member_casual == "member")
casual_table <- all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n() ,duration = sum(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  filter(member_casual == "casual")
View(casual_table)

casual_duration <- sum(casual_table$duration)
member_duration <- sum(member_table$duration)
print(paste("Member users total trip duration: ", member_duration))
print(paste("Casual users total trip duration: ",casual_duration))
print(paste("Total differenece in duration using bikes between members and casual users in last 7 months: ",((member_duration-casual_duration)/60)/60))
print(paste("Total hour of use from casual users", ((casual_duration)/60)/60))
print(paste("Total hour of use from members users",((member_duration)/60)/60))

```

```{r echo=FALSE}
colour_scl <- c("#408000", "#f7a92c")
all_trips_v2 %>% 
  mutate(month = month(started_at, label = TRUE)) %>%
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), duration = sum(ride_length)) %>% 
  arrange(member_casual, month) %>% 
  #ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge")+scale_fill_manual(values = colour_scl) %>% 
  ggplot(aes(x = month, y = duration, fill = member_casual)) + geom_col(position = "dodge")+scale_fill_manual(values = colour_scl)


```

# <span style="color:#575656"> Observations: </span>

1) From November/2022 to May/2023 the number of member users is 1,518,025 and the casual users 672,357, with a difference of 845,668 more members than casual users.

2) Saturday was the day with more Casual users with 112,973 rides, while Tuesday was the day with more members users with 266,528 rides.

3) The ride time for both user type is: 289,682.6 for members, and 285,833.7 for casual users, with a difference of 3,849.6 ride hours.

# <span style="color:#575656">Conclutions</span>

- Casual users make longer trips than members, especially on Sundays which is when longest trip average is recorded.

- Members do more rides per day than casual users, being Tuesday the day with more rides by members.

- There is an increment of rides by casual users on weekends, Saturday is the day with more rides by casual users.

- Data suggest that casual users mostly use the bikes for recreational purposes, on the other hand, the huge increase on the total user rides by members on work days says that most of members may use the bikes to go to work or doing work trips.

# <span style="color:#575656">Hypothesis</span>

After seeing the increase on average trip duration from casual users in weekend, it can be concluded that casual users mostly use the bikes for recreational purposes, on the other hand, the huge increase on the total user rides by members on work days says the opposite.

# <span style="color:575656"> Recommendations </span>

- The significant increase in total use duration indicates that now is the best time to start an effective campaign.

- Focus the campaign in the most commercial stations.

- Given the results from average trip duration  and the average number of trips for each day-of-week, the  casual users spend more time in trips on weekends. To convert the most casual users into members, it is recommendable to focus the marketing campaign in weekends in order to reach the most casual users possible.
