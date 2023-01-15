---
output:
  html_document: default
  pdf_document: default
---

```{r cars}
summary(cars)
```
# Case Study - Bellabeat
#### This is a case study for Google Data Analytics Professional Certificate

## Introduction
You are a junior data analyst working on the marketing analyst team at Bellabeat, a high-tech manufacturer of health-focused
products for women. Bellabeat is a successful small company, but they have the potential to become a larger player in the global smart device market. Urška Sršen, cofounder and Chief Creative Officer of Bellabeat, believes that analyzing smart device fitness data could help unlock new growth opportunities for the company. You have been asked to focus on one of Bellabeat’s products and analyze smart device data to gain insight into how consumers are using their smart devices. The insights you discover will then help guide marketing strategy for the company. You will present your analysis to the Bellabeat executive team along with your high-level recommendations for Bellabeat’s marketing strategy.

## Question
1. What are some trends in smart device usage?
2. How could these trends apply to Bellabeat customers?
3. How could these trends help influence Bellabeat marketing strategy?

## Upload packages and databases

```{r}
library(tidyverse)
library(ggplot2)
library(janitor)
library(here)
library(dplyr)
library(skimr)
library(lubridate)
library(tidyr)
```

```{r}
daily_activity <- read.csv ("C:\\Users\\vielu\\OneDrive\\Documents\\Data Analysis\\Case Study - Fitbit\\dailyActivity_merged.csv")
sleep_day <- read.csv ("C:\\Users\\vielu\\OneDrive\\Documents\\Data Analysis\\Case Study - Fitbit\\sleepDay_merged.csv")
weight_info <- read.csv ("C:\\Users\\vielu\\OneDrive\\Documents\\Data Analysis\\Case Study - Fitbit\\weightLogInfo_merged.csv")
```

## Cleaning data
```{r eval=TRUE}
glimpse(daily_activity)
glimpse(sleep_day)
glimpse(weight_info)
```
Organize the Id format to Character
```{r eval=TRUE}
daily_activity <- daily_activity %>% 
  mutate(Id = as.character(Id))
sleep_day <- sleep_day %>% 
  mutate(Id = as.character(Id))
weight_info <- weight_info %>% 
  mutate(Id = as.character(Id))
```
Organize the date and time format
```{r eval=TRUE}
daily_activity <- daily_activity %>%
  mutate(ActivityDate = as_date(ActivityDate, format = "%m/%d/%Y"))
```

```{r eval=TRUE}
sleep_day <- separate(sleep_day, SleepDay, into=c('Date','Time'), sep=' ')
```
```{r eval=TRUE}
sleep_day <- sleep_day %>% 
  mutate(Date = as_date(Date, format = "%m/%d/%Y"))
```

```{r eval=TRUE}
sleep_day <- sleep_day %>% 
  mutate(Time = as.POSIXct(Time, format = "%I:%M:%S"))
```
Remove duplicate values
```{r eval=TRUE}
sum(duplicated(daily_activity))
sum(duplicated(sleep_day))
sum(duplicated(weight_info))
```
```{r eval=TRUE}
sleep_day <- sleep_day %>% 
  distinct() %>%
  drop_na()
```

##Analyze
We can check out each user´s mean steps, distance, calories spent and total number of day used the app.
```{r eval=TRUE}
Mean_activity_by_id <- daily_activity %>% 
  group_by(Id) %>%
  summarize (MeanSteps = mean(TotalSteps), MeanDistance = mean(TotalDistance), MeanCalories = mean(Calories), TotalActiveDay = n())
```
```{r eval=TRUE}
daily_activity %>% 
  summarize(TotalMean = mean(TotalSteps))
```
Generate a bar chart about the mean stpes of user
```{r eval=TRUE}
ggplot(Mean_activity_by_id)+
  geom_col(mapping=aes(x=Id, y=MeanSteps))+
  geom_hline(yintercept = 7519)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "Mean Steps per User compare to the Total Mean Steps")
```
Calculate the percentage of user type
```{r eval=TRUE}
Mean_activity_by_id <- Mean_activity_by_id%>% 
  mutate(UserType = case_when(
    MeanCalories < 1000 ~ "Inactive",
    MeanCalories >= 1000 & MeanCalories <2000 ~ "Slightly Active",
    MeanCalories >= 2000 & MeanCalories <3000 ~ "Active",
    MeanCalories >= 3000 ~ "Very Active"
  ))
```
```{r eval=TRUE}
UserType <- Mean_activity_by_id %>% 
  group_by(UserType) %>%
  count(UserType)
```
```{r eval=TRUE}
UserType <- UserType %>% 
mutate(UserType, Percent=n/33*100)
```
```{r eval=TRUE}
UserType <- UserType %>% 
  mutate_if(is.numeric, round, digits=2)
```
Generate the pie chart
```{r eval=TRUE}
ggplot(UserType, aes(x="", y=Percent, fill=UserType))+
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  geom_text(aes(y=Percent, label = Percent),position = position_stack(vjust=0.5))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  labs(title="Percentage of User Type")
```
Calculate usage frequency during the week
```{r eval=TRUE}
daily_activity <- daily_activity %>% 
  mutate(Weekday = weekdays(ActivityDate))
```
Since my computer´s system language is in Portuguese, the weekday´s name came out in Portuguese and I need to substitute them with English.
```{r eval=TRUE}
rep_str = c('domingo'='Sunday','sábado'='Saturday','sexta-feira'='Friday','quinta-feira'='Thursday','quarta-feira'='Wednesday', 'terça-feira'='Tuesday', 'segunda-feira'='Monday
')
daily_activity$Weekday <- str_replace_all(daily_activity$Weekday, rep_str)
```

```{r eval=TRUE}
Week_activity <- daily_activity %>% 
  group_by(Weekday) %>% 
  count(Weekday)
```
```{r eval=TRUE}
Week_activity <- Week_activity %>% 
  mutate(NumWeekActivity =n)
```
```{r eval=TRUE}
Week_activity <- Week_activity %>% 
  mutate(Percent=NumWeekActivity/940*100)
```
```{r eval=TRUE}
Week_activity <- Week_activity %>% 
  mutate_if(is.numeric, round, digits=2)
```

Generate a pie chart about the percentage of use during the week
```{r eval=TRUE}
ggplot(Week_activity, aes(x="", y=Percent, fill=Weekday))+
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  geom_text(aes(y=Percent, label = Percent),position = position_stack(vjust=0.5))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  labs(title="Percentage of usage during the Week")
```

## Conclustion

1. From the analysis we can see that most of the pp users are Active user, they spent around 2000-3000 calories per day.
2. Almost 50% usage of app happened during the middle of the week, especially on Tuesday. 
3. The marketing team can target on the slightly active user and focus on Thuesday, Wednesday and Thursday.
