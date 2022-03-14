# 
#   Capstone Project - "Bellabeat" - Fit Bit Tracker data analysis
#     Project background:
#       Bellabeat's co-founder Urška Sršen has requested the analysis of publicly available FitBit tracker data,
#       to identify a market strategy that will help increase Bellabeat's future growth/revenue.
#     Author  : Manoj Menon
#     Version : 01
#     Updated : 03/12/2022
#       Moved the project from RStudio Cloud to local Visual Studio Code environment
#       Removed un-necessary files (hourly and minute records) as they were deemed not useful for this analysis
#       Step and Calories data is included in the dailyActivities file  - so removed the dailyStep and dailyCalories files from analysis

#   ------------------------------------------------------------------------------------------------------------


# Preparing the RStudio environment for the analysis
# Uploaded the zip file (raw source files) into RStudio folder structure

install.packages("tidyverse")
# install.packages("pacman")
library(tidyverse)

#------------------------------------------------------------------------------
# DATA PREPARE PHASE

#     Importing the source files by reading the csv into new dataframes
#       Reduced the number of imported files in this version 
#       (based on earlier version observations, some were not needed)

dailyActivity <- read_csv("/cloud/project/dailyActivity_merged.csv")
dailyCalories <- read_csv("/cloud/project/dailyCalories_merged.csv")
dailyIntensities <- read_csv("/cloud/project/dailyIntensities_merged.csv")
dailySteps <- read_csv("/cloud/project/dailySteps_merged.csv")
heartrate_seconds <- read_csv("/cloud/project/heartrate_seconds_merged.csv")
sleepDay <- read_csv("/cloud/project/sleepDay_merged.csv")
weightLogInfo <- read_csv("/cloud/project/weightLogInfo_merged.csv")


# Viewing the data to understand its structure and contents, focusing mainly on the 'daily' data files

View(dailyActivity)
unique(dailyActivity$Id)
View(dailyCalories)
unique(dailyCalories$Id)
View(dailyIntensities)
unique(dailyIntensities$Id)
View(dailySteps)
unique(dailySteps$Id)
View(sleepDay)
unique(sleepDay$Id)
View(heartrate_seconds)
unique(heartrate_seconds$Id)
View(weightLogInfo)
unique(weightLogInfo$Id)

#              Observations:
#                   There are 33 total participants
#                   Files can be categorized into 3 main types of time series - daily records, hourly records and records by minutes.
#                   All files and records are linked by a common Key field called "Id".
#                   The daily files contains data for all 33 participants (activity, calories, intensities, steps)
#                   By viewing the daily files, it can be seen that only about 1 month of data is available in this dataset (4/12 - 5/12)
#                   Sleep time information is available for 24 out of 33 participants
#                   Heart rate records is available for only 14 out of the 33 participants in this surveyed dataset
#                   Weight log information is available for only 8 out of 33 participants
#                   Some files are available in both long and wide format (e.g. minuteStepsNarrow and minuteStepsWide)
#                   MET = Metabolic Equivalent.  Need to understand what the MET numbers mean.
#                   The participants are identified by Id number.  There is no direct indication of the gender (male or female)
#             Conclusions :
#                   The data set is limited to only 33 individuals/participants. It is not clear how many of these are women. 
#                   So it is likely that the sample size is statistically insufficient to draw conclusions with a high level of confidence.

# The code used to arrive at this conclusion is below.

#--------------------------------------------------------------------------------------
#   DATA PROCESS PHASE

#       Checking the daily data for incorrect or missing data
#       Number of steps taken or calories burnt or Activity minutes or time slept cannot be zero in a day
#       Total daily activity should add up to 1440 minutes (60 x 24), otherwise it indicates data collection is incomplete

dailyActivity %>% 
  filter(!complete.cases(.)) %>% 
  filter(TotalSteps == 0) %>% 
  filter(Calories == 0) %>%
  mutate(TotalActivityMinutes = SedentaryMinutes + LightlyActiveMinutes + FairlyActiveMinutes + VeryActiveMinutes) %>% 
  filter(TotalActivityMinutes == 1440) %>%
  View()

#           Observations for dailyActivity records :
#             0 observations with NA or missing data
#             77 observations have TotalSteps = 0
#             4 observations have Calories = 0
#             462 observations have TotalActivityMinutes that do not add up to 1440mins


#   Clean up of dailyActivity - removing 0 value records and formatting date
dailyActivityClean <-
dailyActivity %>% 
  mutate(TotalActivityMinutes = SedentaryMinutes + LightlyActiveMinutes + FairlyActiveMinutes + VeryActiveMinutes) %>% 
  filter(TotalSteps != 0) %>%
  filter(Calories != 0)
#filter(TotalActivityMinutes != 1440)    - decided to leave the incomplete records in, other wise data set is reduced in half

dailyActivityClean$ActivityDate = as.POSIXct(dailyActivityClean$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
dailyActivityClean$Date <- format(dailyActivityClean$ActivityDate, format = "%m/%d/%Y")


#.....................................

sleepDay %>%
  filter(!complete.cases(.)) %>% 
  filter(TotalMinutesAsleep == 0) %>%
  View()

#           Observations for sleepDay records :
#             0 observations with NA or missing data
#             0 observations have TotalMinutesAsleep = 0

#   Clean up of sleepDay records
#       No cleaning is required for sleepDay file except date format

sleepDay$SleepDay = as.POSIXct(sleepDay$SleepDay, format="%m/%d/%Y", tz=Sys.timezone())
sleepDay$Date <- format(sleepDay$SleepDay, format = "%m/%d/%Y")

View(sleepDay)
View(dailyActivityClean)

#  Combine the two data frames dailyActivitiesClean and sleepDay based on Id and Date

mergeData <- merge(dailyActivityClean, sleepDay, by = c("Id", "Date"))

#   Only keeping the columns needed for analysis
mergeDataSubset <- select(mergeData, Id, Date, TotalSteps, TotalDistance, TotalActivityMinutes, Calories, TotalMinutesAsleep, TotalTimeInBed, SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes)
View(mergeDataSubset)

#...........................

# dailyCalories file :   The data for Calories is included in the dailyActivities file
#                         This file is not used in this version of the program file

#.............................

# dailySteps file :   The data for Steps is included in the dailyActivities file
#                     This file is not used in this version of the program file
#...............................

# dailyIntensities file :   The data for Intensities is included in the dailyActivities file
#                           This file is not used in this version of the program file


#  -------------------------------------------------------------------------------------------
#   DATA ANALYZE PHASE
#     Plotting data as a whole and also by individuals to observe trends, co-relations

ggplot(mergeDataSubset, aes(TotalDistance, TotalSteps)) + 
  geom_point() + 
  geom_smooth() +
  labs(title = "Distance vs Steps", x = "Total Distance", y = "Total Steps")
ggsave("DistanceVsSteps.jpg", plot =last_plot(), width = 4)

ggplot(mergeDataSubset, aes(Calories,TotalSteps)) + 
  geom_point() + 
  geom_smooth() +
  labs(title = "Calories vs Total Steps", x = "Total Calories", y = "Total Steps")
ggsave("CaloriesVsSteps.jpg", plot =last_plot(), width = 4)

ggplot(mergeDataSubset, aes(TotalSteps, Calories)) + geom_point() + facet_wrap(~ Id)
#     This is an obvious relation : the more steps one takes the further he/she is likely to travel
#                                               and the more calories is likely to be burnt

ggplot(mergeDataSubset, aes(TotalActivityMinutes, Calories)) + 
  geom_point() + 
  geom_smooth() +
  labs(title = "Activity vs Calories burnt", x = "Total Activity Minutes", y = "Total Calories")
ggsave("ActivityVsCalories.jpg", plot =last_plot(), width = 4)

ggplot(mergeDataSubset, aes(TotalActivityMinutes, Calories)) + geom_point() + facet_wrap(~ Id)
#     It is obvious that more the active minutes more the calories burnt
#     However, within the 800 - 1200 minutes range there is a large variation in calories burnt
#     This suggests that the 'kind' of activity performed will help optimise the calories burnt



ggplot(mergeDataSubset, aes(TotalMinutesAsleep, TotalActivityMinutes)) + 
  geom_point() + 
  geom_smooth() +
  labs(title = "Sleeping time vs Activity time", x = "Total Minutes Asleep", y = "Total Activity minutes")
ggsave("SleepVsActivity.jpg", plot =last_plot(), width = 4)

ggplot(mergeDataSubset, aes(TotalMinutesAsleep, TotalActivityMinutes)) + geom_point() + facet_wrap(~ Id)
#   This is the most interesting chart as it shows Sleep time and Activity time inversely co-related
#   The point where sleep and activity are balanced is in the 400 to 500 hrs range
#   It is possible to identify individuals who are in this optimal sleep zone but with light activity

#  -------------------------------------------------------------------------------------------
#   DATA SHARE PHASE

#   KEY FINDINGS
#     The data shows that the main features used and tracked by the majority of the users are those that 
#     are automatically tracked as opposed to manually entered (like weight). 
#     A lot of missing data seems to suggest that the device used should be one that is the most convenient 
#     for daily use as well as while sleeping or doing special activities like swimming, cycling etc.
#     There are some obvious co-relations abserved in the above visualizations however, the data can be used
#     smartly to identify those individuals who need the extra nudge to change their behaviour.
#     The avaialble data is in-sufficient to make conclusions specifically for women since the gender is not 
#     identified in the dataset - statistical significance cannot be estimated for women to make specific recommendations.

#   GENERAL RECOMMENDATIONS
#     Focus on data collection of the automated kind that does not require manual input from user.
#     Sleep time is an important piece of data - make it easier to collect this information in order 
#     to recommend or 'nudge' users into optimising it.
#     'Nudge' the users to balance their sedentary hours with more active hours.
#     Optimise the time when these reminders are sent to the customers based on the time of day and 
#     monitored activity levels.

#   NEXT STEPS
#     Find additional data sources that can augment this data set, especially if gender is identified
#     This will help with better analysis for a customized recommendations for women.

#   Additional data sources at : https://datasets.simula.no/pmdata/
#   Analysis of Fitbit Data: - Zenodo https://zenodo.org
