# The following code was initially created by Owen Liu (UCSB) and edited before being sent to Jacob Kaplan (CA State Waterboards).
# This code is intended to be used to group a dataset with samplings performed at irregular intervals.
# Samples taken within 4 days of one another are intended to be grouped together.
# At the close of that window, the next window begins at the next earliest sampling date, and so on.
# Windows may have 1-4 sample dates in them, but the goal is then to report the aggregated sample value based on the assigned window.

library(tidyverse)
library(lubridate)

# make a pseudo-random sequence of dates (so that there's some irregularity in spacing of observations)
seq_dates <- ymd("2019-01-01")+days(cumsum(sample(1:10,size = 100,replace=T)))# toy data with the random dates
df <- tibble(observation=1:100,date=seq_dates, extra=1001:1100)

# for a given date, find dates within +/- 4 days
calc_within_4days <- function(current_date,seq_dates){
  diffs <- time_length(current_date %--% seq_dates,"days")
  seq_dates[diffs>=-4&diffs<=4]
}
# produce the grouping index

# first, use the above function to produce a list of dates within +/- 4 days of each date
df_nest <- df %>%
  mutate(close_dates=purrr::map(date,calc_within_4days,seq_dates=df$date)) %>%
  # then, expand (unnest) the data
  unnest(close_dates)

df_nest2 <- df_nest %>%
  # only keep the FIRST (i.e., earliest in time) group that an observation falls into
  group_by(close_dates) %>% 
  summarise(group=min(observation)) %>% 
  # finally, collapse the periods so they have no gaps (i.e. so it goes from group 2 to group 3 instead of group 2 to group 4 with no group 3)
  ungroup() %>% 
  mutate(group2=dense_rank(group))

#### Additional brainstorming below ####

# Create a function from the above commands:

# first, use the above function to produce a list of dates within +/- 4 days of each date
lines_evidence <- function(df){
  # for a given date, find dates within +/- 4 days
  calc_within_4days <- function(current_date,seq_dates){
    diffs <- time_length(current_date %--% seq_dates,"days")
    seq_dates[diffs>=-4&diffs<=4]
  }
  # produce the grouping index
  df %>%
    # (1) Group by: Waterbody, WBID, pollutant, matrix, fraction, unit, Station Code, beneficial use, objective language, objective_ref, evaluation guideline, eval guideline_ref
    mutate(close_dates=purrr::map(DATE,calc_within_4days,seq_dates=df$DATE)) %>%
    # then, expand (unnest) the data
    unnest(close_dates) %>%
    # only keep the FIRST group that an observation falls into
    group_by(close_dates) %>% 

    summarise(group=min(observation)) %>% 
    # finally, collapse the periods so they have no gaps 
    ungroup() %>% 
    mutate(group2=dense_rank(group))  
}

# Import the dummy dataset I've created.

dummy <- read_csv("sample_df_wide.csv")

dummy1 <- dummy[1:14,]

dummy2 <- dummy1 %>%
  mutate(waterbody = as.factor(Waterbody)) %>%
  mutate(matrix = as.factor(Matrix)) %>%
  mutate(analyte = as.factor(Analyte)) %>%
  mutate(DATE = mdy(date))

NEW <- dummy2 %>%
  lines_evidence() %>%
  # (2) For each group apply the averaging period logic
  ungroup() %>%
  summarise(MEAN = mean(Concentration))

# (3) Based on each of the averaging period groups within each of the grouping variables, apply one or more summarizing functions.  For example, average the result, average the objective, etc.

# Note: The important parts of this are that we carry all of the grouping variables forward into our summarized data frame, and that the averaging period grouping is applied to each group of variables, not to the data frame as a whole.