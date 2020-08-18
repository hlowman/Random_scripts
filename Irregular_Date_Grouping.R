# The following code was initially created by Owen Liu (UCSB) and edited before being sent to Jacob Kaplan (CA State Waterboards).
# This code is intended to be used to group a dataset with samplings performed at irregular intervals.
# Samples taken within 4 days of one another are intended to be grouped together.
# At the close of that window, the next window begins at the next earliest sampling date, and so on.
# Windows may have 1-4 sample dates in them, but the goal is then to report the aggregated sample value based on the assigned window.

#### Owen's code Part 1 ####

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

# Import the dummy dataset I've created.

dummy <- read_csv("sample_df_wide.csv")

dummy1 <- dummy[1:14,]

dummy2 <- dummy1 %>%
  mutate(waterbody = as.factor(Waterbody)) %>%
  mutate(matrix = as.factor(Matrix)) %>%
  mutate(analyte = as.factor(Analyte)) %>%
  mutate(DATE = mdy(date))

# Create a function from the above commands:

# first, use the above function to produce a list of dates within +/- 4 days of each date
lines_evidence <- function(df){
  
  df %>% group_by(observation)
  # (1) Group by: Waterbody, WBID, pollutant, matrix, fraction, unit, Station Code, beneficial use, objective language, objective_ref, evaluation guideline, eval guideline_ref
  
  # for a given date, find dates within +/- 4 days
  calc_within_4days <- function(current_date,seq_dates){
    diffs <- time_length(current_date %--% seq_dates,"days")
    seq_dates[diffs>=-4&diffs<=4]
  }
  # produce the grouping index
  df %>%
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

NEW <- dummy2 %>%
  lines_evidence() 
  # (2) For each group apply the averaging period logic

  summarise(MEAN = mean(Concentration))

# (3) Based on each of the averaging period groups within each of the grouping variables, apply one or more summarizing functions.  For example, average the result, average the objective, etc.

# Note: The important parts of this are that we carry all of the grouping variables forward into our summarized data frame, and that the averaging period grouping is applied to each group of variables, not to the data frame as a whole.

#### Owen's code Part 2 ####

# make a pseudo-random sequence of dates (so that there's some irregularity in spacing of observations)
# Creates a set number of random values.
set.seed(1304) 

seq_dates2 <- ymd("2019-01-01")+days(cumsum(sample(1:10,size = 100,replace=T)))

# toy data with the random dates
df2 <- tibble(observation=1:100,date=seq_dates2)

# function takes a current date, and a vector of dates to match
# and finds any dates within the next four days
calc_4days2 <- function(d,date_vec){
  diffs <- time_length(d %--% date_vec,"days")
  date_vec[diffs<=4]
}

# need an iterator now, to build each piece of the group key at a time
groups_key <- tibble()
current_p <- 1
current_d <- df2$date[1]
current_date_vec <- sort(df2$date)
for(i in seq_along(df2$date)){
  #if we've already assigned all the dates, stop
  if(length(current_date_vec)==0) break
  # otherwise build a group of dates
  grp <- calc_4days2(current_d,current_date_vec)
  # add the group of dates to our key
  groups_key <- bind_rows(groups_key,tibble(date=grp,group=current_p))
  # remove dates in the group from the pool of possible dates
  current_date_vec <- setdiff(current_date_vec,grp) %>% as_date()
  # iterate our counters
  current_p <- current_p + 1
  current_d <- current_date_vec[1]
}
# can add the grouping key back to the original data
df2_with_key <- df2 %>% left_join(groups_key)

#### More brainstorming ####

View(dummy2)

# Create a new column to assign indices based on defined grouping of samples. We need to do this so that R recognizes sampling dates as unique in the code below.
dummy3 <- dummy2 %>%
  group_by(waterbody, matrix, analyte) %>%
  mutate(counter = cur_group_id()) %>%
  mutate(date = DATE)

# function takes a current date, and a vector of dates to match
# and finds any dates within the next four days
calc_4days2 <- function(d,date_vec){
  diffs <- time_length(d %--% date_vec,"days")
  date_vec[diffs<=4]
}

# need an iterator now, to build each piece of the group key at a time

for(i in seq_along(dummy3$counter)){
    groups_key2 <- tibble()
    current_p2 <- 1
    current_d2 <- dummy3$DATE[1]
    current_date_vec2 <- sort(dummy3$DATE)
  for(j in seq_along(dummy3$DATE)){
    
  #if we've already assigned all the dates, stop
    if(length(current_date_vec2)==0) 
      break
  # otherwise build a group of dates
    grp2 <- calc_4days2(current_d2,current_date_vec2)
  # add the group of dates to our key
    groups_key2 <- bind_rows(groups_key2,tibble(date=grp2,group=current_p2))
  # remove dates in the group from the pool of possible dates
    current_date_vec2 <- setdiff(current_date_vec2,grp2) %>% as_date()
  # iterate our counters
    current_p2 <- current_p2 + 1
    current_d2 <- current_date_vec2[1]
  }
  
}

# can add the grouping key back to the original data
dummy_with_key <- dummy3 %>% 
  left_join(groups_key2)

# Starting *somewhat* over and trying to use the map() function from the purrr package.

# First, I'm going to create a new function for use in the map() portion, using Owen's working code from above, which worked on a singular time series of dates (with no replicates).

create_date_groups <- function(LOE_data){
  groups_key <- tibble()
  current_p <- 1
  current_d <- LOE_data$DATE[1]
  current_date_vec <- sort(LOE_data$DATE)
  for(i in seq_along(LOE_data$DATE)){
    #if we've already assigned all the dates, stop
    if(length(current_date_vec)==0) break
    # otherwise build a group of dates
    grp <- calc_4days2(current_d,current_date_vec)
    # add the group of dates to our key
    groups_key <- bind_rows(groups_key,tibble(date=grp,group=current_p))
    # remove dates in the group from the pool of possible dates
    current_date_vec <- setdiff(current_date_vec,grp) %>% as_date()
    # iterate our counters
    current_p <- current_p + 1
    current_d <- current_date_vec[1]
  }
  # add data onto final dataset
  LOE_data %>% 
    left_join(groups_key2)
}

dummy3_with_key <- dummy3 %>% # Take the original dataframe
  split(.$counter) %>% # Splits by "unique" datasets within the larger dataset (matching site/matrix/analyte)
  map(create_date_groups) 

str(dummy3_with_key)
