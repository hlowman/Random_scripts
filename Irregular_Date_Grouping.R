# The following code was initially created by Owen Liu (UCSB) and edited before being sent to Jacob Kaplan (CA State Waterboards).
# This code is intended to be used to group a dataset with samplings performed at irregular intervals.
# Samples taken within 4 days of one another are intended to be grouped together.
# At the close of that window, the next window begins at the next earliest sampling date, and so on.
# Windows may have 1-4 sample dates in them, but the goal is then to report the aggregated sample value based on the assigned window.

library(tidyverse)
library(lubridate)

# make a pseudo-random sequence of dates (so that there's some irregularity in spacing of observations)
seq_dates <- ymd("2019-01-01")+days(cumsum(sample(1:10,size = 100,replace=T)))# toy data with the random dates
df <- tibble(observation=1:100,date=seq_dates)

# for a given date, find dates within +/- 4 days
calc_within_4days <- function(current_date,seq_dates){
  diffs <- time_length(current_date %--% seq_dates,"days")
  seq_dates[diffs>=-4&diffs<=4]
}
# produce the grouping index

# first, use the above function to produce a list of dates within +/- 4 days of each date
df_nest <- df %>% mutate(close_dates=purrr::map(date,calc_within_4days,seq_dates=df$date)) %>%
  # then, expand (unnest) the data
  unnest(close_dates) %>%
  # only keep the FIRST (i.e., earliest in time) group that an observation falls into
  group_by(close_dates) %>% 
  summarise(group=min(observation)) %>% 
  # finally, collapse the periods so they have no gaps (i.e. so it goes from group 2 to group 3 instead of group 2 to group 4 with no group 3)
  ungroup() %>% 
  mutate(group=dense_rank(group))
