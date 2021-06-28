# Gap-Filling Daily Temperature data based on weekly measurements
# Ana Miller-ter Kuile
# June 28, 2021

# this code was developed as part of my summer EDI data science fellowship,
# in which I wanted to convert a few weekly measurement values for min and
# max temperature to daily temperatures min/max values for the prior week

# I did this via a method known as "gap-filling" in which a weekly value is 
# assumed to correspond to the value for days prior to the day in which it 
# was measured - which is accurate in the case where a weekly min/max temp
# are recorded per week.

# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

# Load dataset ------------------------------------------------------------

# This dataset consists of a sample of the dataset from Manitou Experimental
# forest for weekly maximum temperatures in the 1960s

data <- read.csv(here("gap_filling_sample_data.csv"))

# In this dataset, "." corresponds to no value collected for that day, and every
# temperature measurement corresponds to a maximum value for the *previous* week

# We made one assumption about these data - and that is that if there is ever
# a gap of *more than* one week, we no longer want to gap-fill the days in which
# measurement values were not collected. This means we assumed that whoever was 
# managing the temperature gauge was resetting it every week. 

# Gap-filling process -----------------------------------------------------

# First, I created a dataset that corresponded to *only* the days in which 
# a temperature value was recorded so that I could give each week an "identifier"

data_comp <- data %>%
  filter(Temp_max != ".") %>% #remove all the days in which no data were taken
  mutate(group = row_number()) %>% #create a sequential group sequence based on row number
  dplyr::select(Date, group) #select the date and group to then merge with the full datset

# After I had created the dataset of just complete cases of maximum temperature,
# I merged it back with the full dataset of all days and then made each date in
# between each temperature measurement part of that week's "group" 
data <- data %>%
  left_join(data_comp, by = "Date") %>% #add the complete case dataset with group names
  fill(group, .direction = "up") #fil group names for dates with no measurement for the previous week

# Finally, I converted temperature to a numeric and replaced the "." with NA values,
# grouped the data by the group ID column, created a new column of "new date", which
# became the date in that group with a complete case for temperature, then
# calculated the difference between the date of the column and the last date with
# a measurement, and finally removed the columns for which that date difference
# exceeded the one-week cutoff. I then removed all the interim columns
data <- data %>% 
  mutate(Temp_max = case_when(Temp_max == "." ~ "NA",
                              TRUE ~ Temp_max)) %>% #convert NA values
  mutate(Temp_max = as.numeric(Temp_max)) %>% #make temp a numeric value
  group_by(group) %>%  #group the data by the weekly grouping
  mutate(new_date = max(Date[!is.na(Temp_max)]), #determine the last date with a temp measurement in taht group
         diff1 = as.numeric(difftime(Date, new_date)/(24*3600))) %>% # calculate the difference between the current date and the last measurement in that group
  mutate(Temp_max_f = Temp_max) %>% #create a gap-filled column 
  fill(Temp_max_f, .direction = "up") %>% # fill the gap-filled maximum temperature column upward 
  mutate(Temp_max_f = replace(Temp_max_f, which(diff1 < -6), NA))%>% #remove values that exceed the one-week cutoff
  dplyr::select(Date, Temp_max, Temp_max_f) #select only columns of interest
             
             