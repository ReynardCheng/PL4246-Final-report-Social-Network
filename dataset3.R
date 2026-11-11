# load libraries
library(tidyverse)

# load datasets
responses <- read.csv('class-responses.csv')
names <- read.csv('class-names.csv')

# option to filter the data: example - I only want to analyze acquaintances 

responses_1 <- responses |> filter(weight == 1)

# option to filter the data: example - I only want to analyze the data of students who are doing the HM version 

responses_2 <- responses |> 
  left_join(names, by = c('rater' = 'id')) |> # merge with demographic info
  filter(HM_or_not == 'HM') |> # filter based on the criteria you want 
  select(rater, selected, weight) # select back the original columns for network construction 

# option to filter the data: I only want to analyze female-female relationships  

responses_3 <- responses |> 
  left_join(names, by = c('rater' = 'id')) |> # merge with demographic info
  left_join(names, by = c('selected' = 'id')) |> # merge with demographic info
  filter(gender.x == 'Female', gender.y == 'Female') |> # filter based on the criteria you want 
  select(rater, selected, weight) # select back the original columns for network construction 
