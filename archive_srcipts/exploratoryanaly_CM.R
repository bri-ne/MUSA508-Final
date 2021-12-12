# Libraries -----------------
library(tidyr)
library(dplyr)
library(lubridate)
library(sf)
library(tidyverse)
library(tidycensus)

# Data ----------------------
Bring in data, wrangle using lubridate 
Bring in other data sources and join in legible ways that will make data useable 
Do exploratory analysis to understand which variables will be useful features
Build features
Parameterizing lines: i.e. what line is it on 
Spatial fixed effect, characteristics of stations or so heavily co-linear
How central something is 
Particular lines have their own issues 
Use case: how do you set up your model—a particular train, a particular time, particular place 
Level of specificity the use case demands 
Scheduled arrival time is a line → 
Process modeling and validation 
Split data: train on 2 months, test on 2 weeks
Create regression that will predict # of minutes late a train will be with information available 2 hours ahead of time  
