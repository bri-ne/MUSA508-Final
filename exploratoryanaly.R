# Libraries -----------------
library(tidyr)
library(dplyr)
library(lubridate)
library(sf)
library(tidyverse)
library(tidycensus)

# Data ----------------------

# ACS VARS
# B08006_011 : Estimate!!Total:!!Public transportation (excluding taxicab):!!Long-distance train or commuter rail
# B08011_001 Estimate!!Total:  SEX OF WORKERS BY TIME OF DEPARTURE TO GO TO WORK 98
# B08011_002 Estimate!!Total:!!12:00 a.m. to 4:59 a.m. SEX OF WORKERS BY TIME OF DEPARTURE TO GO TO WORK99
# B08011_003 Estimate!!Total:!!5:00 a.m. to 5:29 a.m. SEX OF WORKERS BY TIME OF DEPARTURE TO GO TO WORK 100
# B08011_004 Estimate!!Total:!!5:30 a.m. to 5:59 a.m. SEX OF WORKERS BY TIME OF DEPARTURE TO GO TO WORK 101
# B08011_005 Estimate!!Total:!!6:00 a.m. to 6:29 a.m. SEX OF WORKERS BY TIME OF DEPARTURE TO GO TO WORK 102
# B08011_006 Estimate!!Total:!!6:30 a.m. to 6:59 a.m. SEX OF WORKERS BY TIME OF DEPARTURE TO GO TO WORK 103
# B08011_007 Estimate!!Total:!!7:00 a.m. to 7:29 a.m. SEX OF WORKERS BY TIME OF DEPARTURE TO GO TO WORK104
# B08011_008 Estimate!!Total:!!7:30 a.m. to 7:59 a.m. SEX OF WORKERS BY TIME OF DEPARTURE TO GO TO WORK 105
# B08011_009 Estimate!!Total:!!8:00 a.m. to 8:29 a.m. SEX OF WORKERS BY TIME OF DEPARTURE TO GO TO WORK 106
# B08011_010 Estimate!!Total:!!8:30 a.m. to 8:59 a.m. SEX OF WORKERS BY TIME OF DEPARTURE TO GO TO WORK 107
# B08011_011 Estimate!!Total:!!9:00 a.m. to 9:59 a.m. SEX OF WORKERS BY TIME OF DEPARTURE TO GO TO WORK 108
# B08011_012 Estimate!!Total:!!10:00 a.m. to 10:59 a.m. SEX OF WORKERS BY TIME OF DEPARTURE TO GO TO WORK 109
# B08011_013 Estimate!!Total:!!11:00 a.m. to 11:59 a.m. SEX OF WORKERS BY TIME OF DEPARTURE TO GO TO WORK 110
# B08011_014 Estimate!!Total:!!12:00 p.m. to 3:59 p.m. SEX OF WORKERS BY TIME OF DEPARTURE TO GO TO WORK 111
# B08011_015 Estimate!!Total:!!4:00 p.m. to 11:59 p.m. SEX OF WORKERS BY TIME OF DEPARTURE TO GO TO WORK


34021 	Mercer 	
34023 	Middlesex
34025 	Monmouth 	
34027 	Morris
34035 	Somerset 	
34037 	Sussex 	
34039 	Union 	
34041 	Warren
34003 	Bergen
34013 	Essex
34017 	Hudson 	
34019 	Hunterdon
34031 	Passaic

HHdata <- read.csv("https://raw.githubusercontent.com/bri-ne/MUSA508-Final/main/Transit_Survey/HH_Public_9-23-2013.csv?token=AVOS24IRLQXB4UFGE3WGXKDBWOHOU")

var <- [""]

census_api_key("791448772c9a051612b70516247f56b54176cfbf", overwrite = TRUE)
vars <- load_variables(2019, "acs5")

myvars = variables = c("B08006_011", 
                          "B08011_001",
                          "B08011_002",
                          "B08011_003",
                          "B08011_004",
                          "B08011_005",
                          "B08011_006",
                          "B08011_007",
                          "B08011_008",
                          "B08011_009",
                          "B08011_010",
                          "B08011_011",
                          "B08011_012",
                          "B08011_013",
                          "B08011_014",
                          "B08011_015")
mycounties = c(34003, 
               34021, 
               34023, 
               34025, 
               34027, 
               34035, 
               34037,
               34039,
               34041,
               34013, 
               34017,
               34019, 
               34031)


NJ_ACS19 <- 
  get_acs(geography = "tract", 
          variables = my_vars,
          year = 2019,
          survey = "acs5", 
          state=34, 
          county=mycounties, 
          geometry=T, 
          output="wide") %>%
  st_transform('ESRI:102254') %>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         FemaleBachelors = B15001_050E, 
         MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E,
         Latino = B03002_012E,
         PctnoMort = B25081_008E
         
         
        TotalCommRail =B08006_011, 
         TotalWorkers=B08011_001,
         =B08011_002,
         =B08011_003,
         =B08011_004,
         =B08011_005,
         =B08011_006,
         =B08011_007,
         =B08011_008,
         =B08011_009,
         =B08011_010,
         =B08011_011,
         =B08011_012,
         =B08011_013,
         =B08011_014,
         =B08011_015) %>%
  dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         pctLatino = ifelse(TotalPop >0, Latino / TotalPop, 0),
         year = "2018") %>%
  dplyr::select(-year, -Whites, -FemaleBachelors, -MaleBachelors, -TotalPoverty) 


