# Libraries -----------------
library(tidyr)
library(dplyr)
library(lubridate)
library(sf)
library(tidyverse)
library(tidycensus)
library(cowplot) #for plotgrid
library(fastDummies)
library(spdep)
library(caret)
library(mapview)

options(scipen=999)
options(tigris_class = "sf")

root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

# is it late over 2 mins y/n 

# model put ins
#   time of day (at least two hours out)
#   line
#   station pickup 
# model outputs 
#   is the train going to be late over 2 mins. at pickup station


# lines we'll be working with 
#


# Graphics - deliverables 
#   

## Census stuff that i have not incorporated ----
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






gladcounties<-c( 	
"Morris",
"Somerset",	
"Union", 	
"Bergen",
"Essex",
"Hudson")

gladcounties<-c("Mercer", 	
                "Middlesex",
                "Monmouth", 	
                "Morris",
                "Somerset",	
                "Sussex", 	
                "Union", 	
                "Warren",
                "Bergen",
                "Essex",
                "Hudson", 	
                "Hunterdon", 
                "Passaic")

gladnjtrim <- njGEO%>%
  filter(county %in% gladcounties)



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
  rename(TotalCommRail =B08006_011, 
         TotalWorkers=B08011_001,
         W12.5=B08011_002,
         W5.530=B08011_003,
         W530.6=B08011_004,
         W6.630=B08011_005,
         W630.7=B08011_006,
         W7.730=B08011_007,
         W730.8=B08011_008,
         W8.830=B08011_009,
         W830.9=B08011_010,
         W9.930=B08011_011,
         W930.10=B08011_012,
         W10.11=B08011_013,
         W11.12=B08011_014,
         W=B08011_015) %>%
  dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         pctLatino = ifelse(TotalPop >0, Latino / TotalPop, 0),
         year = "2018") %>%
  dplyr::select(-year, -Whites, -FemaleBachelors, -MaleBachelors, -TotalPoverty) 

## NJ TRANSIT DATA ----------

HHdata <- read.csv("https://raw.githubusercontent.com/bri-ne/MUSA508-Final/main/Transit_Survey/HH_Public_9-23-2013.csv?token=AVOS24IRLQXB4UFGE3WGXKDBWOHOU")


data <- read.csv("https://raw.githubusercontent.com/bri-ne/MUSA508-Final/main/data/2019_02.csv?token=AVOS24LDA4AR5QJ46IKK3D3BWOPNG")


## Explore ----
data["delay_minutes"][is.na(data["delay_minutes"])] <- 0 # <------ make NAs in delay_minutes 0
data["stop_sequence"][is.na(data["stop_sequence"])] <- 0 # <------ make NAs in delay_minutes 0

GroupedLine<- njtransitdf%>%dplyr::group_by(line)%>%summarise(LineAvgDelay = mean(delay_minutes), 
                                                       MedianStopSeq = median(stop_sequence),
                                                       MaxStopSeq = max(stop_sequence))
  
  ### left join 
data.feat <- merge(x=data,y=GroupedLine,by="line",all.x=TRUE) 


# think of variables:
#     - origin penn station or no?
#     - Cat what line? 
#     - NUM: groupby by train id, get avg. delay 
#     - Cat: middle stop? (stop sequence for line, my guess is that the  middle will be the most off time no idea why i think this)
#     - NUM: how many stops? or CAT: Long train? Y??N

  
## get col names
  boulder_subset_sf_cols <- data.frame(colnames((subset_boulder_sf)))

#### The Dummy Variable Fn I'm using will autocmatically change all char data types to dummys 

subset.feat <- data.feat[!duplicated(data.feat$line), ]
  
subset.feat <-subset.feat%>%dplyr::select(line, MaxStopSeq, LineAvgDelay, type)

sapply(subset.feat, class)

variablesofinterest <- fastDummies::dummy_cols(subset.feat) 

variablesofinterest <- variablesofinterest%>%dplyr::select(-line)
## Partion Code ----

inTrain <-   variablesofinterest%>%createDataPartition(
  y = paste(),
  p = .75, list = FALSE)


boulder.training <- roll_up_the_partition_please[inTrain,] 
boulder.test <- roll_up_the_partition_please[-inTrain,]  


## Linear Regression ----

reg.train <- lm( ~ ., data = boulder.training %>% 
                  dplyr::select(LNprice, 
                                all_other_basements,
                                IntWallDscr_Drywall,
                                Roof_no_description,
                                `designCodeDscr_1 Story - Ranch`,
                                `HeatingDscr_Radiant Floor`,
                                Ext_wall_stucco_strawbale_brick_blck,
                                `district_Boulder Valley School District RE-2`,
                                trail_nn3,
                                LNlag,
                                mainfloorSF,
                                year,
                                carStorageSF,
                                pctBachelors,
                                pollut_nn2,
                                Age,
                                MedHHInc
                  ))


cols <- data.frame(colnames(stoplocations)) 


## Predict 

boulder.test <-
  boulder.test %>%
  mutate(LNSalePrice.Predict = predict(reg.train, boulder.test),
         LNSalePrice.Error = LNSalePrice.Predict - LNprice,
         LNSalePrice.AbsError = abs(LNSalePrice.Predict - LNprice),
         LNSalePrice.APE = (abs(LNSalePrice.Predict - LNprice)) / LNSalePrice.Predict)

boulder.test <-
  boulder.test %>%
  mutate(SalePrice.Predict = exp(LNSalePrice.Predict),
         SalePrice.Error = SalePrice.Predict - dollarprice,
         SalePrice.AbsError = abs(SalePrice.Predict - dollarprice),
         SalePrice.APE = abs(SalePrice.Predict - dollarprice) / SalePrice.Predict)

mean(boulder.test$SalePrice.AbsError, na.rm = T) # 139587.7 

mean(boulder.test$SalePrice.APE, na.rm = T) #0.1719868


## SANDBOX ~~ free for all do not use ----


devtools::install_github("dkahle/ggmap")
install.packages('maps')
install.packages("mapview")
library(mapview)
mapview(basemaps = "CartoDB.Positron", stoplocations)
location <- "New Jersey"
location<- geocode(location)

mymap <- get_stamenmap()

stoplocations4326 <- stoplocations%>%st_transform(crs=4326)
ggmap(mymap)+
geom_point(aes(x = LONGITUDE, y = LATITUDE), data = stoplocations4326,  
           alpha = .5, color="darkred", size = 3)



ggplot()

mapview(nj)


nrow(study.panel)+nrow(train.template)  
nrow(train.panel) 


mapview(basemaps = "CartoDB.Positron", bergGEOM)
mapview(basemaps = "CartoDB.Positron", gladGEOM)
