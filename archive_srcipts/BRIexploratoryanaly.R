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


## old code for reference ----------------------------------------------------



# Previous Code Cleaning & Break Up --- OLD SETUP CODE

## Creating panels

```{r trainpanel,  warning=FALSE, message=FALSE, include=FALSE, echo=TRUE, results='hide'}


train.panel <- 
  train.template %>%
  right_join(study.panel, on=intervalhour) %>% 
  group_by(intervalhour, from, line)



### Breaking this up to curb unecessary processing on a bunch of NAS

train.panel <- train.panel%>%drop_na()

train.panel <- train.panel%>%summarize(late = mean(delay_dt, na.rm=T)) %>%
  mutate(daynumeric= wday(intervalhour),
         late_catagorical = ifelse(late < minutes(1), "ontime", ifelse(late < minutes(4), "few_minutes", "late")))

#leave this for potential use as a cat. 


## NEW Train Panel

### Making all new NAs ZERO ~~~~
NEWstudypanel[c("lagHour", "lag2Hours", "lag3Hours", "lag4Hours", "lag12Hours","lag1day")][is.na(NEWstudypanel[c("lagHour", "lag2Hours", "lag3Hours", "lag4Hours", "lag12Hours","lag1day")])] <- 0 



NEWtrainpanel <- NEWstudypanel%>%mutate(daynumeric= wday(intervalhour),
                                        week = week(intervalhour),
                                        weekday = weekdays(intervalhour),
                                        late_catagorical = ifelse(late < minutes(1), "ontime", ifelse(late < minutes(4), "few_minutes", "late")),)


```


```{r studypanel,  warning=FALSE, message=FALSE, include=FALSE, echo=TRUE, results='hide'}

## 
study.panel <- 
  expand.grid(intervalhour = unique(train.template$intervalhour), 
              from = unique(train.template$from),
              stopseq=unique(train.template$stop_sequence))
#line= unique(train.template$line))

## NEW Study Panel: Adding column for line and Rbind this mfs 
## if we want them combined 

NEWstudypanel<- rbind((gladInterval%>%mutate(line="Gladstone Branch")), (bergInterval%>%mutate(line="Bergen Co. Line")))

```





## OLD Regressions
### Quick Regression

NEWweather.panel <- merge(x= NEWstudypanel, y= weather.Panel, by= 'intervalhour', all.x= T)


quickreg<-  NEWweather.panel%>%dplyr::select(late, Percipitation)
reg <- lm(late~ Percipitation, data=quickreg)

quickreg$predvals <- fitted(reg)


quick.plot<- ggplot(data=quickreg, aes(x=late, y=predvals)) +
  geom_point() 


```{r traintest}
#first making a y-numeric for 1 late over 2 mins, and 0 for not late over 2mins

#Split data: train on 2 months, test on 2 weeks
## Partition --- changed this so that it is trainning on first 8 weeks, testing on last 2
set.seed(2121)
dataTrain <- filter(train.weather.panel, week < 9)
dataTest <- filter(train.weather.panel, week >= 9)


```

```{r model}
## First REG ## DID NOT WORK, "algorith did not converge, fitted probs numerically 0 or 1 occured"

#Model <- glm(y_numeric ~ .,
#                       data=dataTrain,     #### ALL VARS
#                       family="binomial" (link="logit"))

#It makes sense that these ones would not work since the number of seconds it is late will correspond perfectly with whether or not it is late...

Model1 <- glm(y_numeric ~ .,
              data=dataTrain%>% 
                dplyr::select(-late, -late_catagorical, -lagHour),     #### need to remove lagHour: otherwise we are making predictions based on information not available two hours out GETTING RID OF ONES THAT THROW ERRORS
              family="binomial" (link="logit"))

Model2<- glm(y_numeric ~ .,
             data=dataTrain %>% 
               dplyr::select(y_numeric, #late,
                             from,
                             lagHour, lag2Hours, lag3Hours, Percipitation, line), #### SOME VARS NEED INTERVAL HOUR THOugh
             family="binomial" (link="logit"))

Model3<- glm(y_numeric ~ .,
             data=dataTrain %>% 
               dplyr::select(y_numeric,
                             from, 
                             intervalhour,
                             lagHour, lag2Hours, lag3Hours), 
             family="binomial" (link="logit"))


Model4<- glm(y2_numeric ~ .,
             data=dataTrain %>% 
               dplyr::select(y2_numeric,
                             from, 
                             intervalhour,
                             lag1day, lag2Hours, lag3Hours,
                             lag12Hoursrain, lag12Hourswind, lag12Hourstemp,lag2Hoursrain, lag2Hourswind, lag2Hourstemp, line), 
             family="binomial" (link="logit"))

Model5<- glm(y5_numeric ~ .,
             data=dataTrain %>% 
               dplyr::select(y5_numeric,
                             from, 
                             intervalhour,
                             lag1day, lag2Hours, lag3Hours,
                             lag12Hoursrain, lag12Hourswind, lag12Hourstemp,lag2Hoursrain, lag2Hourswind, lag2Hourstemp, line), 
             family="binomial" (link="logit"))
```

```{r summarytable}
## after trying with the late param in model 2 i got the same error
#summary(Model)

summary1.table <- tbl_regression(Model1, exponentiate = TRUE)

summary2.table <- tbl_regression(Model2, exponentiate = TRUE) #do not use, need interval hour

summary3.table <- tbl_regression(Model3, exponentiate = TRUE)

summary4.table <- tbl_regression(Model4, exponentiate = TRUE)
summary5.table <- tbl_regression(Model5, exponentiate = TRUE)
#PROBS
```



```{r Testprobs}
## TEST PROBS **MODEL 3 --- 50% THRESH**
testProbs3 <- data.frame(Outcome = as.factor(dataTest$y_numeric),
                         Probs = predict(Model3, dataTest, type="response"))

testProbs3 <- 
  testProbs3 %>%
  mutate(predOutcome  = as.factor(ifelse(testProbs3$Probs > 0.50 , 1, 0)))

## 4

testProbs4 <- data.frame(
  Outcome = as.factor(dataTest$y2_numeric),
  Probs = predict(Model4, dataTest, type="response"))

testProbs4 <- 
  testProbs4 %>%
  mutate(predOutcome  = as.factor(ifelse(testProbs4$Probs > 0.57 , 1, 0)))

## 5 
testProbs5 <- data.frame(Outcome = as.factor(dataTest$y5_numeric),
                         Probs = predict(Model5, dataTest, type="response"))

testProbs5 <- 
  testProbs5 %>%
  mutate(predOutcome  = as.factor(ifelse(testProbs5$Probs > 0.50 , 1, 0)))



```


```{r roc}


## iterate THRESH
testProbs4.thresholds <- 
  iterateThresholds(data=testProbs4, observedClass = Outcome, 
                    predictedProbs = Probs)

testProbs4.thresholds%>%arrange(Rate_TP,Rate_TN) ## 0.57 identified

aucTable <- 
  testProbs4 %>% mutate(AUC = auc(Outcome,Probs)) %>% 
  mutate(AUC = as.character(round(AUC, 3))) 



```



```{r distribution}

## TEST PROBS PLOT **MODEL 3 --- 50% THRESH**

three<- ggplot(testProbs3, aes(x = Probs, fill = as.factor(Outcome))) + 
  geom_density() +
  facet_grid(Outcome ~ .) +
  scale_fill_manual(values = palette2) +
  labs(x = "Late Over 2 Mins", y = "Density of probabilities",
       title = "Third Model: Distribution of Predicted Probabilities by Observed Outcome",
       caption= "Fig.6") +
  theme(strip.text.x = element_text(size = 18),
        legend.position = "none")


## TEST PROBS PLOT **MODEL 4 --- 50% THRESH**

four<- ggplot(testProbs4, aes(x = Probs, fill = as.factor(Outcome))) + 
  geom_density() +
  facet_grid(Outcome ~ .) +
  scale_fill_manual(values = palette2) +
  labs(x = "Late Over 2 Mins", y = "Density of probabilities",
       title = "Fourth Model: Distribution of Predicted Probabilities by Observed Outcome",
       caption= "Fig.6") +
  theme(strip.text.x = element_text(size = 18),
        legend.position = "none")+plotTheme2()


## TEST PROBS PLOT **MODEL 5 --- 50% THRESH**

five <- ggplot(testProbs5, aes(x = Probs, fill = as.factor(Outcome))) + 
  geom_density() +
  facet_grid(Outcome ~ .) +
  scale_fill_manual(values = palette2) +
  labs(x = "Late Over 5 Mins", y = "Density of probabilities",
       title = "Five Model: Distribution of Predicted Probabilities by Observed Outcome",
       caption= "Fig.6") +
  theme(strip.text.x = element_text(size = 18),
        legend.position = "none")


plot_grid(three, four, five, ncol = 1, nrow=3)
```


```{r validation}


## Cross Validation 

ctrl <- trainControl(method = "cv", number = 100, classProbs=TRUE, summaryFunction=twoClassSummary)

#### Model 3 ----


#define the variables we want
Model3.vars <- colnames(train.weather.panel%>%dplyr::select(
  from, 
  intervalhour,
  lagHour, lag2Hours, lag3Hours))


cvFit3 <- caret::train(y2 ~ .,
                       data=train.weather.panel %>%dplyr::select(all_of(Model3.vars), y2), 
                       method="glm", family="binomial",
                       metric="ROC", trControl = ctrl)

cvFit3.plot<- dplyr::select(cvFit3$resample, -Resample) %>%
  gather(metric, value) %>%
  left_join(gather(cvFit3$results[2:4], metric, mean)) %>%
  ggplot(aes(value)) + 
  geom_histogram(bins=35, fill = "#93afdc") +
  facet_wrap(~metric) +
  geom_vline(aes(xintercept = mean), colour = "#a81010", linetype = 3, size = 1.5) +
  scale_x_continuous(limits = c(0, 1)) +
  labs(x="Goodness of Fit", y="Count", title="MODEL 3 Goodness of Fit Metrics",
       subtitle = "Across-fold mean reprented as dotted lines",
       caption= "Fig.10") +
  plotTheme2()


("#a81010", "#f8cd12", "#93afdc")
#### Model 4 ----


#define the variables we want
Model4.vars <- colnames(dataTest%>%dplyr::select(
  from, 
  intervalhour,
  lag1day, lag2Hours, lag3Hours,
  lag12Hoursrain, lag12Hourswind, lag12Hourstemp,lag2Hoursrain, lag2Hourswind, lag2Hourstemp, line))


cvFit4 <- caret::train(y2 ~ .,
                       data=dataTest %>%dplyr::select(all_of(Model4.vars), y2), 
                       method="glm", family="binomial",
                       metric="ROC", trControl = ctrl)


cvFit4.plot <-dplyr::select(cvFit4$resample, -Resample) %>%
  gather(metric, value) %>%
  left_join(gather(cvFit4$results[2:4], metric, mean)) %>%
  ggplot(aes(value)) + 
  geom_histogram(bins=35, fill = "#93afdc") +
  facet_wrap(~metric) +
  geom_vline(aes(xintercept = mean), colour = "#a81010", linetype = 3, size = 1.5) +
  scale_x_continuous(limits = c(0, 1)) +
  labs(x="Goodness of Fit", y="Count", title="MODEL 4 Goodness of Fit Metrics",
       subtitle = "Across-fold mean reprented as dotted lines",
       caption= "Fig.10") +
  plotTheme2()



#### Model 5 ----


#define the variables we want
Model5.vars <- colnames(train.weather.panel%>%dplyr::select(
  from, 
  intervalhour,
  lag1day, lag2Hours, lag3Hours,
  lag12Hoursrain, lag12Hourswind, lag12Hourstemp,lag2Hoursrain, lag2Hourswind, lag2Hourstemp, line))


cvFit5 <- caret::train(y5 ~ .,
                       data=train.weather.panel %>%dplyr::select(all_of(Model5.vars), y5), 
                       method="glm", family="binomial",
                       metric="ROC", trControl = ctrl)

cvFit5.plot<- dplyr::select(cvFit5$resample, -Resample) %>%
  gather(metric, value) %>%
  left_join(gather(cvFit5$results[2:4], metric, mean)) %>%
  ggplot(aes(value)) + 
  geom_histogram(bins=35, fill = "#FFB000") +
  facet_wrap(~metric) +
  geom_vline(aes(xintercept = mean), colour = "#648FFF", linetype = 3, size = 1.5) +
  scale_x_continuous(limits = c(0, 1)) +
  labs(x="Goodness of Fit", y="Count", title="MODEL 5 Goodness of Fit Metrics",
       subtitle = "Across-fold mean reprented as dotted lines",
       caption= "Fig.10") +
  plotTheme2()


plot_grid(cvFit3.plot, cvFit4.plot, cvFit5.plot, nrow=3)




```

## VISUALS 

##### LAG PLOTS -----




plotData.lag <-train.weather.panel%>%
  filter(week == 2) %>%
  dplyr::select(contains("rain"), late) %>%
  gather(Variable, Value, -late) %>%
  mutate(Variable = fct_relevel(Variable, "lag2Hoursrain","lag3Hoursrain",
                                "lag4Hoursrain","lag12Hoursrain","lag1dayrain"))
correlation.lag <-
  group_by(plotData.lag, Variable) %>%
  summarize(correlation = round(cor(Value, late, use = "complete.obs"), 2)) 





LAGplot <-ggplot(plotData.lag, aes(Value,late)) +
  geom_point(size = 0.1) +
  geom_text(data = correlation.lag, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1.5, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "#a81010") +
  facet_wrap(~Variable, ncol = 2, scales = "free") +
  labs(title = "Train Delays as a Function of RainLag Delays",
       subtitle = "One Week in 2019") +
  plotTheme2()


library(cowplot)
two <- train.weather.panel %>%
  group_by(y2_numeric) %>% 
  summarize(Percipitation = mean(lag4Hourswind)) %>%
  ggplot(aes(y2_numeric, Percipitation)) + geom_bar(stat = "identity", color = palette2, fill= palette2) +
  labs(title="Does lateness vary with temperature two hours ahead?",
       x="y2_numeric", y="Mean Temp 2 Hours Before Scheduled Departure") +
  plotTheme() 


five <- train.weather.panel %>%
  group_by(y5_numeric) %>% 
  summarize(Percipitation = mean(lag4Hourswind)) %>%
  ggplot(aes(y5_numeric, Percipitation)) + geom_bar(stat = "identity", color = palette2, fill= palette2) +
  labs(title="Does lateness vary with temperature two hours ahead?",
       x="y5_numeric", y="Mean Temp 2 Hours Before Scheduled Departure") +
  plotTheme() 

plot_grid(two, five)



train.weather.panel %>%
  group_by(line) %>% 
  summarize(lateness = mean(late)) %>%
  ggplot(aes(line, lateness)) + geom_bar(stat = "identity", color = palette2, fill= palette2) +
  labs(title="Does Lateness Vary Depending on the Line?",
       x="Line", y="Mean Seconds Late", color = "#0C0E1D") +
  plotTheme2() 
train.weather.panel %>%mutate(weekday = fct_reorder(weekday, day))%>%
  group_by(weekday) %>% 
  summarize(lateness = mean(late)) %>%
  ggplot(aes(weekday, lateness)) + geom_bar(stat = "identity", color = "#0c0e1d", fill= "#0c0e1d") + 
  labs(title="Does Lateness Vary With the Day of the Week?",
       x="day of the week", y="Mean seconds late") +
  plotTheme2() 




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

#f2fdff
origin <- train.weather.panel %>%
  group_by(from) %>% 
  summarize(lateness = mean(late)) %>% mutate(from = fct_reorder(from, lateness)) %>%
  ggplot(aes(lateness, from)) + geom_bar(stat = "identity",  color = NA, fill= "#0c0e1d") +
  labs(title="Does Lateness Vary by Stop Origin",
       x="Mean seconds late", y="Origin Stop") +
  scale_y_discrete(expand = expansion(mult = c(.02, .02)))+
  scale_x_continuous(expand = expansion(mult = c(0, 0.02)))+
  plotTheme2()
```34023 	Middlesex
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



#  DO NOT NEED Tibble, but keeping just in case 

No weather yet.

## Making First Tibble
-   Just doing this for one train first?
  -   get only regression variables
-   divide train and test 
-   make tibble
-   run regression



#### Making it a tibble -----
gladTibble <- nest(as.tibble(gladModel), cols= -train_id)

gladTrainTibble <- nest(as.tibble(gladTrain), cols= -train_id)
gladTestTibble <- nest(as.tibble(gladTest), cols= -train_id)
unnest(gladTrainTibble)


#### and then adding the lag ----
unnest(unnest(gladTibble))

gladTibble <- gladTibble%>%
  mutate(onestop_lag = map(.x=gladTibble, .f = dplyr::lag(late, 1)))
#,
twostop_lag = map(.x = data, fit = reg2, .f = model_pred),
threestop_lag = map(.x = data, fit = reg3, .f = model_pred))