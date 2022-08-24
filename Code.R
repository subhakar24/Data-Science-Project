#installing the pacakages required
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("lubridate")
library(lubridate)
install.packages("mice")
library(mice)
install.packages("corrplot")
library(corrplot)


#Installing the packages reuqired
install.packages("tidyverse")
library(tidyverse)
install.packages("corrplot")
library(corrplot)
install.packages("mice")
library(mice)
install.packages("cowplot")
library(cowplot)

#install.packages("VIM")
#library(VIM)

#importing csv file data into r 

covid_main <- read.csv("owid-covid-data-latest.csv",header=TRUE)

glimpse(head(covid_main))
str(covid_main)

View(covid_main)


#Now finding the missing values present the data set
sum(is.na(covid_main))

#percentage of missing values
missing_values <- lapply(covid_main, function(x) mean(is.na(x))*100)

View(missing_values)


#From the dataset selecting the colums which have a missing value percentage
#of 60 and below
new_covid_main=select(covid_main,iso_code,continent,location,date,total_cases,
                      new_cases,new_cases_smoothed,total_deaths,new_deaths,
                      new_deaths_smoothed,total_cases_per_million,
                      new_cases_per_million,new_cases_smoothed_per_million,
                      total_deaths_per_million,new_deaths_per_million,
                      new_deaths_smoothed_per_million,reproduction_rate,
                      new_tests,total_tests,total_tests_per_thousand,
                      new_tests_smoothed,new_tests_smoothed_per_thousand,
                      tests_per_case,tests_units,new_vaccinations_smoothed,
                      new_vaccinations_smoothed_per_million,
                      new_people_vaccinated_smoothed,
                      new_people_vaccinated_smoothed_per_hundred,
                      stringency_index,population,population_density,
                      median_age,aged_65_older,aged_70_older,gdp_per_capita,
                      extreme_poverty,cardiovasc_death_rate,diabetes_prevalence,
                      female_smokers,male_smokers,handwashing_facilities,
                      hospital_beds_per_thousand,life_expectancy,
                      human_development_index)

View(new_covid_main)

sum(is.na(new_covid_main))

# selecting the countries required & creating subsets
Uk_data <-filter(new_covid_main,location=="United Kingdom")
View(Uk_data)

#selecting the colums required for our analysis
new_Uk_data_1 =select(Uk_data,c(location,date,total_cases,total_deaths,
                                reproduction_rate,total_tests,stringency_index,
                                tests_per_case,))

View(new_Uk_data_1)

plot(new_Uk_data_1$total_cases)
#lets check the missing values in the newly created df now

new_Uk_data_1_missing <- lapply(new_Uk_data_1, function(x) mean(is.na(x))*100)

View(new_Uk_data_1_missing)


# total_deaths na values to 0
new_Uk_data_1$total_deaths[is.na(new_Uk_data_1$total_deaths)]=0
View(new_Uk_data_1)


#Now imputing the missing values using the mice package
md.pattern(new_Uk_data_1)

Uk_imp_data <- mice(new_Uk_data_1,m=5,method = "cart",seed=500)

summary(Uk_imp_data)
new_Uk_data_1 <- complete(Uk_imp_data,2)

View(new_Uk_data_1)

# we can observe that the date is in fact format and not date frmt
glimpse(new_Uk_data_1) 

#converting the date format from fact to date
new_Uk_data_1$date <- as.Date(new_Uk_data_1$date,format= "%Y-%m-%d")
glimpse(new_Uk_data_1)
View(new_Uk_data_1)

#Exploraty Data Analysis 

str(new_Uk_data_1)
#looking at what is in the start and end of the dataframe
head(new_Uk_data_1)
tail(new_Uk_data_1)


#plotting histograms to observe the frequencies  
par(mfrow=c(2,3))
hist(new_Uk_data_1$total_cases, col ="red",
     xlab = "Total number of cases",
     main = "Histogram of total cases of Uk data")

hist(new_Uk_data_1$total_deaths, breaks = 10,col ="green",
     xlab="Total number of deaths",
     main = "Histogram of total number of deaths")

hist(new_Uk_data_1$reproduction_rate,breaks = 10, col="yellow",
     xlab="The reproduction rate",
     main="Histogram of the reproduction rate")

hist(new_Uk_data_1$total_tests, col="brown",
     xlab = "The total number of tests",
     main = "Histogram of the total number of tests")

hist(new_Uk_data_1$stringency_index,col = "purple",
     xlab="The Stringency Index",
     main="Histogram of the Stringency Index")

hist(new_Uk_data_1$tests_per_case,breaks = 6,col = "blue",
     xlab="Tests per case",
     main = "Histogram of tests per case of Uk data")

par(mfrow=c(1,1))

#Checking for outliers using boxplots 
par(mfrow=c(2,3))
boxplot(new_Uk_data_1$total_cases,xlab="Total number of cases",col="red") #outliers
boxplot(new_Uk_data_1$total_deaths,xlab="Total number of deaths",col="green")
boxplot(new_Uk_data_1$reproduction_rate,xlab="Reproduction rate",col="yellow") #outliers
boxplot(new_Uk_data_1$total_tests,xlab="Total number of tests",col="pink")
boxplot(new_Uk_data_1$stringency_index,xlab="Stringency Index",col="blue")
boxplot(new_Uk_data_1$tests_per_case,xlab="Tests per each case",col="orange") # outliers

par(mfrow=c(1,1))

#dealing with outliers 

#removing outliers for total cases
boxplot(new_Uk_data_1$total_cases,plot = FALSE)$out

ol_tc <- boxplot(new_Uk_data_1$total_cases,plot = FALSE)$out

Uk_data_without_outliers<- new_Uk_data_1[-which(new_Uk_data_1$total_cases %in% ol_tc),]

boxplot(Uk_data_without_outliers$total_cases)

#Removing outliers from reproduction rate
boxplot(new_Uk_data_1$reproduction_rate,plot = FALSE)$out

ol_rp <- boxplot(new_Uk_data_1$reproduction_rate,plot = FALSE)$out

Uk_data_without_outliers <- new_Uk_data_1[-which(new_Uk_data_1$reproduction_rate %in% ol_rp),]

boxplot(Uk_data_without_outliers$reproduction_rate)

#Removng outliers from  tests per case
boxplot(new_Uk_data_1$tests_per_case,plot = FALSE)$out

ol_tpc <- boxplot(new_Uk_data_1$reproduction_rate,plot = FALSE)$out

Uk_data_without_outliers <- new_Uk_data_1[-which(new_Uk_data_1$reproduction_rate %in% ol_tpc),]

boxplot(Uk_data_without_outliers$tests_per_case)

# the df which is clear of outliers  
View(Uk_data_without_outliers) 
View(new_Uk_data_1)

#Plotting after removing the majority of outliers
par(mfrow=c(2,3))
boxplot(Uk_data_without_outliers$total_cases,xlab="Total number of cases",col="red") #outliers
boxplot(Uk_data_without_outliers$reproduction_rate,xlab="Reproduction rate",col="yellow") #outliers
boxplot(Uk_data_without_outliers$tests_per_case,xlab="Tests per each case",col="orange") 

par(mfrow=c(1,1))

#correlation 
cor(Uk_data_without_outliers$total_cases,x$stringency_index)
cor(Uk_data_without_outliers$total_deaths,x$stringency_index)
cor(Uk_data_without_outliers$reproduction_rate,x$stringency_index)
cor(Uk_data_without_outliers$total_tests,x$stringency_index)
cor(Uk_data_without_outliers$tests_per_case,x$stringency_index)

#Running the correlation analysis by creating a correaltion matrix  
Uk_correlation <- Uk_data_without_outliers %>%
  select(c("total_cases", "total_deaths","stringency_index","reproduction_rate",
           "total_tests","tests_per_case"))

Uk_crd <- cor(Uk_correlation)

View(Uk_crd)

#plotting the correaltion matrix by using corrplot 
corrplot::corrplot(Uk_crd)

#Aggregating date to convert it from daily to monthly 
Uk_data_without_outliers_2 <- Uk_data_without_outliers
#creating year/month column 
Uk_data_without_outliers_2 $year_month <- floor_date(Uk_data_without_outliers_2 $date,
                                                     "month")   

#Using the summarize function from dplyr package 
Uk_aggregate_date <- Uk_data_without_outliers_2  %>%      
  group_by(year_month) %>% 
  dplyr::summarize(total_cases = sum(total_cases),total_deaths=sum(total_deaths),
                   reproduction_rate=sum(reproduction_rate),total_tests=sum(total_tests),
                   stringency_index=mean(stringency_index),tests_per_case=sum(tests_per_case) ) %>% 
  as.data.frame()

#Aggregated data frame which has dates now month vise 
View(Uk_aggregate_date) 

View(Uk_data_without_outliers_2 ) # normal df without aggregating 


#After finding that a relationship exists, we visulize the impact of that relation by using
#ggplot and in a time peroid of 6 months per year

#plotting for the first 6 months of 2020

plot_1<- ggplot(Uk_aggregate_date,aes(x=year_month,y=stringency_index))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2020-06-01")))

plot_2 <- ggplot(Uk_aggregate_date,aes(x=year_month,y=total_cases))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2020-06-01")))+
  ylim(0,1.0e+7)

plot_3 <- ggplot(Uk_aggregate_date,aes(x=year_month,y=total_deaths))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2020-06-01")))+
  ylim(0,2.0e+6)

plot_4 <- ggplot(Uk_aggregate_date,aes(x=year_month,y=total_tests))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2020-06-01")))+
  ylim(0,2.0e+8)

plot_5 <- ggplot(Uk_aggregate_date,aes(x=year_month,y=tests_per_case))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2020-06-01")))+
  ylim(0,4000)

plot_6 <-ggplot(Uk_aggregate_date,aes(x=year_month,y=reproduction_rate))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2020-06-01")))

plot_grid(plot_1,plot_2,plot_3,plot_4,plot_5,plot_6,labels = "AUTO")

#Plotting for the next 6 months of 2020
p7 <- ggplot(Uk_aggregate_date,aes(x=year_month,y=stringency_index))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-07-01"),as.Date("2020-12-01")))

p8 <-  ggplot(Uk_aggregate_date,aes(x=year_month,y=total_cases))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-07-01"),as.Date("2020-12-01")))+
  ylim(0,7.0e+7)

p9 <-ggplot(Uk_aggregate_date,aes(x=year_month,y=total_deaths))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-07-01"),as.Date("2020-12-01")))+
  ylim(0,3.0e+6)

p10 <-ggplot(Uk_aggregate_date,aes(x=year_month,y=total_tests))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-07-01"),as.Date("2020-12-01")))+
  ylim(0,3.0e+9)

p11 <- ggplot(Uk_aggregate_date,aes(x=year_month,y=tests_per_case))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-07-01"),as.Date("2020-12-01")))+
  ylim(0,7000)

p12 <- ggplot(Uk_aggregate_date,aes(x=year_month,y=reproduction_rate))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-07-01"),as.Date("2020-12-01")))

plot_grid(p7,p8,p9,p10,p11,p12)

#plotting for the first 6 months of 2021
p13 <- ggplot(Uk_aggregate_date,aes(x=year_month,y=stringency_index))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-01-01"),as.Date("2021-06-01")))

#p14 <- 
ggplot(Uk_aggregate_date,aes(x=year_month,y=total_cases))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-01-01"),as.Date("2021-06-01")))+
  ylim(0,3.0e+8)

p15 <- ggplot(Uk_aggregate_date,aes(x=year_month,y=total_deaths))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-01-01"),as.Date("2021-06-01")))+
  ylim(0,4000000)

p16 <- ggplot(Uk_aggregate_date,aes(x=year_month,y=total_tests))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-01-01"),as.Date("2021-06-01")))+
  ylim(0,7.0e+9)

p17 <- ggplot(Uk_aggregate_date,aes(x=year_month,y=tests_per_case))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-01-01"),as.Date("2021-06-01")))+
  ylim(0,20000)

p18 <- ggplot(Uk_aggregate_date,aes(x=year_month,y=reproduction_rate))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-01-01"),as.Date("2021-06-01")))

plot_grid(p13,p14,p15,p16,p17,p18)


#plotting for the next  6 months of 2021
p19 <- ggplot(Uk_aggregate_date,aes(x=year_month,y=stringency_index))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-07-01"),as.Date("2021-12-01")))

p20 <- ggplot(Uk_aggregate_date,aes(x=year_month,y=total_cases))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-07-01"),as.Date("2021-12-01")))+
  ylim(0,4.0e+8)

p21 <- ggplot(Uk_aggregate_date,aes(x=year_month,y=total_deaths))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-07-01"),as.Date("2021-12-01")))+
  ylim(0,6000000)

p22 <- ggplot(Uk_aggregate_date,aes(x=year_month,y=total_tests))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-07-01"),as.Date("2021-12-01")))+
  ylim(0,3.0e+10)

p23 <- ggplot(Uk_aggregate_date,aes(x=year_month,y=tests_per_case))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-07-01"),as.Date("2021-12-01")))+
  ylim(0,2000)

p24 <- ggplot(Uk_aggregate_date,aes(x=year_month,y=reproduction_rate))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-07-01"),as.Date("2021-12-01")))

plot_grid(p19,p20,p21,p22,p23,p24)






#correlation tests ## -- original analysis conti
#individual tests
cor(new_Uk_data_1$total_cases,new_Uk_data_1$stringency_index)
cor(new_Uk_data_1$total_deaths,new_Uk_data_1$stringency_index)
cor(new_Uk_data_1$reproduction_rate,new_Uk_data_1$stringency_index)
cor(new_Uk_data_1$total_tests,new_Uk_data_1$stringency_index)
cor(new_Uk_data_1$tests_per_case,new_Uk_data_1$stringency_index)

#grouped correaltion test
continous <- new_Uk_data_1 %>%
  select(c("total_cases", "total_deaths","stringency_index","reproduction_rate",
                                        "total_tests","tests_per_case"))

Uk_crd <- cor(continous)

View(Uk_crd)

corrplot::corrplot(Uk_crd) # plotting the correlation 


#Aggrigating daily data to months and year to be used in the viz
y <- new_Uk_data_1
y2$year_month <- floor_date(y2$date,"month") #creating year/month column 

y_aggr <- y2 %>%              # Aggregate data
  group_by(year_month) %>% 
  summarize(total_cases = sum(total_cases),total_deaths=sum(total_deaths),
                   reproduction_rate=sum(reproduction_rate),total_tests=sum(total_tests),
                   stringency_index=mean(stringency_index),tests_per_case=sum(tests_per_case) ) %>% 
  as.data.frame()

View(y_aggr) # aggregated df month vise 

View(new_Uk_data2) # normla df without aggregating 

#plotting for the first 6 months of 2020
ggplot(Uk_data_aggr,aes(x=year_month,y=stringency_index))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2020-06-01")))

ggplot(Uk_data_aggr,aes(x=year_month,y=total_cases))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2020-06-01")))+
  ylim(0,10000000)

ggplot(Uk_data_aggr,aes(x=year_month,y=total_deaths))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2020-06-01")))+
  ylim(0,4000000)

ggplot(Uk_data_aggr,aes(x=year_month,y=total_tests))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2020-06-01")))+
  ylim(0,40000000000)

ggplot(Uk_data_aggr,aes(x=year_month,y=tests_per_case))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2020-06-01")))+
  ylim(0,4000)

ggplot(Uk_data_aggr,aes(x=year_month,y=reproduction_rate))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2020-06-01")))

#plotting for the next 6 months of 2020

ggplot(Uk_data_aggr,aes(x=year_month,y=stringency_index))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-07-01"),as.Date("2020-12-01")))

ggplot(Uk_data_aggr,aes(x=year_month,y=total_cases))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-07-01"),as.Date("2020-12-01")))+
  ylim(0,40000000)

ggplot(Uk_data_aggr,aes(x=year_month,y=total_deaths))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-07-01"),as.Date("2020-12-01")))+
  ylim(0,4000000)

ggplot(Uk_data_aggr,aes(x=year_month,y=total_tests))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-07-01"),as.Date("2020-12-01")))+
  ylim(0,4000000000)

ggplot(Uk_data_aggr,aes(x=year_month,y=tests_per_case))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-07-01"),as.Date("2020-12-01")))+
  ylim(0,40000)

ggplot(Uk_data_aggr,aes(x=year_month,y=reproduction_rate))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-07-01"),as.Date("2020-12-01")))

#Plotting for the 1st 6 months in 2021
ggplot(Uk_data_aggr,aes(x=year_month,y=stringency_index))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-01-01"),as.Date("2021-06-01")))

ggplot(Uk_data_aggr,aes(x=year_month,y=total_cases))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-01-01"),as.Date("2021-06-01")))+
  ylim(0,400000000)

ggplot(Uk_data_aggr,aes(x=year_month,y=total_deaths))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-01-01"),as.Date("2021-06-01")))+
  ylim(0,4e+6)

ggplot(Uk_data_aggr,aes(x=year_month,y=total_tests))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-01-01"),as.Date("2021-06-01")))+
  ylim(0,10000000000)

ggplot(Uk_data_aggr,aes(x=year_month,y=tests_per_case))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-01-01"),as.Date("2021-06-01")))+
  ylim(0,40000)

ggplot(Uk_data_aggr,aes(x=year_month,y=reproduction_rate))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-01-01"),as.Date("2021-06-01")))

#plotting for the next 6 months of 2021
ggplot(Uk_data_aggr,aes(x=year_month,y=stringency_index))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-07-01"),as.Date("2021-12-01")))

ggplot(Uk_data_aggr,aes(x=year_month,y=total_cases))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-07-01"),as.Date("2021-12-01")))+
  ylim(0,500000000)

ggplot(Uk_data_aggr,aes(x=year_month,y=total_deaths))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-07-01"),as.Date("2021-12-01")))+
  ylim(0,5000000)

ggplot(Uk_data_aggr,aes(x=year_month,y=total_tests))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-07-01"),as.Date("2021-12-01")))+
  ylim(0,30000000000)

ggplot(Uk_data_aggr,aes(x=year_month,y=tests_per_case))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-07-01"),as.Date("2021-12-01")))+
  ylim(0,3000)

ggplot(Uk_data_aggr,aes(x=year_month,y=reproduction_rate))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-07-01"),as.Date("2021-12-01")))



### India ##
#creating India dataframe
India_data <-filter(new_covid_main,location=="India")
View(India_data)

#selecting the colums required 
new_India_data_1 =select(India_data,c(location,date,total_cases,total_deaths,
                                reproduction_rate,total_tests,stringency_index,
                                tests_per_case,))

View(new_India_data_1)

#Checking missing values percentage 
missing_values_ind <- lapply(new_India_data_1, function(x) mean(is.na(x))*100)

View(missing_values_ind)

#total_death columns na values to 0
new_India_data_1$total_deaths[is.na(new_India_data_1$total_deaths)]=0
View(new_India_data_1)


#Now imputing the missing values using the mice package
md.pattern(new_India_data_1)

India_imp_data <- mice(new_India_data_1,m=5,method = "cart",seed=500)

summary(India_imp_data)
new_India_data_1 <- complete(India_imp_data,2)

View(new_India_data_1)

#Checking the percentage of missing values now 

missing_values_ind <- lapply(new_India_data_1, function(x) mean(is.na(x))*100)

View(missing_values_ind)

glimpse(new_India_data_1) # date is in fact format 

#converting the date format from fact to date
new_India_data_1$date <- as.Date(new_India_data_1$date,format= "%Y-%m-%d")
glimpse(new_India_data_1)
View(new_India_data_1)

#EDA=
#Using str function to see the structure of the data frame
str(new_India_data_1)
#looking at what is in the dataframe
head(new_India_data_1)
tail(new_India_data_1)


#Plotting histograms to observe the frequeincies of the data 
par(mfrow=c(2,3))
hist(new_India_data_1$total_cases, col ="red",
     xlab = "Total number of cases",
     main = "Histogram of total cases of Uk data")

hist(new_India_data_1$total_deaths, breaks = 10,col ="green",
     xlab="Total number of deaths",
     main = "Histogram of total number of deaths")

hist(new_India_data_1$reproduction_rate,breaks = 10, col="yellow",
     xlab="The reproduction rate",
     main="Histogram of the reproduction rate")

hist(new_India_data_1$total_tests, col="brown",
     xlab = "The total number of tests",
     main = "Histogram of the total number of tests")

hist(new_India_data_1$stringency_index,col = "purple",
     xlab="The Stringency Index",
     main="Histogram of the Stringency Index")

hist(new_India_data_1$tests_per_case,breaks = 6,col = "blue",
     xlab="Tests per case",
     main = "Histogram of tests per case of Uk data")

par(mfrow=c(1,1))

#Checking for outliers using boxplot  
par(mfrow=c(2,3))
boxplot(new_India_data_1$total_cases,xlab="Total number of cases",col="red") 
boxplot(new_India_data_1$total_deaths,xlab="Total number of deaths",col="green")
boxplot(new_India_data_1$reproduction_rate,xlab="Reproduction rate",col="yellow") #outliers
boxplot(new_India_data_1$total_tests,xlab="Total number of tests",col="pink")
boxplot(new_India_data_1$stringency_index,xlab="Stringency Index",col="blue")
boxplot(new_India_data_1$tests_per_case,xlab="Tests per each case",col="orange") # outliers

par(mfrow=c(1,1))

#Handling the outliers for the 2 columns 
# For reproduction rate
boxplot(new_India_data_1$reproduction_rate,plot = FALSE)$out

ol_rp_i <- boxplot(new_India_data_1$reproduction_rate,plot = FALSE)$out

India_data_without_outliers <- new_India_data_1[-which(new_India_data_1$reproduction_rate %in% ol_rp_i),]

boxplot(India_data_without_outliers$reproduction_rate)

#For tests per case
boxplot(new_India_data_1$tests_per_case,plot = FALSE)$out

ol_tpc_i <- boxplot(new_India_data_1$reproduction_rate,plot = FALSE)$out

India_data_without_outliers <- new_India_data_1[-which(new_India_data_1$reproduction_rate %in% ol_tpc_i),]

boxplot(India_data_without_outliers$tests_per_case)

# the df which is clear of outliers , the main one now is this 
View(India_data_without_outliers) 
View(new_India_data_1)


#correlation Analysis by creating a matrix and then plotting it 
India_correlation <- India_data_without_outliers %>%
  select(c("total_cases", "total_deaths","stringency_index","reproduction_rate",
           "total_tests","tests_per_case"))


India_crd <- cor(India_correlation)

#Plotting the matrix by using corrplot
corrplot::corrplot(India_crd)

View(India_crd)

#Aggrigating daily data to months and year 
India_data_without_outliers_2 <- India_data_without_outliers
#creating year/month column 
India_data_without_outliers_2$year_month <- floor_date(India_data_without_outliers_2$date,"month") 

India_aggregate_date <- India_data_without_outliers_2 %>%          
  group_by(year_month) %>% 
  dplyr::summarize(total_cases = sum(total_cases),total_deaths=sum(total_deaths),
                   reproduction_rate=sum(reproduction_rate),total_tests=sum(total_tests),
                   stringency_index=mean(stringency_index),tests_per_case=sum(tests_per_case) ) %>% 
  as.data.frame()

#Aggregated data frame
View(India_aggregate_date) 
View(India_data_without_outliers_2)

#Same as we did for the UK data, after we find a relation exists , we plot to observe 
#its impact by using ggplot and with a time peroid of 6 months gap.
#plotting First 6 months of 2020
p1 <- ggplot(India_aggregate_date,aes(x=year_month,y=stringency_index))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2020-06-01")))

p2 <- ggplot(India_aggregate_date,aes(x=year_month,y=total_cases))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2020-06-01")))+
  ylim(0,15.0e+6)

p3 <- ggplot(India_aggregate_date,aes(x=year_month,y=total_deaths))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2020-06-01")))+
  ylim(0,400000)

p4<- ggplot(India_aggregate_date,aes(x=year_month,y=total_tests))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2020-06-01")))+
  ylim(0,1.0e+9)

p5 <- ggplot(India_aggregate_date,aes(x=year_month,y=tests_per_case))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2020-06-01")))+
  ylim(0,4000)

p6 <- ggplot(India_aggregate_date,aes(x=year_month,y=reproduction_rate))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2020-06-01")))

plot_grid(p1,p2,p3,p4,p5,p6)

  
#Plotting the next 6 months of 2020 
p25 <- ggplot(India_aggregate_date,aes(x=year_month,y=stringency_index))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-07-01"),as.Date("2020-12-01")))

p26 <- ggplot(India_aggregate_date,aes(x=year_month,y=total_tests))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-07-01"),as.Date("2020-12-01")))+
  ylim(0,4.0e+9)

p27 <- ggplot(India_aggregate_date,aes(x=year_month,y=total_cases))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-07-01"),as.Date("2020-12-01")))+
  ylim(0,4.0e+8)

p28 <- ggplot(India_aggregate_date,aes(x=year_month,y=total_deaths))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-07-01"),as.Date("2020-12-01")))+
  ylim(0,7.0e+6)

p29 <- ggplot(India_aggregate_date,aes(x=year_month,y=tests_per_case))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-07-01"),as.Date("2020-12-01")))+
  ylim(0,3000)

p30 <- ggplot(India_aggregate_date,aes(x=year_month,y=reproduction_rate))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2020-07-01"),as.Date("2020-12-01")))

plot_grid(p25,p26,p27,p28,p29,p30)

#Plotting for the first 6 months of 2021
p31 <- ggplot(India_aggregate_date,aes(x=year_month,y=stringency_index))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-01-01"),as.Date("2021-06-01")))

p32 <- ggplot(India_aggregate_date,aes(x=year_month,y=total_cases))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-01-01"),as.Date("2021-06-01")))+
  ylim(0,2.0e+9)

p33 <- ggplot(India_aggregate_date,aes(x=year_month,y=total_deaths))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-01-01"),as.Date("2021-06-01")))+
  ylim(0,2.0e+7)

p34 <- ggplot(India_aggregate_date,aes(x=year_month,y=total_tests))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-01-01"),as.Date("2021-06-01")))+
  ylim(0,2.0e+10)

p35 <- ggplot(India_aggregate_date,aes(x=year_month,y=tests_per_case))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-01-01"),as.Date("2021-06-01")))+
  ylim(0,4000)

p36 <- ggplot(India_aggregate_date,aes(x=year_month,y=reproduction_rate))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-01-01"),as.Date("2021-06-01")))

plot_grid(p31,p32,p33,p34,p35,p36)

#Plotting for the remaing 6 months of 2021 
p37 <- ggplot(India_aggregate_date,aes(x=year_month,y=stringency_index))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-07-01"),as.Date("2021-12-01")))

p38 <- ggplot(India_aggregate_date,aes(x=year_month,y=total_cases))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-07-01"),as.Date("2021-12-01")))+
  ylim(0,15.0e+8)

p39 <- ggplot(India_aggregate_date,aes(x=year_month,y=total_deaths))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-07-01"),as.Date("2021-12-01")))+
  ylim(0,2.0e+7)

p40 <- ggplot(India_aggregate_date,aes(x=year_month,y=total_tests))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-07-01"),as.Date("2021-12-01")))+
  ylim(0,3.0e+10)

p41 <- ggplot(India_aggregate_date,aes(x=year_month,y=tests_per_case))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-07-01"),as.Date("2021-12-01")))+
  ylim(0,7000)

p42 <- ggplot(India_aggregate_date,aes(x=year_month,y=reproduction_rate))+
  geom_line()+
  scale_x_date(limits = c(as.Date("2021-07-01"),as.Date("2021-12-01")))

plot_grid(p37,p38,p39,p40,p41,p42)



citation(package = "tidyverse")
citation(package = "corrplot")
citation(package = "mice")
citation(package = "cowplot")



