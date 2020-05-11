setwd("C:/Users/Osilade/Documents/PG- Data and Business/stat/Project 2/")
###Import Dataset 
library(readr)
Cold_Storage_Jan <- read_csv("Cold_Storage_Jan.csv")

###Assign to work with Season and Temperature columns and data only 
## working with summer temperature 
Summer_Temp = Cold_Storage_Jan$Temperature[Cold_Storage_Jan$Season=='Summer']
Avg_Summer_Temp = mean(Summer_Temp)
Avg_Summer_Temp
hist(Summer_Temp)
range(Summer_Temp)
summary(Summer_Temp)
##checking the mode of the temperature 
sort(Summer_Temp)
names(table(Summer_Temp))[table(Summer_Temp)==max(table(Summer_Temp))]
## IQR FOR WINTER 
Summer_IQR =IQR(Summer_Temp)
print(age_iqr)

## working with Winter Temperature 
Winter_Temp = Cold_Storage_Jan$Temperature[Cold_Storage_Jan$Season=='Winter']
Avg_Winter_Temp = mean(Winter_Temp)
Avg_Winter_Temp
Median_Winter_temp = median(Winter_Temp)
Median_Winter_temp
hist(Winter_Temp)
range(Winter_Temp)
summary(Winter_Temp)

##checking the mode of the temperature 
sort(Winter_Temp)
names(table(Winter_Temp))[table(Winter_Temp)==max(table(Winter_Temp))]

## IQR FOR WINTER 
Winter_iqr=IQR(Winter_Temp)
print(age_iqr)


## Working With Rainy Temperature
Rainy_Temp = Cold_Storage_Jan$Temperature[Cold_Storage_Jan$Season=='Rainy']
Avg_Rainy_Temp = mean(Rainy_Temp)
Avg_Rainy_Temp
hist(Rainy_Temp)
range(Rainy_Temp)
## IQR FOR WINTER 
Rainy_iqr=IQR(Rainy_Temp)
print(age_iqr)

summary(Rainy_Temp)
##checking the mode of the temperature 
sort(Rainy_Temp)
names(table(Rainy_Temp))[table(Rainy_Temp)==max(table(Rainy_Temp))]


## Finding the overall mean temperature 
Avg_Cold_Storage = mean(Cold_Storage_Jan$Temperature)
Avg_Cold_Storage

summary(Cold_Storage_Jan)
mean(Cold_Storage_Jan$Temperature[Cold_Storage_Jan$Month=='May'])


  
## Finding the overall S.D 
sD_Cold_Storage  = sd(Cold_Storage_Jan$Temperature)
sD_Cold_Storage

library(rpivotTable)
rpivotTable(Cold_Storage_Jan)

boxplot(Temperature~Season,Cold_Storage_Jan,horizontal = TRUE,col=(c("darkblue","tomato","darkgreen")),main='Box plot by Temperature')

hist((Cold_Storage_Jan$Temperature),main= 'Cold Storage Temp', xlab =  'Temperature')

##Finding the normal distribution for the pb that the annual temperature fell below 2C
##prob that temperature falls below 2'oC (T < 2'oC)
Cold_Storagepnorm= pnorm(2,mean = Avg_Cold_Storage,sd= sD_Cold_Storage,lower.tail = T)
Cold_Storagepnorm
##drawing the norm dist



## prob that temperature goes above 4'OC (T > 4'oc)
Cold_storagepnorm2 = 1-pnorm(4,mean = Avg_Cold_Storage,sd= sD_Cold_Storage)
Cold_storagepnorm2

## Checking what percentage the AMC falls under
Percentage_of_AMC = Cold_Storagepnorm + Cold_storagepnorm2
Percentage_of_AMC
Percentage_of_AMC2 = Percentage_of_AMC * 100
Percentage_of_AMC2


###ANOVA
library(rmarkdown)
library(MASS)
###one way
by(Cold_Storage_Jan$Temperature,INDICES = Cold_Storage_Jan$Season,FUN = mean)
by(Cold_Storage_Jan$Temperature,INDICES = Cold_Storage_Jan$Season,FUN = var)
attach(Cold_Storage_Jan)
by(Cold_Storage_Jan$Temperature,INDICES = Cold_Storage_Jan$Season,FUN = summary)
boxplot(Temperature~Season,horizontal = TRUE,col=(c('Orange','tomato','light blue')),main='Boxplot of Temperature')
Cold_Storage_Jan_Nova = aov(Temperature~Season,data=Cold_Storage_Jan)
Cold_Storage_Jan_Nova
summary(Cold_Storage_Jan_Nova)
pf(25.32,2,362,lower.tail = FALSE)
TukeyHSD(Cold_Storage_Jan_Nova)



##QUESTION 2 
Cold_Storage_Mar <- read_csv("Cold_Storage_Mar.csv")
Cold_Storage_mar_temp = Cold_Storage_Mar$Temperature[Cold_Storage_Mar$Season=='Summer']
hist(Cold_Storage_mar_temp)
Avg_Cold_Storage_mar = mean(Cold_Storage_mar_temp)
Avg_Cold_Storage_mar
Cold_Storage_Mar_Iqr= IQR(Cold_Storage_mar_temp)
print(Cold_Storage_Mar_Iqr)

## creating a box plot  
boxplot(Feb_Mar_temp,horizontal = TRUE,col=(c("green")),main='Box plot by Temperature')

sD_Cold_Storage_Mar = sd(Cold_Storage_mar_temp)
sD_Cold_Storage_Mar

t_stat = (Avg_Cold_Storage_mar - 3.9)/(sD_Cold_Storage_Mar/sqrt(length(Cold_Storage_mar_temp)))
t_stat
1-pt(t_stat,df=length(Cold_Storage_mar_temp)-1)

##checking the t -test
t.test(Feb_Mar_temp,mu=3.9,alternative = 'greater')

## Reject the null as the P value is less than the alpha ..0.1





