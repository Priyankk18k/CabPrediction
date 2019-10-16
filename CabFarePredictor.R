rm (list = ls()) # Cleaing the evironment
setwd('C:/Users/Priyank/Desktop/kannu/Documents/cabPricePrediction') #setting directory
getwd()

#Reading the data
Gtest = read.csv('test.csv')
Gtrain = read.csv('train_cab.csv')

#knowing structure of our data
str(Gtrain) #We have 16067 obs. of  7 variables

#Changing the data types of our variables
Gtrain$fare_amount = as.numeric(as.character(Gtrain$fare_amount))
Gtrain$passenger_count = as.integer(as.character(Gtrain$passenger_count))


summary(Gtrain)
############################DATA PRE-POCESSING########################
######################################################################

#1.***************************Data Cleaning and Feature Engineering ***************************
Gtrain$passenger_count[Gtrain$passenger_count > 8] <- 2 
Gtrain$pickup_latitude[Gtrain$pickup_latitude > 90 ] <- 0
Gtrain$fare_amount[Gtrain$fare_amount > 100 ] <- 0
Gtrain$fare_amount[Gtrain$fare_amount < 0 ] <- 0

#Extracting the date time year month from date column and storing each variable in Training dataframe
library(dplyr) #for using mutate
library(lubridate) #for using ymd_hms
Gtrain <- mutate(Gtrain,
                    pickup_datetime = ymd_hms(`pickup_datetime`),
                    month = as.integer(month(pickup_datetime)),
                    year = as.integer(year(pickup_datetime)),
                    dayOfWeek = as.integer(wday(pickup_datetime)),
                    hour = hour(pickup_datetime),
                    hour = as.integer(hour(pickup_datetime))
)

#replacing all the 0's in Data as NA
Gtrain[Gtrain == 0] <- NA

#for cauculating distance from logi/lati
library(geosphere)
Gtrain$pickup_longitude = abs(Gtrain$pickup_longitude)
Gtrain$pickup_latitude = abs(Gtrain$pickup_latitude)
Gtrain$dropoff_longitude = abs(Gtrain$dropoff_longitude)
Gtrain$dropoff_latitude = abs(Gtrain$dropoff_latitude)

#Gtrain$long = abs(Gtrain$pickup_longitude - Gtrain$dropoff_longitude)
#Gtrain$lati = abs(Gtrain$pickup_latitude - Gtrain$dropoff_latitude)

Gtrain = Gtrain %>% 
  mutate(distance = by(Gtrain, 1:nrow(Gtrain), function(row) { 
    distHaversine(c(row$pickup_longitude, row$pickup_latitude), c(row$dropoff_longitude,row$dropoff_latitude))/1000}))
summary(Gtrain$distance)

Gtrain$distance[Gtrain$distance > 500 ] <- 0
Gtrain$distance[Gtrain$distance < 0 ] <- 0


#2.***************************Missing value analysis ***************************
Missing_val = data.frame(sapply(Gtrain, function(x) sum(is.na(x))))

#Show Percentage of missing value
Missing_val$Variables = row.names(Missing_val)
row.names(Missing_val) = NULL
names(Missing_val)[1] = "Missing_values"
Missing_val$Missing_percentage=(Missing_val$Missing_values/nrow(Gtrain))*100

Missing_val

Gtrain = na.omit(Gtrain) #deleted rows that has NA

#3.***************************Visitualisation ***************************
library(ggplot2)
library(scales)

par(mar=c(3,3,1,1))
par(mfrow=c(3,2))

#A)Univariate
#variable quantity
#par("mar") - set it to par(mar= c(1,1,1,1)) to avoid margin error for plots
#par("mar")
hist(Gtrain$fare_amount, main="ScatterPlot of fare_amount")
hist(Gtrain$passenger_count, main="ScatterPlot of passenger_count")
hist(Gtrain$month, main="ScatterPlot of month")
hist(Gtrain$year, main="Hist of year")
hist(Gtrain$dayOfWeek, main="Hist of dayOfWeek")
hist(Gtrain$hour, main="Hist of hour")
plot(Gtrain$distance, main="ScatterPlot of distance")
#plot(Gtrain$long, main="ScatterPlot of Abs longitude")
#plot(Gtrain$lati, main="ScatterPlot of Abs latitude")


#check skwness of the target variable
library(PerformanceAnalytics)
skew_xts <-  skewness(Gtrain$fare_amount)
skew_xts
#unlike python where i kept my fare under 100 and it gave me skew of 2.9 , R gives me 10.85835 when the fare is kept under 500.



#B)bi-variant 
#Continuous variables vs target variable 
#PLoting the graph for hour and Fare
ggplot(Gtrain,aes(x = hour, y = fare_amount))+
  geom_line()+
  labs(x= "hour of the day")+
  scale_x_discrete(limits = c(0:23))+
  scale_y_continuous(limits=c(0,180))
#From the above graph we can see that the timeing is not affecting too much. Maximin dots are below 100. 

#PLoting the year for distance and Fare
ggplot(Gtrain,aes(x = year, y = fare_amount))+geom_point()+
  geom_line()+
  labs(x= "year")+
  scale_x_discrete(limits = c(0:23))+
  scale_y_continuous(limits=c(0,180))

#PLoting the graph for passanger_count and Fare
gplot_p <- ggplot(data=Gtrain, aes(x=passenger_count, y=fare_amount)) + geom_point()+ geom_line()+ 
  ggtitle("Time and Fare Plot") +
  xlab("Passenger Count ") + 
  ylab("Fare")
gplot_p
# From the Graph it seems passenger count is not affecting the fare and frequency of 1 pssenges are high

#PLoting the graph for day and Fare
gplot_d <- ggplot(data=Gtrain, aes(x=dayOfWeek, y=fare_amount)) + geom_point()+ geom_line()+ 
  ggtitle("Day count and Fare Plot") +
  xlab("Day Count ") + 
  ylab("Fare")
gplot_d

#PLoting the graph for distance and Fare
gplot <- ggplot(data=Gtrain, aes(x=Gtrain$distance, y=Gtrain$fare_amount)) + geom_point()+ geom_line()+ 
  ggtitle("Distance and Fare Plot") +
  xlab("Distance in KM ") + 
  ylab("Fare")
gplot


boxplot.
#4.*************************** Outlier Analysis ***************************
numeric_index = sapply(Gtrain, is.numeric) # creating numerical value index
numeric_data = Gtrain[,numeric_index] # storing numeric data
cnames = colnames(numeric_data) #storing numeric data column names
summary(numeric_data)
#Creating box-plot to analyze outliers
for (i in 1:length(cnames)){
  assign(paste0("gn", i), ggplot(aes_string(y = cnames[i], x = "fare_amount"), data = subset(Gtrain)) +
           stat_boxplot(geom = "errorbar", width = 0.5) + 
           geom_boxplot(outlier.colour = "red", fill = "blue", outlier.shape = 18, outlier.size = 1, notch = FALSE) + 
           theme(legend.position = "bottom") + labs(y = cnames[i], x="fare") + ggtitle(paste("Boxplot of fare for", cnames[i])))
}
gridExtra::grid.arrange(gn2, gn3, gn4,gn5, gn6, gn7,gn8, gn9, gn10, gn11, ncol = 4, nrow = 3) # exclud if gn1 as that is unique for each observation

#replace outliers with NA 
for(i in cnames) {
  print(i)
  val = Gtrain[,i][Gtrain[,i] %in% boxplot.stats(Gtrain[,i]) $out]
  
  print(length(val))
  Gtrain[,i][Gtrain[,i] %in% val] = NA
}

#imputing NA values--deleting
Gtrain=na.omit(Gtrain)
summary(Gtrain)


# creating copy of preprocessed data 
mydata = Gtrain

#5.*************************** Feature selection ***************************
#install.packages("corrgram") # for correlation graph
library(corrgram)

#A. Correlation check on continuous variable
round(cor(numeric_data),2) #Correlation table column wise
corrgram(Gtrain[, numeric_index], order = F, upper.panel = panel.pie, text.panel = panel.txt, main = "correlation plot") 


#Multicollinearity test
library(usdm)

vifcor(Gtrain[,c(0,1,2,3)])

############################MODELING########################
############################################################

#Sampling
#1. Non scaled data(scalling is not required as i am using scaled data for my model)
set.seed(101)
train_index = sample(1:nrow(Gtrain), 0.8*nrow(Gtrain))
data_train = Gtrain[train_index,] 
data_test = Gtrain[-train_index,]

train_cab_final = subset(Gtrain, select = c(1,7:12))
summary(train_cab_final)
str(train_cab_final)



#function error matrics
mape = function(actual, predict){
  mean(abs((actual-predict)/actual))*100}
rmse = function(actual, predict){
  sqrt(sum((predict - actual)^2) / nrow(train_cab_final))}
rmpe = function(actual, predict){
  sqrt(sum((predict - actual)^2) / nrow(train_cab_final)) / mean(actual)}




##1. *******************Linear model*************************
#*************************************************************
#1. Model with only distance
linerModel = lm(fare_amount ~ distance, data = train_cab_final)
summary(linerModel)

#predicting the Fare amount for Test data
predict_fare_simple<-predict(linerModel, newdata = train_cab_final)
View(train_cab_final)
View(predict_fare_simple)
mape(train_cab_final$fare_amount, predict_fare_simple) #23.71183
rmse(train_cab_final$fare_amount, predict_fare_simple) #2.213257
rmpe(train_cab_final$fare_amount, predict_fare_simple) #0.26
#Multiple R-squared:  0.6514,	Adjusted R-squared:  0.6513 


---------------
#2. Model with every variable (Multiple linear regression)
#fit the Model with fare amount and all independant variables.
linerModel2 <- lm(fare_amount~., data = train_cab_final)
summary(linerModel2)
#predicting the Fare amount for Test data
predict_fare_lm<-predict(linerModel, newdata = train_cab_final)
View(train_cab_final)
View(predict_fare_lm)

mape(train_cab_final$fare_amount, predict_fare_lm) #23.71183
rmse(train_cab_final$fare_amount, predict_fare_lm) #`2.213257`
rmpe(train_cab_final$fare_amount, predict_fare_lm) #0.26
#Multiple R-squared:  0.6847,	Adjusted R-squared:  0.6846 



##2. *******************Random forest*************************
#*************************************************************
library(randomForest)
library(inTrees)

#model
model_RF = randomForest(fare_amount ~. , train_cab_final, importance = TRUE, ntree = 500)

#Error plotting
plot(model_RF) #my error i decreasing with higher number of trees

# Checking model by predicting on out of sample data
predictRF <- predict(model_RF, train_cab_final)
print(model_RF)

mape(train_cab_final$fare_amount, predictRF) #10.52
rmse(train_cab_final$fare_amount, predictRF) #0.95
rmpe(train_cab_final$fare_amount, predictRF) #0.11
#Mean of squared residuals: 4.049305
#% Var explained: 71.36

varImpPlot(model_RF1)
-------------------------------------
model_RF1 = randomForest(fare_amount ~ distance , train_cab_final, importance = TRUE, ntree = 100)

#Error plotting
plot(model_RF1) #my error i decreasing with higher number of trees

# Checking model by predicting on out of sample data
predictRF1 <- predict(model_RF1, train_cab_final)
print(model_RF1)

mape(train_cab_final$fare_amount, predictRF1) #10.52
rmse(train_cab_final$fare_amount, predictRF1) #0.95
rmpe(train_cab_final$fare_amount, predictRF1) #0.11
#Mean of squared residuals: 4.049305
#% Var explained: 71.36

plot(train_cab_final$fare_amount, predict_fare_simple, xlab = 'Actual values', ylab = 'Predicted values', main = 'simple liner')
plot(train_cab_final$fare_amount, predict_fare_lm, xlab = 'Actual values', ylab = 'Predicted values', main = 'Multilinear')
plot(train_cab_final$fare_amount, predictRF, xlab = 'Actual values', ylab = 'Predicted values', main = 'Random forest') 
#choosing random forest for my test dataset

#===========Predicting for Test Data========================#
apply(Gtest, 2, function(x){sum(is.na(x))})#no missing value
str(Gtest)
Gtest <- mutate(Gtest,
                   pickup_datetime = ymd_hms(`pickup_datetime`),
                   month = as.integer(month(pickup_datetime)),
                   year = as.integer(year(pickup_datetime)),
                   dayOfWeek = as.integer(wday(pickup_datetime)),
                   hour = hour(pickup_datetime),
                   hour = as.integer(hour(pickup_datetime))
)
summary(Gtest)

Gtest = Gtest %>% 
  mutate(distance = by(Gtest, 1:nrow(Gtest), function(row) { 
    distHaversine(c(row$pickup_longitude, row$pickup_latitude), c(row$dropoff_longitude,row$dropoff_latitude))/1000}))
summary(Gtest$distance)
str(Gtest)
View(Gtest)

test_cab_final = subset(Gtest, select = c(6:11))
View(test_cab_final)
str(test_cab_final)

predictRF_test = predict(model_RF, test_cab_final)



train_cab_final$predict_fare_lm = predict_fare_lm
train_cab_final$predictRF = predictRF
view(test_cab_final)

test_cab_final$fare_amount = predictRF_test

#PLoting the graph for distance and Fare
gplot <- ggplot(data=test_cab_final, aes(x=distance, y=fare_amount)) + geom_point()+ geom_line()+ 
  ggtitle("Distance and Fare Plot") +
  xlab("Distance in KM ") + 
  ylab("Fare")
gplot







