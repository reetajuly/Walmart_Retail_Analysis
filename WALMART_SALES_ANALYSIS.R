
library(ggplot2) # for visualization
library(dplyr)
library(lubridate)
library(tidyverse)
#library(car)
library(corrplot) # for corrplot
library(caTools) # for splitting dataset
library(MLmetrics)
#install.packages('klaR')
library(kernlab)
library(readr)
library(caret)
#install.packages("GGally") 
library(gplots)
library(randomForest)
library(forecast)
library(GGally)
library(psych)

setwd("~/SIMPLILEARN/PROJECTS/R PROJECT/1572585064_walmart_store_sales")


#----------------------------------LOADING DATA-------------------------------------- 
WALMART_DATA <-  read.csv('Walmart_Store_sales.csv',stringsAsFactors = F)

WALMART_DATA$Date <- dmy(WALMART_DATA$Date)

View(WALMART_DATA)

str(WALMART_DATA)

dim(WALMART_DATA)

#---------------------------CHECKING FOR MISSING VALUES------------------------------------------

any(is.na(WALMART_DATA))

#---------------------------FINDING THE STORE WITH MAXIMUM SALES -------------------------------

STORE_MAX_SALE <- aggregate(WALMART_DATA$Weekly_Sales, by = list(WALMART_DATA$Store), sum , na.rm = T)
STORE_MAX_SALE <- STORE_MAX_SALE %>% rename(STORES = Group.1, AVG.WEEKLYSALES = x)

STORE_MAX_SALE = STORE_MAX_SALE[which.max(unlist(STORE_MAX_SALE$x)),]
STORE_MAX_SALE
str(STORE_MAX_SALE)

PLT_1 <- ggplot(data=STORE_MAX_SALE, aes(x=STORES, y=AVG.WEEKLYSALES))+  
      geom_bar(stat="identity" , fill="steelblue",colors ='black') + theme_minimal()
  
PLT_1 <- PLT_1 + geom_text(aes(label=round(AVG.WEEKLYSALES,digits = 2)),size = 3, hjust = 2, vjust = 2,color = 'white')
#+coord_flip()
PLT_1


# STORE 20 HAS MAXIMUM SALES - $301397792

#--------------------------FINDING STANDARD DEVIATION ---------------------------------

STD <- aggregate(WALMART_DATA$Weekly_Sales, by = list(WALMART_DATA$Store), sd)
STD <- STD %>% rename(STORES = Group.1, STANDARD_DEVIATION = x)
STD = STD[which.max(unlist(STD$STANDARD_DEVIATION)),]
STD

PLT_1 <- ggplot(data=STD, aes(x=STORES, y=STANDARD_DEVIATION)) +  
  geom_bar(stat="identity" , fill="steelblue",colors ='black') + theme_minimal() + coord_flip()
+ geom_text(aes(label=y),size = 3, hjust = 2, vjust = 2,color = 'white')


PLT_1

# STORE 14 HAS MAXIMUM STANDARD DEVIATION - $317569.9

#-----------------------------COEFFICIENT OF MEAN OF STD---------------------
Coeff_Standard_Deviation <- STD$STANDARD_DEVIATION / mean(WALMART_DATA$Weekly_Sales) * 100
STD <- mutate(STD,Coeff_Standard_Deviation)
View(STD)
Coeff_Standard_Deviation


PLT_1 <- ggplot(data=STD, aes(x=STORES, y=Coeff_Standard_Deviation)) + 
  geom_bar(stat="identity",fill = 'steelblue',colors = 'black') + theme_minimal()
  
PLT_1 <- PLT_1 + geom_text(aes(label=round(Coeff_Standard_Deviation,digits = 2)),size = 3,hjust = 2, vjust = 3,color = 'white') 

PLT_1

#COEFFICIENT OF MEAN OF STANDARD DEVIATION OF STORE 14 = 30.33244

#-------------------------QUARTERLY GROWTH RATE -------------------------------


QUARTERS <- quarters(WALMART_DATA$Date)
WALMART_DATA = mutate(WALMART_DATA,QUARTERS)

WALMART_DATA <- mutate(WALMART_DATA,Year = as.numeric(format(WALMART_DATA$Date, "%Y")))

Q2 <- filter(WALMART_DATA, QUARTERS == 'Q2' & Year == '2012')
Q3 <- filter(WALMART_DATA, QUARTERS == 'Q3' & Year == '2012')

Q2 <- aggregate(Q2$Weekly_Sales,by= list(Store = Q2$Store),sum)
Q3 <- aggregate(Q3$Weekly_Sales,by= list(Store = Q3$Store),sum)

str(Q2)

temp <- merge(Q2,Q3,by = "Store")

temp <- temp %>% rename(Q2 = x.x, Q3 = x.y)

temp['Growth_Ratio'] <- NA

for(i in 1:nrow(temp)){ temp$Growth_Ratio <- ((temp$Q3 - temp$Q2) / temp$Q2) * 100}


temp <- temp[order(round(temp$Growth_Ratio), decreasing = TRUE), ]  
   
head(temp , n=5)

qq <- mutate(growth = (lead(Total,1) - Total)/Total )


#------------STORE 7 HAS HIGHEST QUARTERLY GROWTH RATE IN Q32012------------------------------

#-------------------------------------------------------------------------------------------

#--------------------------HOLIDAYS WHICH HAVE HIGHER SALES------------------------------------

#----------------Relation between Holdays and Weekly Sales--------------------------------------------------

PLT_1 <- ggplot(data=STORE1, aes(x=STORE1$Holiday_Flag, y=STORE1$Weekly_Sales)) +
  geom_bar(stat="identity",col='blue',fill='green',width = 0.5) + theme_minimal() + geom_col(width = 0.5)

PLT_1
NON_HOL_FLAG <- filter(WALMART_DATA[1:4],Holiday_Flag == 0)
Mean_non_hol_flag <- mean(NON_HOL_FLAG$x)  #----------Mean of all stores on non holiday - 1046965---------------------------


WALMART_DATA$New_holiday <- NA
str(WALMART_DATA)


#Super Bowl: 12-Feb-10, 11-Feb-11, 10-Feb-12, 8-Feb-13
#Labour Day: 10-Sep-10, 9-Sep-11, 7-Sep-12, 6-Sep-13
#Thanksgiving: 26-Nov-10, 25-Nov-11, 23-Nov-12, 29-Nov-13
#Christmas: 31-Dec-10, 30-Dec-11, 28-Dec-12, 27-Dec-13

Super <- c('2011-02-11','2010-02-12','2012-02-10' ,'2013-02-08')
Labour <- c('2010-09-10','2011-09-09','2012-09-07','2013-09-06')
ThGiving <- c('2010-11-26','2011-11-25','2012-11-23','2013-11-29')
Cmas <- c('2010-12-31','2011-12-30','2012-12-28','2013-12-27')


WALMART_DATA$New_holiday <- as.character(WALMART_DATA$New_holiday)
WALMART_DATA$Date <- as.character(WALMART_DATA$Date)

for(i in 1: nrow(WALMART_DATA))
{
  if(WALMART_DATA$Holiday_Flag[i] == 1){
    print(WALMART_DATA$Date[i])
  if(WALMART_DATA$Date[i] %in% (Super))
  {WALMART_DATA$New_holiday[i] <- 'S'}
  else if(WALMART_DATA$Date[i] %in% (Labour))
  { WALMART_DATA$New_holiday[i] <- 'L'}
  else if(WALMART_DATA$Date[i] %in% (ThGiving))
  {WALMART_DATA$New_holiday[i] <-'T'}
  else if (WALMART_DATA$Date[i] %in% (Cmas))
  {WALMART_DATA$New_holiday[i] <- 'C'}
  else{WALMART_DATA$New_holiday[i] <- 'H'}
  }
}

View(WALMART_DATA)


SuperBowl_Sales <- filter(WALMART_DATA, Holiday_Flag ==1, New_holiday == 'S')
MEAN_SUPERBOWL <- mean((SuperBowl_Sales$Weekly_Sales))
MEAN_SUPERBOWL

LABOUR_SALES <- filter(WALMART_DATA, Holiday_Flag ==1, New_holiday == 'L')
MEAN_LABOUR <- mean((LABOUR_SALES$Weekly_Sales))
MEAN_LABOUR


THNKSGIV_SALES <- filter(WALMART_DATA, Holiday_Flag ==1, New_holiday == 'T')
MEAN_THNKSGIV <- mean((THNKSGIV_SALES$Weekly_Sales))
MEAN_THNKSGIV


CHRISTMAS_SALES <- filter(WALMART_DATA, Holiday_Flag ==1, New_holiday == 'C')
MEAN_CHRISTMAS <- mean((CHRISTMAS_SALES$Weekly_Sales))
MEAN_CHRISTMAS

Mean_non_hol_flag


#------------------------OUTPUT - THANKSGIVING SALES IS HIGHER THAN AVG NON HOLIDAY SALES------------------------------------------


#-----------------Extracting month from Date and adding to dataframe--------------------------



##ExtractDate(as.character('2/19/20 20:31'))


############-----------FUNCTION TO EXTRACT MONTH------------------------------------------

ExtractMonth <- function(Date){
  
  ExtractedDate <- as.Date(Date, format="%m/%d/%Y")
  month <- format(ExtractedDate,"%b")
  return(month)
  
}

#----------------------FUNCTION TO EXTRACT YEAR------------------------------------------
ExtractYear <- function(Date){
  
  ExtractedDate <- as.Date(Date, format="%m/%d/%Y")
  year <- year(ExtractedDate)
  return(year)
  
}

#-----------------------FUNCTION TO EXTRACT DAY--------------------------------------------

ExtractDay <- function(Date){
  
  ExtractedDate <- as.Date(Date, format="%m/%d/%Y")
  day <- day(ExtractedDate)
  return(day)
  
}

df_date = ExtractMonth(WALMART_DATA$Date)
WALMART_DATA = cbind(WALMART_DATA,df_date)
names(WALMART_DATA)[names(WALMART_DATA)=="df_date"] <- "Month"


df_year = ExtractYear(WALMART_DATA$Date)
WALMART_DATA = cbind(WALMART_DATA,df_year)
names(WALMART_DATA)[names(WALMART_DATA)=="df_year"] <- "Year"

df_day = ExtractDay(WALMART_DATA$Date)
WALMART_DATA = cbind(WALMART_DATA,df_day)
names(WALMART_DATA)[names(WALMART_DATA)=="df_day"] <- "Day"


#-----------------------------------------------------------------------------------------------
monthlyData <- WALMART_DATA %>% group_by(Month,Year) %>% summarise(Mean_Monthly =
                                                               mean(Weekly_Sales))
View(monthlyData)

PLT_1 <- ggplot(data=monthlyData, aes(x=Month, y=Mean_Monthly)) + 
  geom_bar(stat="identity",fill = 'steelblue',colors = 'black') + theme_minimal()

PLT_1 <- PLT_1 + geom_text(aes(label= Mean_Monthly),size = 3,hjust = 2, vjust = 3,color = 'white') 

PLT_1


Semester <- semester(WALMART_DATA$Date,with_year = T)
WALMART_DATA <- mutate(WALMART_DATA, Semester)

semester_data <- WALMART_DATA %>% group_by(Semester,Year) %>% summarise(Mean_Semester= mean(Weekly_Sales))
View(semester_data)



#------------------------------------------------------------------------------------------
#-----------------------------STATISTICAL MODEL------------------------------------------
## Build  prediction models to forecast demand (Modeling)  


WALMART_DATA %>% arrange(mdy(WALMART_DATAE1$Date))
WALMART_DATA$RESTRUCT_DATE=row_number(WALMART_DATA$Date)
View(WALMART_DATA)

#Change dates into days by creating new variable
WALMART_DATA <- WALMART_DATA%>%mutate(Days=day(Date))
View(WALMART_DATA)

STORE1 <- filter(WALMART_DATA,Store == 1)
STORE1
dim(STORE1)
str(STORE1)
View(STORE1)
summary(STORE1) 


dropc <- c('Store','Date','RESTRUCT_DATE','Days')

STORE1 <- STORE1[,!names(STORE1) %in% dropc]

#---------------------------FIND CORRELATION BETWEEN FEATURES---------------------------------------

cor_temp <- cor(STORE1$Weekly_Sales, STORE1$Temperature)  #----------- -0.2227006
cor_fuel <- cor(STORE1$Weekly_Sales, STORE1$Fuel_Price)   #-----------  0.1245916
cor_cpi <- cor(STORE1$Weekly_Sales, STORE1$CPI)          #-----------  0.2254077
cor_unemployment <- cor(STORE1$Weekly_Sales, STORE1$Unemployment) #----------- -0.09795539
cor_holidayflag <- cor(STORE1$Weekly_Sales, STORE1$Holiday_Flag) #-----------  0.1949052

par(mfrow=c(3,3))
for(i in 3:6){
  plot(STORE1[,i], 
       STORE1$Weekly_Sales, 
       main=names(STORE1[i]), 
       ylab=names(STORE1$Weekly_Sales), 
       xlab="",
       pch = 21,
       bg = "red",   # Fill color
       col = "blue", # Border color
       cex = 3,      # Symbol size
       lwd = 3)  
}

pairs.panels(STORE1,
             smooth = F,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "pearson", # Correlation method (also "spearman" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)  
#--------------------------------------BoxPlot - Check for outliers-------------------------------------

par(mfrow=c(1, 4))  # divide graph area in 4 columns

boxplot(STORE1$Temperature, main="Temperature",col='red', sub=paste("Outlier rows: ", boxplot.stats(STORE1$Temperature)$out))  

boxplot(STORE1$Fuel_Price, main="Fuel_Price",col='#E69F00', sub=paste("Outlier rows: ", boxplot.stats(STORE1$Fuel_Price)$out)) 

boxplot(STORE1$CPI, main="CPI",col='red', sub=paste("Outlier rows: ", boxplot.stats(STORE1$CPI)$out)) 

boxplot(STORE1$Unemployment, main="Unemployment",col='#E69F00', sub=paste("Outlier rows: ", boxplot.stats(STORE1$Unemployment)$out))

#-------------------------------------Density plot - Check if the response variable is close to normality----------------------------------------------------

library(e1071)
par(mfrow=c(2, 2))  # divide graph area

plot(density(STORE1$Temperature), main="Density Plot: Temperature", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(STORE1$Temperature), 2)))  # density plot for 'speed'
polygon(density(STORE1$Temperature), col="red")

plot(density(STORE1$Fuel_Price), main="Density Plot: Fuel Price", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(STORE1$Fuel_Price), 2)))  # density plot for 'dist'
polygon(density(STORE1$Fuel_Price), col="red")

plot(density(STORE1$CPI), main="Density Plot: CPI", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(STORE1$CPI), 2)))  # density plot for 'speed'
polygon(density(STORE1$CPI), col="red")

plot(density(STORE1$Unemployment), main="Density Plot: Unemployment", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(STORE1$Unemployment), 2)))  # density plot for 'dist'
polygon(density(STORE1$Unemployment), col="red")


#------------From the correlation score we see that Temperature,Unemployment are negatively correlated and 
#               Fuel,CPI and Holidays have positive impact on sales.-------------------------------------



#----------------------------DATA STANDARDIZATION---------------------------------------------------------------

Store_1 <- STORE1 %>% mutate_all(~(scale(.) %>% as.vector))
View(Store_1)

#-------------------------------------BUILDING MODELS   -------------------------------------------------------------------------

# setting seed to generate a 
# reproducible random sampling
set.seed(123)


# Data split


sample = sample.split(Store_1, SplitRatio = 0.7) 
train = subset(Store_1, sample == T)
test = subset(Store_1, sample == F)
dim(train)
dim(test)
View(train)
View(test)


#--------------------------LINEAR REGRESSION USING CROSS VALIDATION-------------------------------------------------

train_control <- trainControl(method = "cv",
                              number = 10)

lm_model <- train(Weekly_Sales ~ ., data = train,
                        trControl = train_control,
                        method = "lm")

modelSummary  <- summary(lm_model)


y_pred_train = predict(lm_model, newdata = train)

length(y_pred_train)
y_pred_train

# Visualizing the training set results
ggplot() + 
  geom_point(aes(x=train$Weekly_Sales,y=y_pred_train)) + 
  xlab('actual_sales') +
  ylab('predicted_sales')+
  ggtitle('comparison of train data')

# Visualizing the test set results

y_pred_test = predict(lm_model, newdata = test)

y_pred_test

ggplot() + 
  geom_point(aes(x=test$Weekly_Sales,y=y_pred_test)) +
  xlab('actual_sales') +
  ylab('predicted_sales')+
  ggtitle('comparison of test data')

### Parameters to validate the accuracy of the model and improvise.

MAPE(y_pred_test,test$Weekly_Sales)

RMSE(y_pred_test,test$Weekly_Sales)

actuals_preds <- data.frame(cbind(actuals=test$Weekly_Sales, predicteds=y_pred_test)) 
# make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy <- min_max_accuracy * 100
min_max_accuracy

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals) 
mape

#-----------------------------------RANDOM FOREST MODEL  ----------------------------------------------------------

set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1: 10))

rm_model <- train(Weekly_Sales~.,
                 data = train,
                 method = "rf",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 300)
print(rm_model)
plot(rm_model)

test.features = subset(test, select=-c(Weekly_Sales))
test.target = subset(test, select=Weekly_Sales)[,1]

predictions = predict(rm_model, newdata = test.feature)

# MAPE and RMSE

MAPE(predictions,test.target)

RMSE(predictions,test.target)

actuals_preds <- data.frame(cbind(actuals=test.target, predicteds=predictions)) 
# make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  

min_max_accuracy

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals) 
mape


