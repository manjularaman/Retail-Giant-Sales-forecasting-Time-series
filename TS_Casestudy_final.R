rm(list=ls())
#install.packages("car")
library(graphics)
library(fracdiff)
library(forecast)
library(tseries)
require(graphics)
#Libraries
library(stringr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(MASS)
library(car)


#Import the Datafile into a df

df <- read.csv("Global Superstore.csv", stringsAsFactors = F)

#Checking for duplicate rows
any(duplicated(df)) #No duplicates

# Checking missing value
sapply(df, function(x) sum(is.na(x))) # No missing values

#There are no duplicates
#Understanding Dimensions
dim(df)

#Structure of the dataset
str(df)

#printing first few rows
head(df)

#Exploring the data
summary(df)

#Changing columns to factor type variables 

df$Ship.Mode <- as.factor(df$Ship.Mode)
df$Segment <- as.factor(df$Segment)
df$State <- as.factor(df$State)
df$City <- as.factor(df$City)
df$Country <- as.factor(df$Country)
df$Market <- as.factor(df$Market)
df$Region <- as.factor(df$Region)
df$Category <- as.factor(df$Category)
df$Sub.Category <- as.factor(df$Sub.Category)
df$Product.Name <- as.factor(df$Product.Name)
df$Order.Priority <- as.factor(df$Order.Priority)

#Format request timestamp

#df$Order.Date <- as.POSIXlt(df$Order.Date, format = "%d-%m-%Y")
#df$Order.Date <- format(df$Order.Date,"%m-%Y")
df$Order.Date = as.Date(df$Order.Date, format="%d-%m-%Y")
df$Order.Date = format(df$Order.Date,"%Y-%m")
View(df)

#Creating group by with aggregated values
df_group <- group_by(df,Market,Segment,Order.Date) 
df_aggregated <- summarise(df_group,Agg_Sales=sum(Sales),Agg_Qty=sum(Quantity),Agg_Profit=sum(Profit))
df_aggregated 
View(df_aggregated)

df_agg_sorted <- arrange(df_aggregated, Order.Date)
View(df_agg_sorted)


#########################################################################################################################################


#Creating 7x3 21 subsets based on Segment and Market 
Segment_list <- levels(df$Segment)
Market_list <- levels(df$Market)
coeff_of_var <- matrix(nrow=3,ncol=7)

for (j in 1:length(Market_list)){ 
  for (i in 1:length(Segment_list)){
    assign(paste("df_subset",i,j,sep="_"),subset(df_agg_sorted,Market==Market_list[j] & Segment==Segment_list[i]))
    subset_generic <- subset(df_agg_sorted,Market==Market_list[j] & Segment==Segment_list[i])
    coeff_of_var[i,j] <- sd(subset_generic$Agg_Profit)/mean(subset_generic$Agg_Profit)
      }
}

colnames(coeff_of_var) <- Market_list
rownames(coeff_of_var) <- Segment_list
#Boxplots for Profit
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

library(cowplot)

plot_grid(ggplot(df_subset_1_1, aes(y=Agg_Profit))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(df_subset_2_1, aes(y=Agg_Profit))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(df_subset_3_1, aes(y=Agg_Profit))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
                    align = "v",nrow = 1)
plot_grid(ggplot(df_subset_1_2, aes(y=Agg_Profit))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(df_subset_2_2, aes(y=Agg_Profit))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(df_subset_3_2, aes(y=Agg_Profit))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
plot_grid(ggplot(df_subset_1_3, aes(y=Agg_Profit))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(df_subset_2_3, aes(y=Agg_Profit))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(df_subset_3_3, aes(y=Agg_Profit))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
plot_grid(ggplot(df_subset_1_4, aes(y=Agg_Profit))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(df_subset_2_4, aes(y=Agg_Profit))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(df_subset_3_4, aes(y=Agg_Profit))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
plot_grid(ggplot(df_subset_1_5, aes(y=Agg_Profit))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(df_subset_2_5, aes(y=Agg_Profit))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(df_subset_3_5, aes(y=Agg_Profit))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
plot_grid(ggplot(df_subset_1_6, aes(y=Agg_Profit))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(df_subset_2_6, aes(y=Agg_Profit))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(df_subset_3_6, aes(y=Agg_Profit))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
plot_grid(ggplot(df_subset_1_7, aes(y=Agg_Profit))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(df_subset_2_7, aes(y=Agg_Profit))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(df_subset_3_7, aes(y=Agg_Profit))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)



###Plots (box plot)
##############################################################################################################
#So the two consistently most Profitable Market-Segment segments are with the lowest coeff of variation
#df_subset5 <- subset(df_agg_sorted,Market == "EU" & Segment == "Consumer")
#cv = 0.6243052

#df_subset2 <- subset(df_agg_sorted,Market == "APAC" & Segment == "Consumer")
#cv = 0.6321323

####################################################################################################################
#So we now genrate the Time series for the top of these top2 most profitable Market segments-category

timeser_top_sales <- ts(df_subset_1_5$Agg_Sales)
plot(timeser_top_sales)
str(timeser_top_sales)

timeser_top_qty <- ts(df_subset_1_5$Agg_Qty)
plot(timeser_top_qty)
str(timeser_top_qty)

timeser_second_top_sales <- ts(df_subset_1_2$Agg_Sales)
plot(timeser_second_top_sales)
str(timeser_second_top_sales)

timeser_second_top_qty <- ts(df_subset_1_2$Agg_Qty)
plot(timeser_second_top_qty)
str(timeser_second_top_qty)

###After this we divide into train and test;last six months are train
timeser_top_sales_train <- as.ts(timeser_top_sales[1:42])
timeser_top_sales_test <- as.ts(timeser_top_sales[43:48])
timeser_top_qty_train <- as.ts(timeser_top_qty[1:42])
timeser_top_qty_test <- as.ts(timeser_top_qty[43:48])
timeser_second_top_sales_train <- as.ts(timeser_second_top_sales[1:42])
timeser_second_top_sales_test <- as.ts(timeser_second_top_sales[43:48])
timeser_second_top_qty_train <- as.ts(timeser_second_top_qty[1:42])
timeser_second_top_qty_test <- as.ts(timeser_second_top_qty[43:48])

##### Modelling the EU_Consumer Sales ##################################

plot(timeser_top_sales_train)

####Trying Holt Winter's method for smoothing
#plot(timeser_top_sales_train)

cols <- c("orange", "blue", "green", "yellow")
alphas <- c(0.1,0.2,0.3,0.5)
#alphas <- c(0.8)
labels <- c(paste("alpha =", alphas))
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(timeser_top_sales_train, alpha=alphas[i],
                                beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}

legend("bottomright", labels, col=cols, lwd=2)
#####Chosing alpha=0.3

smoothedseries <- HoltWinters(timeser_top_sales_train, alpha=0.3,
                              beta=FALSE, gamma=FALSE)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

timevals_in <- 1:42

smootheddf <- as.data.frame(cbind (timevals_in,rbind(timeser_top_sales_train[1],fitted(smoothedseries))))

colnames(smootheddf) <- c("Month","Sales")
smootheddf <- smootheddf[,-3]
###Multiplicative
#lines(smootheddf)
plot(smootheddf,type="line")
###Degree 2 polynomial
lmfit <- lm(Sales~ (sin(0.5*Month) *poly(Month,2) + cos(0.5*Month)) * poly(Month,2), data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='orange', lwd=3)
###Degree 3 polynomial
lmfit <- lm(Sales~ (sin(0.5*Month) *poly(Month,3) + cos(0.5*Month)) * poly(Month,3), data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='green', lwd=3)
### Degree 3 polynomial with 0.8 coeff
lmfit <- lm(Sales~ (sin(0.8*Month) *poly(Month,3) + cos(0.8*Month)) * poly(Month,3), data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='blue', lwd=3)
###Additive version
lmfit <- lm(Sales~ (sin(0.8*Month) + poly(Month,3) + cos(0.8*Month)) , data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='yellow', lwd=3)


##Multiplicative degree 3 polynomial  with 0.5 seems best

plot(smootheddf,type="line")
lmfit <- lm(Sales~ (sin(0.5*Month) *poly(Month,3) + cos(0.5*Month)) * poly(Month,3), data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='green', lwd=3)

#Locally predictable series
#We will model it as an ARMA series

local_pred <- timeser_top_sales_train - global_pred
plot(local_pred, col='red', type = "l")
###ACF and PACF
acf(local_pred)
acf(local_pred, type="partial")
###ACF 
armafit <- auto.arima(local_pred)
armafit
tsdiag(armafit)
#ARIMA(0,0,0)
#log likelihood=-452.3
#AIC=906.59   AICc=906.69   BIC=908.33
#Since it is ARIMA(0,0,0), there is no auto regressive component

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
#P=0.02
kpss.test(resi)
#P=0.1
###the residue is white noise

#Modeling the TS

outdata <- timeser_top_sales_test
timevals_out <- 43:48

global_pred_out <- predict(lmfit,data.frame(Month=timevals_out))

fcast <- global_pred_out
plot(fcast,type="l")


#Now, let's compare our prediction with the actual values, using MAPE


MAPE_class_dec <- accuracy(fcast,timeser_top_sales_test)[5]

MAPE_class_dec
## 26.82808

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(c(timeser_top_sales_train,timeser_top_sales_test), col = "black",type= "both")
lines(class_dec_pred, col = "red")

##Forcasting for next 6 months ( 49-54)
timevals_out <- 49:54
global_pred_out <- predict(lmfit,data.frame(Month=timevals_out))
View(global_pred_out)


#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(timeser_top_sales)
autoarima
##ARIMA(2,1,0) 
#log likelihood=-512.5
#AIC=1031   AICc=1031.56   BIC=1036.55
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser_top_sales - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
#p=0.0146 so we reject the null hypothesis that the series is not stationary
kpss.test(resi_auto_arima)
#p = 0.1 so we cannot reject the null hypothesis that the residual time series is stationary

#comparing prediction with the actual values, using MAPE


MAPE_class_dec <- accuracy(fitted(autoarima),timeser_top_sales)[5]

MAPE_class_dec
## 36.81048
####Classical decomposition MAPE was 26.82808 ; better than auto arima
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

plot(timeser_top_sales, col = "black",type= "both")
lines(fitted(autoarima), col = "red")


#####Modelling EU region Consumer Quantity ********#############

plot(timeser_top_qty_train)

####Trying Holt Winter's method for smoothing


cols <- c("orange", "green", "blue", "yellow")
alphas <- c(0.1,0.2,0.3,0.5)
#alphas <- c(0.8)
labels <- c(paste("alpha =", alphas))
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(timeser_top_qty_train, alpha=alphas[i],
                               beta=FALSE, gamma=FALSE)

  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}

legend("topleft", labels, col=cols, lwd=2)
#####Chosing alpha=0.3

smoothedseries <- HoltWinters(timeser_top_qty_train, alpha=0.3,
                              beta=FALSE, gamma=FALSE)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

timevals_in <- 1:42

smootheddf <- as.data.frame(cbind (timevals_in,rbind(timeser_top_qty_train[1],fitted(smoothedseries))))

colnames(smootheddf) <- c("Month","Qty")
smootheddf <- smootheddf[,-3]
###Multiplicative
#lines(smootheddf)
plot(smootheddf,type="line")
###Degree 2 polynomial
lmfit <- lm(Qty~ (sin(0.5*Month) *poly(Month,2) + cos(0.5*Month)) * poly(Month,2), data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='green', lwd=3)
###Degree 3 polynomial
lmfit <- lm(Qty~ (sin(0.5*Month) *poly(Month,3) + cos(0.5*Month)) * poly(Month,3), data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='orange', lwd=3)
### Degree 3 polynomial with 0.8 coeff
lmfit <- lm(Qty~ (sin(0.8*Month) *poly(Month,3) + cos(0.8*Month)) * poly(Month,3), data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='blue', lwd=3)
###Additive version
lmfit <- lm(Qty~ (sin(0.8*Month) + poly(Month,3) + cos(0.8*Month)) , data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='purple', lwd=3)


##Multiplicative degree 3 polynomial  with 0.5 seems best

plot(smootheddf,type="line")
lmfit <- lm(Qty~ (sin(0.5*Month) *poly(Month,3) + cos(0.5*Month)) * poly(Month,3), data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='brown', lwd=3)

#Locally predictable series
#We will model it as an ARMA series

local_pred <- timeser_top_qty_train - global_pred
plot(local_pred, col='red', type = "l")
###ACF and PACF
acf(local_pred)
acf(local_pred, type="partial")
###ACF 
armafit <- auto.arima(local_pred)
armafit
tsdiag(armafit)
#ARIMA(0,0,0)
#log likelihood=-265.74
#AIC=533.75   AICc=533.85   BIC=535.49
#Since it is ARIMA(0,0,0), there is no auto regressive component

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
#P=0.07182
kpss.test(resi)
#P=0.1
###the residue is white noise

#Modeling the TS

outdata <- timeser_top_qty_test
timevals_out <- 43:48


global_pred_out <- predict(lmfit,data.frame(Month=timevals_out))

fcast <- global_pred_out
plot(fcast,type="l")

##Forcasting for next 6 months using CD model
timevals_out <- 49:54
global_pred_out <- predict(lmfit,data.frame(Month=timevals_out))
View(global_pred_out)

#Now, let's compare our prediction with the actual values, using MAPE


MAPE_class_dec <- accuracy(fcast,timeser_top_qty_test)[5]

MAPE_class_dec
## 32.79516

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(c(timeser_top_qty_train,timeser_top_qty_test), col = "black",type= "both")
lines(class_dec_pred, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

#autoarima <- auto.arima(timeser_top_qty)
autoarima <- auto.arima(timeser_top_qty_train)

autoarima
##ARIMA(2,1,0) 
#llog likelihood=-261.9
#AIC=529.8   AICc=530.44   BIC=534.94
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

#resi_auto_arima <- timeser_top_qty - fitted(autoarima)
resi_auto_arima <- timeser_top_qty_train - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
#p=0.07718 
kpss.test(resi_auto_arima)
#p = 0.1 so we cannot reject the null hypothesis that the residual time series is stationary

#comparing prediction with the actual values, using MAPE


#MAPE_class_dec <- accuracy(fitted(autoarima),timeser_top_qty)[5]
MAPE_class_dec <- accuracy(fitted(autoarima),timeser_top_qty_train)[5]

MAPE_class_dec
## 29.78408
#  31.5688 is the MAPE value we get with the train data set

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

plot(timeser_top_qty, col = "black",type= "both")
lines(fitted(autoarima), col = "red")

##Forcasting for next 12(43-54) months using ARIMA model
global_pred_out <- predict(autoarima,n.ahead=12)
global_pred_out

##### Modelling the APAC_Consumer Sales ######################


####Trying Holt Winter's method for smoothing
plot(timeser_second_top_sales_train)

cols <- c("orange", "blue", "green", "yellow")
alphas <- c(0.1,0.2,0.3,0.5)
#alphas <- c(0.8)
labels <- c(paste("alpha =", alphas))
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(timeser_second_top_sales_train, alpha=alphas[i],
                               beta=FALSE, gamma=FALSE)

  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}

legend("topleft", labels, col=cols, lwd=2)
#####Chosing alpha=0.3

smoothedseries <- HoltWinters(timeser_second_top_sales_train, alpha=0.3,
                              beta=FALSE, gamma=FALSE)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

timevals_in <- 1:42

smootheddf <- as.data.frame(cbind (timevals_in,rbind(timeser_second_top_sales_train[1],fitted(smoothedseries))))

colnames(smootheddf) <- c("Month","Sales")
smootheddf <- smootheddf[,-3]
###Multiplicative
plot(smootheddf,type="line")
###Degree 2 polynomial
lmfit <- lm(Sales~ (sin(0.5*Month) *poly(Month,2) + cos(0.5*Month)) * poly(Month,2), data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='green', lwd=3)
###Degree 3 polynomial
lmfit <- lm(Sales~ (sin(0.5*Month) *poly(Month,3) + cos(0.5*Month)) * poly(Month,3), data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='orange', lwd=3)
### Degree 3 polynomial with 0.8 coeff
lmfit <- lm(Sales~ (sin(0.8*Month) *poly(Month,3) + cos(0.8*Month)) * poly(Month,3), data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=3)

### Degree 2 polynomial with 0.8 coeff
lmfit <- lm(Sales~ (sin(0.8*Month) *poly(Month,2) + cos(0.8*Month)) * poly(Month,2), data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='purple', lwd=3)


##Multiplicative degree 2 polynomial  with 0.5 seems best

plot(smootheddf,type="line")
lmfit <- lm(Sales~ (sin(0.5*Month) *poly(Month,2) + cos(0.5*Month)) * poly(Month,2), data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='brown', lwd=3)

#Locally predictable series
#We will model it as an ARMA series

local_pred <- timeser_second_top_sales_train - global_pred
plot(local_pred, col='red', type = "l")
###ACF and PACF
acf(local_pred)
acf(local_pred, type="partial")
###ACF
armafit <- auto.arima(local_pred)
armafit
tsdiag(armafit)
#ARIMA(0,0,0)
#log likelihood=-453.32
#AIC=908.65   AICc=908.75   BIC=910.38
#Since it is ARIMA(0,0,0), there is no auto regressive component

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
#P=0.01
kpss.test(resi)
#P=0.1
###the residue is white noise

#Modeling the TS

outdata <- timeser_second_top_sales_test
timevals_out <- 43:48


global_pred_out <- predict(lmfit,data.frame(Month=timevals_out))

fcast <- global_pred_out
plot(fcast,type="l")

#Now, let's compare our prediction with the actual values, using MAPE


MAPE_class_dec <- accuracy(fcast,timeser_second_top_sales_test)[5]

MAPE_class_dec
## 20.62275

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(c(timeser_second_top_sales_train,timeser_second_top_sales_test), col = "black",type= "both")
lines(class_dec_pred, col = "red")


##Forcasting for next 6 months using CD model
timevals_out <- 49:54
global_pred_out <- predict(lmfit,data.frame(Month=timevals_out))
View(global_pred_out)


#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(timeser_second_top_sales)
autoarima
##ARIMA(0,1,1) 
#log likelihood=-515.41
#AIC=1034.83   AICc=1035.1   BIC=1038.53
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser_top_sales - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
#p=0.1128  series is not stationary
kpss.test(resi_auto_arima)
#p = 0.1 so we cannot reject the null hypothesis that the residual time series is stationary
##giving precedence to KPSS test, we can say it is stationary
#comparing prediction with the actual values, using MAPE


MAPE_class_dec <- accuracy(fitted(autoarima),timeser_top_sales)[5]

MAPE_class_dec
## 45.53
### Classical decomposition gives a better model with a better MAPE=20.62275
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

plot(timeser_top_sales, col = "black",type= "both")
lines(fitted(autoarima), col = "red")

####Modelling the APAC Consumer segment quantities

plot(timeser_second_top_qty_train)

####Trying Holt Winter's method for smoothing


cols <- c("orange", "blue", "green", "yellow")
alphas <- c(0.1,0.2,0.3,0.5)
#alphas <- c(0.8)
labels <- c(paste("alpha =", alphas))
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(timeser_second_top_qty_train, alpha=alphas[i],
                                beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}

legend("topleft", labels, col=cols, lwd=2)
#####Chosing alpha=0.3

smoothedseries <- HoltWinters(timeser_second_top_qty_train, alpha=0.3,
                              beta=FALSE, gamma=FALSE)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

timevals_in <- 1:42

smootheddf <- as.data.frame(cbind (timevals_in,rbind(timeser_second_top_qty_train[1],fitted(smoothedseries))))

colnames(smootheddf) <- c("Month","Qty")
smootheddf <- smootheddf[,-3]
###Multiplicative
#lines(smootheddf)
plot(smootheddf,type="line")
###Degree 2 polynomial
lmfit <- lm(Qty~ (sin(0.5*Month) *poly(Month,2) + cos(0.5*Month)) * poly(Month,2), data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='green', lwd=3)
###Degree 3 polynomial
lmfit <- lm(Qty~ (sin(0.5*Month) *poly(Month,3) + cos(0.5*Month)) * poly(Month,3), data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='orange', lwd=3)
### Degree 3 polynomial with 0.8 coeff
lmfit <- lm(Qty~ (sin(0.8*Month) *poly(Month,3) + cos(0.8*Month)) * poly(Month,3), data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='blue', lwd=3)
### Degree 2 polynomial with 0.8
lmfit <- lm(Qty~ (sin(0.8*Month) *poly(Month,2) + cos(0.8*Month)) * poly(Month,2), data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='purple', lwd=3)
###Additive version
lmfit <- lm(Qty~ (sin(0.8*Month) + poly(Month,3) + cos(0.8*Month)) , data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='purple', lwd=3)


##Multiplicative degree 2 polynomial  with 0.5 seems best

plot(smootheddf,type="line")
lmfit <- lm(Qty~ (sin(0.5*Month) *poly(Month,2) + cos(0.5*Month)) * poly(Month,2), data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='brown', lwd=3)

#Locally predictable series
#We will model it as an ARMA series

local_pred <- timeser_second_top_qty_train - global_pred
plot(local_pred, col='red', type = "l")
###ACF and PACF
acf(local_pred)
acf(local_pred, type="partial")
###ACF 
armafit <- auto.arima(local_pred)
armafit
tsdiag(armafit)
#ARIMA(0,0,0)
#log likelihood=-265.74
#AIC=533.48   AICc=533.58   BIC=535.22
#Since it is ARIMA(0,0,0), there is no auto regressive component

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
#P=0.07232
kpss.test(resi)
#P=0.1
###the residue is white noise

#Modeling the TS

outdata <- timeser_second_top_qty_test
timevals_out <- 43:48


global_pred_out <- predict(lmfit,data.frame(Month=timevals_out))

fcast <- global_pred_out
plot(fcast,type="l")

#Now, let's compare our prediction with the actual values, using MAPE


MAPE_class_dec <- accuracy(fcast,timeser_second_top_qty_test)[5]

MAPE_class_dec
## 20.38749

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(c(timeser_second_top_qty_train,timeser_second_top_qty_test), col = "black",type= "both")
lines(class_dec_pred, col = "red")

##Forcasting for next 6 months using CD model
timevals_out <- 49:54
global_pred_out <- predict(lmfit,data.frame(Month=timevals_out))
View(global_pred_out)



#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(timeser_second_top_qty)
autoarima
##ARIMA(0,1,1) 
#log likelihood=-303.76
#AIC=611.53   AICc=611.8   BIC=615.23
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser_second_top_qty - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
#p=0.01
kpss.test(resi_auto_arima)
#p = 0.1 so we cannot reject the null hypothesis that the residual time series is stationary

#comparing prediction with the actual values, using MAPE


MAPE_class_dec <- accuracy(fitted(autoarima),timeser_second_top_qty)[5]

MAPE_class_dec
## 33.9185
### The classical decomposition MAPE was 20.38749
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

plot(timeser_second_top_qty, col = "black",type= "both")
lines(fitted(autoarima), col = "red")










