## Aurhor: Ben Letham (bletham@fb.com)
## Organization: Facebook
## Package: Prophet( Michine Learning)
## Data: http://aavso.org/solar/
## Date: 2019-10-31
## Purpose: To combine Sunspots data and R + Prophet(ML) to predict Sunspots/Miniumim activity.
## Challenge: Was to take into account 11 year solor mim/max cycles
## Documentions: 
## https://facebook.github.io/prophet/docs/seasonality,_holiday_effects,_and_regressors.html#specifying-custom-seasonalities

library(tidyverse)
library(data.table)
library(xts)
library(prophet)
library(plotly)

## 
rm(list=ls())
##
## Set Working Directory
# setwd('c:/Users/Howe/Desktop/')

## Download latest data from aavso
aavso <-fread("./aavso_2019.csv")
# aavso <-fread("https://www.aavso.org/sites/default/files/solar/NOAAfiles/NOAAdaily.csv")
# colnames(aavso) <- c("JD","Year","Month","Day","n_g", "n_s","Wn", "s_g","s_s" ,"Ws", "Wolf" )

aavso$Ymd <- as.Date(paste(aavso$Year, aavso$Month, aavso$Day, sep = "-"))
aavso<-aavso[Ymd>="2012-01-01",.(Ymd,Wolf),]
df <- aavso
colnames(df) <- c("ds", "y"  )
summary(df)

##
## Beginning of Ben's Prophet code
##
m <- prophet(seasonality.mode="multiplicative")
m <- add_seasonality(m, name="cycle_11year", period=365.25 * 11,fourier.order=5)
m <- fit.prophet(m, df)
future <- make_future_dataframe(m,periods=4000,freq="day")
forecast <- predict(m, future)
plot(m,forecast) +ggtitle("AAVSO Sunspot Predictions:2012 - 2025")
##
## Subplot of forecast table: 2014 - 2025
forecast1 <- as.data.table(forecast)
forecast1 <- forecast1[ds >="2012-01-01",]
x11()
ggplot(data=forecast1,aes(x=ds,y=yhat)) + geom_line() + geom_smooth() + 
  ggtitle("AAVSO Sunspot Prediction: 2012 - 2025")
## Subplot of forecast table: 2019 - 2025

forecast2 <- forecast1[ds >="2019-01-01",]
 ggplot(data=forecast2,aes(x=ds,y=yhat)) + geom_line() + geom_smooth() + 
  ggtitle("AAVSO Sunspot Prediction: 2019 - 2025")
## Predict for 12 months beginning with Apr. 2019
forecast2 <- forecast1[ds >="2019-01-01" & ds <="2021-05-31",]
 ggplot(data=forecast2,aes(x=ds,y=yhat)) + geom_line() + geom_smooth() + 
  ggtitle("AAVSO Sunspot Prediction: 2019 - 2021")


aavso1<-aavso[Ymd>="2012-01-01",.(Ymd,Wolf),]
aavso1$Vote <- ifelse(aavso1$Wolf ==0,0,1)
Fit <- as.data.frame(lowess(aavso1$Wolf,f=0.3))
aavso1 <-cbind(aavso1,Fit$y)
colnames(aavso1) <-c("Ymd","Wolf","Vote","Loess")
str(aavso1)
summary(aavso1)
##

## Create daily summary (Vote) field with XTS
##
isn.xts <- xts(x = aavso1$Vote , order.by = aavso1$Ymd)
isn.monthly <- apply.monthly(isn.xts, sum)
isn <-as.data.table(isn.monthly)
colnames(isn) <- c("Ymd","Days")
S3 <- isn %>% filter(Ymd >="2019-01-01")
ggplot(data=S3,aes(x=Ymd,y=Days)) +geom_line() +geom_smooth(method="loess",col="blue") + 
  ggtitle(" XTS: Monthy Days with Spots: 2012 - 2019")
##
## Prophet prediction based on Daily Vote Field
##
df <- S3 %>% select(Ymd,Days)
colnames(df) <- c("ds","y")
m <- prophet(seasonality.mode="multiplicative")
#m <- add_seasonality(m, name="cycle_11year", period=364.25 * 11,fourier.order=5)
m <- add_seasonality(m, name="Carrington R", period=27.25,fourier.order=5)

m <- fit.prophet(m, df)
future <- make_future_dataframe(m,periods=2000,freq="day")
forecast <- predict(m, future)
plot(m, forecast) +ggtitle("AAVSO CR Votes: Jan. 2012 - Oct. 2019") +ylab("Predicted Days w/ Spots") +
  xlab("Carrington R" )
##
##
S4 <- filter(forecast,ds >="2020-01-01" & ds <="2026-12-31")
ggplot(data=S4,aes(x=ds,y=yhat_upper,col="Upper")) +geom_col() +
  geom_col(data=S4,aes(x=ds,y=yhat,col="Predict")) +
  geom_col(data=S4,aes(x=ds,y=yhat_lower,col="Lower")) +
  xlab("Date: Days and Year") + ylab("Days Per CR") + 
  geom_smooth(data=S4,aes(x=ds,y=yhat,col="Loess"))
##