library(tidyverse)
library(data.table)
library(xts)
library(prophet)
library(plotly)
rm(list=ls())

sidc <-fread("http://sidc.be/silso/DATA/SN_d_tot_V2.0.csv",sep = ';')
colnames(sidc) <- c("Year","Month","Day", "Fdate","Spots", "Sd","Obs" ,"Defin"  )
sidc$Ymd <- as.Date(paste(sidc$Year, sidc$Month, sidc$Day, sep = "-"))
sidc1<-sidc [Ymd>="1853-11-09",.(Ymd,Spots),]
sidc1$Vote <- ifelse(sidc1$Spots ==0,0,1)
df <- sidc1 %>% select(Ymd,Spots)
colnames(df) <- c("ds", "y"  )

##
m <- prophet(seasonality.mode="multiplicative")
m <- add_seasonality(m, name="cycle_11year", period=365.25 * 11,fourier.order=5)
m <- fit.prophet(m, df)
future <- make_future_dataframe(m,periods=8000,freq="day")
forecast <- predict(m, future)
plot(m,forecast) +ggtitle("AAVSO Sunspot Predictions:1853 - 2025")
##
##
forecast %>% filter(ds >="2012-01-01") %>%
ggplot(aes(x=ds,y=yhat)) +geom_line() + 
  geom_smooth()
##
df2 <- sidc1 %>% select(Ymd,Spots) %>% filter(Ymd >="2012-01-01")
colnames(df2) <- c("ds", "y"  )
m <- prophet(seasonality.mode="multiplicative")
m <- add_seasonality(m, name="cycle_11year", period=365.25 * 11,fourier.order=5)
m <- fit.prophet(m, df2)
future <- make_future_dataframe(m,periods=4000,freq="day")
forecast <- predict(m, future)
plot(m,forecast) +ggtitle("AAVSO Sunspot Predictions:2012 - 2030")
##
##


##
df1 <- sidc1 %>% select(Ymd,Spots) %>% filter(Ymd >="2012-01-01")
colnames(df1) <- c("ds", "y"  )
summary(df1)
m <- prophet(seasonality.mode="multiplicative")
m <- add_seasonality(m, name="Carrington R", period=27.25,fourier.order=5)
m <- fit.prophet(m, df1)
future <- make_future_dataframe(m,periods=4000,freq="day")
forecast <- predict(m, future)
plot(m, forecast) +ggtitle("AAVSO CR Votes: 2012 - Oct. 2019") +ylab("Predicted Days w/ Spots") +
  xlab("Carrington R" )
##