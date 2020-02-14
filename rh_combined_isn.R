

# Initialization
library(MASS)
library(stats)   # location of the time series modules
library(graphics)
library(lattice)

# put the data in R (user), 
setwd("~howe/DeskTop/")
WD <- getwd()

###   Specify the name and address of the data file,   ####
###   read it in, save it as RData

infile <- paste(WD, "combined_isn.csv", sep="/")
CR <- data.frame(read.csv(infile, header=TRUE))
outfile <- paste(WD, "ISN_.RData", sep="/")
save(CR, file=outfile, ascii=FALSE)

summary(CR)

###   Set as time series   #################################
tmp <- ts(CR)
tsp(tmp)

colMeans(tmp)
      sd(tmp)
apply(tmp,2,min)
apply(tmp,2,max)
plot.ts(tmp, main="Averages Output ", xlab="Time", ylab="Count Averages Output", ylim(0,5))
x11()
plot.ts(tmp[,2:4], plot.type="single", col=c(2,3,4), main="Averages Output", xlab="g green, s red R blue", ylab="Counts")
x11()
acf(tmp[,2], main="g")
x11()
acf(tmp[,3], main="s")
x11()
acf(tmp[,4], main="R")
x11()

#ts.acf  <- acf(tmp, plot=F)
#ts.pacf <- acf(tmp, type="partial", plot=F)

# ###   Time series analysis   ###############################
# ###   acf and pacf   #######################################

#acf(tmp)
#acf(tmp, type="partial")
pairs(tmp,panel=panel.smooth,  lwd=2)
x11()
pairs(tmp[,2:4],panel=panel.smooth,  lwd=2)
x11()

###   Autocorrelation analysis - first diff  ###############
#tmp <- diff(CR)
###   Test for normal distributions   ######################

colMeans(tmp)
      sd(tmp)
      sd(tmp)*sd(tmp)
apply(tmp,2,min)
apply(tmp,2,max)

wilcox.test(tmp[,2])
wilcox.test(tmp[,3])
wilcox.test(tmp[,4])

t.test(tmp[,2],tmp[,3])
t.test(tmp[,2],tmp[,4])
t.test(tmp[,3],tmp[,4])
t.test(tmp[,3],tmp[,4],alternative="greater",var.equal=F)

cor.test(tmp[,2],tmp[,3])
cor.test(tmp[,2],tmp[,4])
cor.test(tmp[,3],tmp[,4])

ks.test(tmp[,2],tmp[,3])
ks.test(tmp[,2],tmp[,4])
ks.test(tmp[,3],tmp[,4])


plot(tmp[,2],type="l")
x11()
tmp.hist <- hist(tmp[,2],prob=T,col="red",main="g Distribution",sub="Fitted n(min, max)",xlab="First Difference")
lines(seq(min(tmp[,2]),max(tmp[,3]),100), dnorm(seq(min(tmp[,2]),max(tmp[,2]),100), mean=mean(tmp[,2]), sd=sd(tmp[,2])), lwd=2)
x11()
plot(tmp[,3],type="l")
x11()
tmp.hist <- hist(tmp[,3],prob=T,col="red",main="s Distribution",sub="Fitted n(min, max)",xlab="First Difference")
lines(seq(min(tmp[,3]),max(tmp[,3]),100), dnorm(seq(min(tmp[,3]),max(tmp[,3]),100), mean=mean(tmp[,3]), sd=sd(tmp[,3])), lwd=2)
x11()
plot(tmp[,4],type="l")
x11()
tmp.hist <- hist(tmp[,4],prob=T,col="red",main="R Distribution",sub="Fitted n(min, max)",xlab="First Difference")
lines(seq(min(tmp[,4]),max(tmp[,3]),100), dnorm(seq(min(tmp[,4]),max(tmp[,4]),100), mean=mean(tmp[,4]), sd=sd(tmp[,4])), lwd=2)
x11()

###   Autocorrelations   ###################################
plot.ts(tmp, main="Ymd by s, R", xlab="Ymd", ylab="Ymd")
x11()
plot.ts(tmp[,2], plot.type="single", col=c(1,2), main="Days by g", xlab="Days", ylab="g")
x11()
plot.ts(tmp[,3], plot.type="single", col=c(1,3), main="Days by s", xlab="Days", ylab="s")
x11()
plot.ts(tmp[,4], plot.type="single", col=c(1,4), main="Days by R", xlab="Days", ylab="R")
x11()

###   arima analysis (d=1)  ##############################

(ch0 <- arima(tmp[,2],order=c(0,0,1)))
cpgram(ch0$resid, main="s  ARIMA(0,1,1) Residuals")
x11()
tsdiag(ch0)
x11()
(ch1 <- arima(tmp[,3],order=c(0,0,2)))
cpgram(ch1$resid, main="R  ARIMA(0,1,3) Residuals")
x11()
tsdiag(ch1)
x11()
(ch2 <- arima(tmp[,4],order=c(0,0,3)))
cpgram(ch2$resid, main="Difference  ARIMA(1,1,1) Residuals")
x11()
tsdiag(ch2)
x11()

###   Cross-correlation analysis - first diff  ###########

pairs(tmp,panel=panel.smooth, main="Pairwise Scatterplots with Loess Smoothing")
x11()
pairs(tmp[,2:4],panel=panel.smooth, main="Pairwise Scatterplots with Loess Smoothing")
x11()

##########################################################



