library(fBasics)
library(fUnitRoots)
library(TSA)
library(ggplot2)
#source("Rprogs.R")
library(forecast)

# Import the necessary packages and set
# random seed for replication consistency
if( !require('depmixS4')) {
  install.packages('depmixS4')
  library('depmixS4')
}
if( !require('quantmod')) {
  install.packages('quantmod')
  library('quantmod')
}


# this function finds the strongest seasonal period
# must also check for stationarity first and do a difference if necessary
# xx is raw time series data (not a ts object)
findPeriod = function(xx,d=0) {
  
  #must make series stationary before finding period
  xx_d = xx
  if( d > 0) {
    xx_d = diff(xx,d)
  }
  
  #find a seasonal period
  pp = periodogram(xx_d,plot=FALSE)
  
  dd = data.frame(freq=pp$freq, spec=pp$spec)
  topfreqs = dd[order(-dd$spec),]
  ppd = 1/topfreqs$freq
  ppd = round(ppd)

  ppdf = data.frame(period=ppd,spec=topfreqs$spec)
  return(ppdf)
}

#these take too long...
if(0) {
ap = aovper(t,x)
pp = peaks(ap)
pp[1:3,]
pk1 = peak1(pp); pk1
period = pk1$per; period

#period = 26.9
dc = dcdft(t,x,0.01,.2)
pp = peaks(dc)
pp[1:3,]
peak1(dc)
}


## plotting
plot_raw = function(mm,dates,...) {
  
  xt = time(mm$ts)
  
  #have at most 10 ticks on x axis
  by=floor(length(xt)/10)
  pp = unique(c(1,seq(from=1,to=length(xt),by=by)))
  
  plot(mm$ts, type='l', xaxt='n',...)
  axis(1, at = xt[pp], labels=dates[pp], las=2,...)
}

plot_fit = function(mm,dates,first=0,last=0,...){

  if( last > 0 ) {
    mm$x = tail(mm$x,last)
    mm$fitted = tail(mm$fitted,last)
  }
  if(first > 0 ) {
    mm$x = head(mm$x,first)
    mm$fitted = head(mm$fitted,first)
  }
  xt = time(mm$x)  
  #have at most 10 ticks on x axis
  by=floor(length(xt)/10)
  pp = unique(c(1,seq(from=1,to=length(xt),by=by)))
  
  plot(mm$x,xaxt='n',xlab="", ...)
  lines(mm$fitted, col='red',lty='dashed',...)
  axis(1, at = xt[pp], labels=dates[pp], las=2,...)
}

plot_forecast = function(mm,ff,dates,...) {
  
  xt = xticks(dates,mm)
  all = date_ticks(dates,mm)
  
  #have at most 10 ticks on x axis
  by=floor(length(xt)/10)
  pp = unique(c(1,seq(from=1,to=length(xt),by=by)))
  
  plot(ff,xaxt='n',...)
  lines(ff$fitted, col='red',lty='dashed',...)
  axis(1, at = xt[pp], labels=all[pp], las=2,...)
}

plot_ts = function(x, tdx,...) {
  
  #have at most 10 ticks on x axis
  xt = time(x)
  by=floor(length(xt)/10)
  pp = unique(c(1,seq(from=1,to=length(xt),by=by)))
  
  plot(x, xlab="",ylab="", xaxt='n',...)
  axis(1, at = xt[pp], labels=as.character(tdx[pp]), las=2)
}

plot_decomp <-function(x, tdx, dd,...)
{
  xt = time(x)
  by=floor(length(xt)/10)
  pp = unique(c(1,seq(from=1,to=length(xt),by=by)))
  #plot(x=tdx,y=data, type="l", xlab="", ylab="",...)
  plot(x,xlab="",ylab="", xaxt='n')
  
  lines(dd$time.series[,2],col="red")
  lines(dd$time.series[,1]+mean(x),col="blue")
  lines(dd$time.series[,3]+mean(x),col="grey")
  legend("topleft", lty=1, col=c("black","blue","red","grey"), 
         c("Data","Seasonal Pattern", "Trend","Remainder"))
  axis(1, at = xt[pp], labels=as.character(tdx[pp]), las=2)
}

plot_acf_pacf <- function(x,...) {
  lag = frequency(x)*2
  lag = max(lag,10*log(length(x)))
  z = par()
  par(mfrow=c(2,1))
  mar = z$mar
  mar[1]=2.1
  mar[3]=1.1
  par(mar=mar)
  acf(x,lag.max=lag,xlab="",main="")
  mar[1]=2.1
  mar[3]=1.1
  par(mar=mar)
  pacf(x,lag.max=lag,xlab="",main="")
  par(mar=z$mar)
  par(mfrow=c(1,1))  
}


plot_dist <- function(x) {
  par(mfrow=c(1,3))
  boxplot(x,main="Quartiles")
  d1=density(x)
  plot(d1$x,d1$y,type='l',xlab="",ylab="",main="Density")
  qqnorm(x,xlab="")
  qqline(x,col='red')
  par(mfrow=c(1,1))
}

plot_hist <- function(x) {
  #x <- X$w
  X = data.frame(w=x)
  hist(x,breaks="FD",plot=F)
  breaks <- pretty(range(x), n = nclass.FD(x), min.n = 1)
  bwidth <- breaks[2]-breaks[1]
  xl <- "Wolf Number"

  #plotting a negative binomial distribution isn't working  
  mu = mean(x)
  sigma2 = var(x)
  p = (sigma2-mu)/sigma2
  r = sigma2/(sigma2-mu)

  #where did these come from?
  Ex=""
  WD=""
  ver=""
  #loc = "plots.png"
  
  #(main <- paste0(Ex," v", ver, " Wolf Number Histogram"))
  #(loc <- paste0(WD,"/Plots/",Ex,ver,part,ilk,".png"))
  main = "Wolf Number Histogram"
  gp <- ggplot(X, aes(w)) + 
    geom_histogram(aes(y=..density..),col='grey45',binwidth=bwidth) + 
    #    geom_density(aes(y=0.045*..count..), colour="black", adjust=4) +
    #    geom_density() +
    scale_x_continuous() +
    scale_y_continuous() +
    stat_function(
      aes(color = "Normal"), 
      fun = dnorm, 
      args = with(X, c(mean = mean(w), sd = sd(w))),
      size=1 
    ) +    
    geom_point(aes(color = "Poisson", y=dpois(X$w, mean(X$w))), size=1) +
    #geom_point(aes(color = "Negative Binomial", y=dbinom(X$w, size=1000, prob=1-p)),size=1) +
    
    #scale_colour_manual("Density", values = c("black", "red", "blue")) +
    scale_colour_manual("Density", values = c("black", "red")) +
    ggtitle(main) + 
    xlab(xl) + 
    ylab("Frequency") +
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(hjust = 0.5)) #+
    #ggsave(loc)
  plot(gp)
}

plot_hist_diff <- function(x) {
  #x <- X$w
  X = data.frame(w=x)
  hist(x,breaks="FD",plot=F)
  breaks <- pretty(range(x), n = nclass.FD(x), min.n = 1)
  bwidth <- breaks[2]-breaks[1]
  xl <- "Wolf Number"
  
  #plotting a negative binomial distribution isn't working  
  mu = mean(x)
  sigma2 = var(x)
  p = (sigma2-mu)/sigma2
  r = sigma2/(sigma2-mu)
  
  #where did these come from?
  Ex=""
  WD=""
  ver=""
  #loc = "plots.png"
  
  #(main <- paste0(Ex," v", ver, " Wolf Number Histogram"))
  #(loc <- paste0(WD,"/Plots/",Ex,ver,part,ilk,".png"))
  main = "Wolf Number Histogram"
  gp <- ggplot(X, aes(w)) + 
    geom_histogram(aes(y=..density..),col='grey45',binwidth=bwidth) + 
    #    geom_density(aes(y=0.045*..count..), colour="black", adjust=4) +
    #    geom_density() +
    scale_x_continuous() +
    scale_y_continuous() +
    stat_function(
      aes(color = "Normal"), 
      fun = dnorm, 
      args = with(X, c(mean = mean(w), sd = sd(w))),
      size=1 
    ) +    
    #geom_point(aes(color = "Poisson", y=dpois(X$w, mean(X$w))), size=1) +
    #geom_point(aes(color = "Negative Binomial", y=dbinom(X$w, size=1000, prob=1-p)),size=1) +
    
    #scale_colour_manual("Density", values = c("black", "red", "blue")) +
    scale_colour_manual("Density", values = c("black", "red")) +
    ggtitle(main) + 
    xlab(xl) + 
    ylab("Frequency") +
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(hjust = 0.5)) #+
  #ggsave(loc)
  plot(gp)
}

#other distributions
#family(object, ...)

#binomial(link = "logit")
#gaussian(link = "identity")
#Gamma(link = "inverse")
#inverse.gaussian(link = "1/mu^2")
#poisson(link = "log")
#quasi(link = "identity", variance = "constant")
#quasibinomial(link = "logit")
#quasipoisson(link = "log")


summary_table <- function(x,main) {
  z = basicStats(x)
  names(z) = main
  
  #only keep some of the stats
  keep = c("Minimum", "Maximum", "Mean", "LCL Mean", "UCL Mean", "Variance", "Stdev", "Skewness", "Kurtosis")
  k = data.frame(z[keep,])
  names(k)=main
  row.names(k)=keep
  
  df = data.frame(row.names(k),k)
  names(df) = c("Statistic", "Value")
  row.names(df)=NULL
  return(df)
}


skewnessTest<-function(data) 
{
  rc = list()
  sk = timeDate::skewness(data)
  T = length(data)
  tm3=sk/sqrt(6/T)
  p = 2*(1-pnorm(abs(tm3)))
  
  rc$skewness = sk
  rc$T = T
  rc$t.val = tm3
  rc$p.val = p
  return(rc)
}

kurtosisTest<-function(data,k=NA)
{
  rc = list()
  if( is.na(k) ){
    k = timeDate::kurtosis(data)
  }
  T = length(data)
  tk=k/sqrt(24/T)
  p = 2*(1-pnorm(abs(tk)))
  
  rc$kurtosis = k
  rc$T = T
  rc$t.val = tk
  rc$p.val = p
  return(rc)
}

#test for constant mean by splitting into 2 samples and testing means are equal
meanTest<-function(data)
{
  T = length(data)
  midpt = T/2
  
  d1 = data[1:midpt]
  d2 = data[midpt:length(data)]
  
  rc = t.test(d1,d2)
  
  return(rc)
}

varianceTest<-function(data)
{
  #split data into 2 groups, get variance of each, conduct F test of ratio
  #degress of freedom = num-1, denom-1
  
  T = length(data)
  #m = median(data)
  #d1 = data[data < m]
  #d2 = data[data >= m]
  
  midpt = T/2
  d1 = data[1:midpt]
  d2 = data[midpt:length(data)]
  
  s1 = var(d1)
  df1 = length(d1)-1
  
  s2 = var(d2)
  df2 = length(d2)-1
  
  #want largest value in numerator so F is an upper tail test
  if( s1>s2) {
    num=s1
    numdf=df1
    denom=s2
    denomdf=df2
  } else {
    num=s2
    numdf=df2
    denom=s1
    denomdf=df1
  }
  
  F = num/denom
  p = 1-pf(F,numdf,denomdf)
  #two tailed test
  p = p*2   
  
  rc = list()
  rc$T = T
  rc$F = F
  rc$p.value = p
  
  return(rc)
}


#conduct hypothesis tests
tests = function(x) {
  
  #normality tests
  z = t.test(x)
  t1 = z$p.value
  
  z = meanTest(x)
  t2 = z$p.value
  
  z = varianceTest(x)
  t3 = z$p.value
  
  z = skewnessTest(x)
  t4 = z$p.val
  
  z = kurtosisTest(x)
  t5 = z$p.val
  
  #length(x) must be <= 5000
  #z = shapiro.test(x)
  #t6 = z$p.value  

  #find order for adf test
  o = tryCatch({
    order = ar(diff(x))
    o = order$order
  },
  error = function(x){
    o = 12
  })
  
  #unit root test  (Dickey-Fuller test)
  #type='c' is chosen because the mean is not 0 (has an intercept) but does not appear to have a trend
  z = suppressWarnings(Box.test(x,lag=o,type='Ljung'))
  t7 = z$p.value
  
  #what adf test type?
  
  #default to no intercept
  type = "nc"
  
  if( t1 <= .05 ) {
    #non zero mean (has an intercept)
    type = "c"  
  }
  
  if( t2 <= .05) {
    #also has a trend
    type = "ct"
  }
  
  z = suppressWarnings(adfTest(x,lags=o,type=type))
  t8 = z@test$p.value
  
  #build table of results
  signif(t8,3)
  
#  results=c(t1,t2,t3,t4,t5,t6,t7,t8)
  results=c(t1,t2,t3,t4,t5,t7,t8)
  results = unlist(lapply(results,function(x){floor(x*1000)/1000}))
  test = c(
    "Mean = 0",
    "Constant Mean",
    "Constant Variance",
    "Skeweness = 0",
    "Excess Kurtosis = 0",
#    "Shapiro-Wilk Test of Normality",
    "Box-Ljung Test of no Autocorrelation",
    "Augmented Dickey-Fuller Test of Unit Roots"
  )
  
  result_desc = unlist(lapply(results, function(x) {
    ifelse(x <= .05, "Rejected", "Not Rejected")
  }))
  
  df = data.frame(test, results, result_desc)
  names(df) = c("Test Description", "P Value", "At 5% Significance Level")
  return(df)
}

trim_window = function(x,start,end) {
  UseMethod("trim_window",x)
}

trim_window.default = function(x,start,end) {
  xx = x[start:end]
  tx = time(x)
  tstart = tx[start]
  tsx = ts(xx,start=tstart,frequency=1)
  return(tsx)
}

trim_window.ts = function(x,start,end) {
  xx = x[start:end]
  tx = time(x)
  tstart = tx[start]
  tsx = ts(xx,start=tstart,frequency=frequency(x))
  return(tsx)
}

trim_window.msts = function(x,start,end) {
  p = attr(x,"msts")
  xx = x[start:end]
  tx = time(x)
  tstart = tx[start]
  tsx = msts(xx,start=tstart, seasonal.periods = p)
  return(tsx)
}


