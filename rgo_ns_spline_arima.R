#  Modified _ns rhowe to work with _ns data 
#  http://stat.ethz.ch/R-manual/R-patched/library/stats/html/smooth.spline.html

#  Modified _ns rhowe to work with _ns data 
#  http://stat.ethz.ch/R-manual/R-patched/library/stats/html/smooth.spline.html

# Run a time series analysis 2 columns of (Obs,k) data
# Initialization
library(MASS)
library(stats)   # location of the time series modules
library(graphics)
library(lattice)
library(rgl)


# put the data sunspot_jan-nov_arima.txt in R (user), 
setwd("./")
WD <- getwd()

###   Specify the name and address of the data file,   ####
###   read it in, save it as RData

infile <- paste(WD, "rgo_N_S_CR.csv", sep="/")
nsRGO <- data.frame(read.csv(infile, header=TRUE))
outfile <- paste(WD, "nsRGO.RData", sep="/")
save(nsRGO, file=outfile, ascii=FALSE)

summary(nsRGO)

#require(graphics)
attach(nsRGO)
plot(CR_S, S, main = "RGO S smoothing splines")
ns.spl <- smooth.spline(CR_S, S)
(ns.spl)
## This example has duplicate points, so avoid cv = TRUE

lines(ns.spl, col = "blue")
lines(smooth.spline(CR_S, S, df = 22), lty = 2, col = "red")
legend(7,-90,c(paste("default [C.V.] => df =",round(ns.spl$df,1)),
               "s( * , df = 22)"), col = c("blue","red"), lty = 1:2,
       bg = 'bisque')
## x11()
detach()


# # ## Residual (Tukey Anscombe) plot:
# plot(residuals(ns.spl) ~ fitted(ns.spl))
# abline(h = 0, col = "gray")
# ## x11()

# #require(graphics)

# attach(nsRGO)
# plot(CR_N, N, main = "RGO N smoothing splines")
# ns.spl <- smooth.spline(CR_N, N)
# (ns.spl)
# ## This example has duplicate points, so avoid cv = TRUE

# lines(ns.spl, col = "blue")
# lines(smooth.spline(CR_N, N, df = 27), lty = 2, col = "red")
# legend(1,-100,c(paste("default [C.V.] => df =",round(ns.spl$df,1)),
               # "s( * , df = 27)"), col = c("blue","red"), lty = 1:2,
       # bg = 'bisque')
# ## x11()
# detach()


# ## Residual (Tukey Anscombe) plot:
# plot(residuals(ns.spl) ~ fitted(ns.spl))
# abline(h = 0, col = "gray")
# ## x11()



###   Set as time series   #################################

#load("ns.spl")
attach(nsRGO)
ns.spl <- smooth.spline(nsRGO)
(ns.spl)

tmp <- ts(nsRGO)
tsp(tmp)

colMeans(tmp)
      sd(tmp)
apply(tmp,2,min)
apply(tmp,2,max)
plot.ts(tmp[,2],tmp[,4], main="S by N",plot.type="single", col=c(1,2), xlab="Umbra area N black", ylab="Umbra area S red")
## x11()

plot.ts(tmp, main=" RGO Averages ", xlab="Days", ylab="RGO Averages", ylim(0,5))
## x11()
plot.ts(tmp[,2],tmp[,4], plot.type="single", col=c(2,3), main=" RGO Umbra Areas", xlab="N red, S_green", ylab="RGO")
# ## x11()
# acf(tmp[,1], main="CR_S")
# ## x11()
# acf(tmp[,2], main="S")
# ## x11()
# acf(tmp[,3], main="CR_N")
# ## x11()
# acf(tmp[,4], main="N")
# ## x11()

ts.acf  <- acf(tmp, plot=F)
ts.pacf <- acf(tmp, type="partial", plot=F)

sol_kTS <- tmp
outfile <- paste(WD, "sol_kTS.RData", sep="/")
save(tmp, file=outfile, ascii=FALSE)

###   Time series analysis   ###############################
###   acf and pacf   #######################################

# # acf(tmp)
# acf(tmp, type="partial")
# pairs(tmp,panel=panel.smooth)
# ## x11()
# pairs(tmp[,1:4],panel=panel.smooth)
# ## x11()

###   Autocorrelation analysis - first diff  ###############
tmp <- diff(sol_kTS)
###   Test for normal distributions   ######################

colMeans(tmp)
      sd(tmp)
      sd(tmp)*sd(tmp)
apply(tmp,2,min)
apply(tmp,2,max)

# #shapiro.test(tmp[,1])
# shapiro.test(tmp[,2])
# shapiro.test(tmp[,4])

plot(tmp[,2],type="l")
## x11()
tmp.hist <- hist(tmp[,2],prob=T,col="red",main="CR_S Sunspot Distribution",sub="Fitted n(min, max)",xlab="First Difference")
lines(seq(min(tmp[,2]),max(tmp[,2]),100), dnorm(seq(min(tmp[,2]),max(tmp[,2]),100), mean=mean(tmp[,2]), sd=sd(tmp[,2])), lwd=2)
## x11()
plot(tmp[,4],type="l")
## ## x11()
tmp.hist <- hist(tmp[,4],prob=T,col="red",main="CR_N Sunspot Distribution",sub="Fitted n(min, max)",xlab="First Difference")
lines(seq(min(tmp[,4]),max(tmp[,4]),100), dnorm(seq(min(tmp[,4]),max(tmp[,4]),100), mean=mean(tmp[,4]), sd=sd(tmp[,4])), lwd=2)
## x11()


###   Autocorrelations   ###################################
plot.ts(tmp, main="S by N",plot.type="single", col=c(1,2), xlab="S", ylab="N")
## x11()
plot.ts(tmp[,2], plot.type="single", col=c(1,2), main="CR_S", xlab="S", ylab="CR_S")
## x11()
plot.ts(tmp[,4], plot.type="single", col=c(1,3), main="CR_N", xlab="N", ylab="CR_N")
## x11()

# acf(tmp)
# acf(tmp, type="partial")
# acf(tmp[,2], main="CR_S Lag 1 ACF")
# ## x11()
# acf(tmp[,4], main="CR_N  Lag 1 ACF")
# ## x11()

# acf(tmp[,2], type="partial", main="CR_S  Lag 1 PACF")
# ## x11()
# acf(tmp[,4], type="partial", main="CR_N  Lag 1 PACF")
# ## x11()

###   arima analysis (d=1)  ##############################

(ch0 <- arima(tmp[,2],order=c(0,0,1)))
cpgram(ch0$resid, main="CR_S  ARIMA(0,1,1) Residuals")
## x11()
tsdiag(ch0)
(ch1 <- arima(tmp[,4],order=c(0,0,1)))
cpgram(ch1$resid, main="CR_N  ARIMA(0,1,1) Residuals")
## x11()
tsdiag(ch1)
## x11()


###   Cross-correlation analysis - first diff  ###########

pairs(tmp[,1:4],panel=panel.smooth, main="Pairwise Scatterplots with Loess Smoothing")
## x11()
#pairs(tmp[,1:4],panel=panel.smooth, main="Pairwise Scatterplots with Loess Smoothing")
# ### x11()
# acf(tmp)
# acf(tmp, type="partial")

##########################################################
#3D
##########################################################

nscycle <- CR_S
#scycle <- c(-2.5 * log10(CR_S) - 10.5)
snorth <- tmp[,2]
ssouth <- tmp[,4]
open3d()
# Needs to be a bigger window than the default
par3d(windowRect = c(100, 100, 612, 612))
Sys.sleep(0.1) # Allow sluggish window managers to catch up
parent <- currentSubscene3d()
mfrow3d(1, 1)
plot3d(nscycle,ssouth,snorth)
#plot3d(ssouth,snorth,nscycle)
grid3d(c("x", "t+", "z"), col = "red", lwd = 1, lty = 1, n = 31)
aspect3d(.75,.75, .5)

legend3d("top", c("RGO Numbers", "X-axis in cycles"), pch = c(1, 12))
useSubscene3d(parent)


# ###   SVD stuff            #################################


# hilbert <- function(n) { i <- 1:n; 1 / outer(i - tmp[1:2], i, "+") }
# X <- hilbert(38) [, 1:4]
# (s <- svd(X))
# D <- diag(s$d)

# s$u %*% D %*% t(s$v) #  X = U D V'
# t(s$u) %*% X %*% s$v #  D = U' X V
# xs <- s$u
# xt <- t(s$u)

# ################################# SVD plots ############################

# main <- " CR_N CR_S "
# sub <- "SVD \nmatrix"

# plot(xt,xs,col="green", main=main, xaxt="n", xlab=sub, ylab="SVD Values")

# axis(1,at=xt,label=NA)
# tck <- axis(1,at=xt,labels=F)
# legend("topleft", legend=c("SVD","SVD lines"), lwd=1, lty=c("solid","dotted","dotdash","dashed"), pch=c(3,NA,3,1), col=c("cyan","black","blue","red"))
# #lines(xt,xs,col="blue",lwd=2,lty="dotted")
# grid(nx=NA, ny=NULL, col="gray")
# ## x11()

# ########################## SVD inverse plots ###########################

# # http://www.ats.ucla.edu/stat/r/pages/svd_demos.htm
# a.svd <- svd(X)
# a.svd$d
# ds <- diag(1/a.svd$d[1:4])
# u <- a.svd$u
# v <- a.svd$v
# us <- as.matrix(u[1:4])
# vs <- as.matrix(v[1:4])

# #(a.ginv <- vs %*% ds %*% t(us))
# # using the function ginv defined in MASS
# #ginv(a)

# plot(us,vs, col="violet", main="ns SVD \ninverted matrix",sub=paste("ginv (Blue) vs. Eigen values (Green)"), xaxt="n", xlab="", ylab=" SVD Values ", type="l")
# #axis(1,labels=xs,at=xt)
# points(us,vs,pch=1,lwd=3,col="blue",cex=2)
# points(xt,xs,pch=4,lwd=4,col="green",cex=1.5)
# grid(col="black")
# ## x11()


# Error analysis

#agg <- aggregate(error[, "TTR" > 0],
#                 list(Worksunspot=wkend$Worksunspot,
#                      ReportingGeo=wkend$ReportingGeo),
#                 mean)



