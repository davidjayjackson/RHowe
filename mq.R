"mq" <- function(x,lag=24){
# Compute multivariate Ljung-Box test statistics
#
if(!is.matrix(x))x=as.matrix(x)
nr=nrow(x)
nc=ncol(x)
g0=var(x)
ginv=solve(g0)
qm=0.0
cat("        m,  Q(m)    and p-value:","\n")
df = 0
pvs=NULL
for (i in 1:lag){
  x1=x[(i+1):nr,]
  x2=x[1:(nr-i),]
  g = cov(x1,x2)
  g = g*(nr-i-1)/(nr-1)
  h=t(g)%*%ginv%*%g%*%ginv
  qm=qm+nr*nr*sum(diag(h))/(nr-i)
  df=df+nc*nc
  pv=1-pchisq(qm,df)
  pvs=c(pvs,pv)
  print(c(i,qm,pv),digits=5)
}
par(mfcol=c(1,1))
plot(pvs,ylim=c(0,1),main="p-values of Ljung-Box statistics")
abline(h=c(0))
lines(rep(0.05,lag),lty=2,col='blue')
}