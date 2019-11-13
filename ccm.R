"ccm" <- function(x,lags=12,level=FALSE){
# Compute and plot the cross-correlation matrices.
# lags: number of lags used.
# level: logical unit for printing.
# Created by R.S. Tsay, March 2009
#
if(!is.matrix(x))x=as.matrix(x)
T=nrow(x)
k=ncol(x)
if(lags < 1)lags=1

# remove the sample means
av=apply(x,2,mean)
y=x
for (j in 1:k){
y[,j]=x[,j]-av[j]
}
V1=cov(y)
print("Covariance matrix:")
print(V1,digits=3)
se=sqrt(diag(V1))
SD=diag(1/se)
S0=SD%*%V1%*%SD
ksq=k*k
wk=matrix(0,ksq,(lags+1))
wk[,1]=c(S0)
j=0
cat("CCM at lag: ",j,"\n")
print(S0,digits=3)
y=y%*%SD
crit=2.326/sqrt(T)
cat("Simplified matrix:","\n")
for (j in 1:lags){
y1=y[1:(T-j),]
y2=y[(j+1):T,]
Sj=t(y2)%*%y1/T
Smtx=matrix(".",k,k)
for (ii in 1:k){
for (jj in 1:k){
if(Sj[ii,jj] > crit)Smtx[ii,jj]="+"
if(Sj[ii,jj] < -crit)Smtx[ii,jj]="-"
}
}
cat("CCM at lag: ",j,"\n")
for (ii in 1:k){
cat(Smtx[ii,],"\n")
}
if(level){
cat("Correlations:","\n")
print(Sj,digits=3)
}
wk[,(j+1)]=c(Sj)

}
par(mfcol=c(k,k))
if(k > 3)par(mfcol=c(3,3))
tdx=c(0,1:lags)
jcnt=0
for (j in 1:ksq){
plot(tdx,wk[j,],type='h',xlab='lag',ylab='ccf',ylim=c(-1,1))
abline(h=c(0))
crit=2/sqrt(T)
abline(h=c(crit),lty=2)
abline(h=c(-crit),lty=2)
jcnt=jcnt+1
if(jcnt==9){
cat("Hit return for more: ","\n")
readline()
}
#
}
par(mfcol=c(1,1))

ccm<-list(ccm=wk)

}