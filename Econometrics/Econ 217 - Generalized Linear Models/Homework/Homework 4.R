#Anthony Fontana
#Econ 217 
#assignment 4

#ARp<-function(n,phi){
 # p<-length(phi)
  #es<-rnorm(n+p)
  #Y<-rep(0,n+p)
  #for (i in (p+1):length(Y)){
    #Y[i]<-t(phi)%*%Y[(i-p):(i-1)+es[i]]
  #}
  #Y<-Y[-(1:p)]
 # return(Y)
#}

#MAp Process
#MAp<-function(n,theta){
  #d<-length(theta)
  #es<-rnorm(n+d)
  #Y<-rep(0,n+d)
 #for(i in (d+1):length(Y)){
    #Y[i]<-es[i]+t(theta)%*%es[(i-d):(i-1)]
  #}
 # Y<-Y[-(1:d)]
  #return(Y)
#}

## Number 1
#N= 10,1000,10000
phi=c(.5,.2,.1)
theta=c(.5,.3)
n=10

ARMA<-function(n,phi,theta){
  p<-length(phi)
  q<-length(theta)
  r<-max(p,q)
  es<-rnorm(n+r)
  Y<-rep(0,n+r)
  Y[1:r]<-rnorm(r)
  for(i in (r+1):(r+n)){
    Y[i]<-t(phi)%*%Y[(i-1):(i-p)]+t(theta)%*%es[(i-1):(i-q)]+es[i]
  }
  return(Y[-(1:r)])
}
  
ARMAplot<-function(N,plot=0){
  Y=ARMA(N,c(.3, .2, .1),c(.2, .1))
  if (plot==1) {
    plot(Y,type="l")
    acf(Y,lag.max=10,type="correlation")
  }
  c(mean(Y), var(Y))
}

par(mfcol=c(2,3))
ARMAplot(100,plot=1)
ARMAplot(1000,plot=1)
ARMAplot(10000,plot=1)

B=1000
for(rep in 1:B){
  idk<-ARMAplot(100)
  Mean<-as.numeric(idk[1])
  Var<-as.numeric(idk[2])
  if(rep==1){results<-data.frame(rep,Mean,Var)}
  else{results<-rbind(results,data.frame(rep,Mean,Var))}
}
quantile(results$Mean,prob=c(0.025,0.975),na.rm=TRUE)
quantile(results$Var,prob=c(0.025,0.975),na.rm=TRUE)


#Number 2
install.packages("MSBVAR")
install.packages("quantmod")
library(quantmod)
getSymbols('GOOG',from='2014-04-01',to='2016-01-01')
price_Google=GOOG$GOOG.Open

getSymbols('AMZN',from='2014-04-01',to='2016-01-01')
price_Amazon=AMZN$AMZN.Open

n=length(price_Amazon)
regGoogle=lm(price_Google[2:n]~price_Google[1:(n-1)]+price_Amazon[1:(n-1)])
regAmazon=lm(price_Amazon[2:n]~price_Google[1:(n-1)]+price_Amazon[1:(n-1)])
summary(regGoogle)
library(stargazer)
stargazer(regGoogle,type="text")
summary(regAmazon)
stargazer(regAmazon,type="text")

library(MSBVAR)
idk<-ts(data.frame(price_Amazon,price_Google))
granger.test(idk,p=1)

#Dont know how to do C was never taught this










