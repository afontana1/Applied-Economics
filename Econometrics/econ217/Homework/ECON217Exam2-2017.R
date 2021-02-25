## Anthony Fontana
## Exam 2
library(foreign)

##Number 1
library(gam)
data_1<-read.dta("https://people.ucsc.edu/~aspearot/Econ_217/org_example.dta")
data_1$month<-as.factor(data_1$month)
data_1$educ<-as.factor(data_1$educ)
data_1$wbho<-as.factor(data_1$wbho)
gam_1<-gam(log(hourslw)~s(age)+educ+wbho+month,data=data_1,family="gaussian")
summary(gam_1)
par(mfrow=c(2,2))
plot(gam_1,terms="s(age)",se=TRUE,rug=FALSE, main="log(hourslw)~s(age)")
abline(h=0)                              
abline(v=30)                             
abline(v=55)
plot(gam_1,terms="educ",se=TRUE,rug=FALSE, main="log(hourslw)~educ")
plot(gam_1,terms="wbho",se=TRUE,rug=FALSE, main="log(hourslw)~wbho")
plot(gam_1,terms="month",se=TRUE,rug=FALSE, main="log(hourslw)~month")
detach("package:gam",unload=TRUE)
install.packages("mgcv")
library(mgcv)
gam_1<-gam(log(hourslw)~s(age)+educ+wbho+month,data=data_1,family="gaussian")
summary(gam_1)
library(stargazer)
stargazer(gam_1,type="text")



##Number 2
#a
library(data.table)
library(foreign)
data_2<-read.csv("https://people.ucsc.edu/~aspearot/Econ_217/Exam_2_Data/Fontana_Anthony.csv",header=TRUE)

fitloess<-loess(y~x,data=data_2,span=.75,degree=1)
plot(data_2$y~data_2$x,xlab="x",ylab="y",main="suboptimal")
lines(x=data_2$x,y=predict(fitloess),type="l",ylim=c(0,1),col=3)

for(h in 1:20){
  for(i in 1:nrow(data_2)){
    drop<-data_2[i,]
    keep<-data_2[-i,]
    fit<-loess(y ~ x,span=(h/20), degree=1, data=keep)
    dropfit<-predict(fit,drop,se=FALSE)
    sqrerr<-(drop$y-as.numeric(dropfit))^2
    if(i*h==1){results<-data.frame(h,i,sqrerr)}
    if(i*h>1){results<-rbind(results,data.frame(h,i,sqrerr))}
  }
}
tapply(results$sqrerr,results$h,FUN=sum,na.rm=TRUE)
use_h<-min(tapply(results$sqrerr,results$h,FUN=sum,na.rm=TRUE))
use_h


fitloess2<-loess(data_2$y~data_2$x,data=data_2,span=5/20)
plot(data_2$y~data_2$x,xlab="x",ylab="y",main="optimal loess")
lines(data_2$x,predict(fitloess2),type="l")

#b
fitloess2<-loess(data_2$y~data_2$x,data=data_2,span=5/20)
summary(fitloess2)
resid<-as.numeric(fitloess2$residuals)
predict<-predict(fitloess2)
B<-20
resultsw<-matrix(NA,nrow=B,ncol=(length(predict(fitloess2))+1))
for(rep in 1:B){
  newresid<-ifelse(runif(nrow(data_2),0,1)>0.5,resid,-resid)
  data_2$rw_boot<-predict+newresid
  fit3<-loess(rw_boot~x,data=data_2,span=5/20)
  coef.B<-predict(fit3)
  resultsw[rep,1]<-rep
  resultsw[rep,2:ncol(resultsw)]<-t(as.matrix(coef.B))
  print(rep)
}

conf<-matrix(NA,nrow=ncol(resultsw),ncol=2)
resultsw<-as.data.frame(resultsw)
names(resultsw)<-c("rep",names(coef(fit3)))
for(i in 1:nrow(data_2)){
conf[i,]<-quantile(resultsw[,i],prob=c(.05,.95),na.rm=TRUE)
print(conf)
}
conf<-conf[-101,]

fitloess2<-loess(data_2$y~data_2$x,data=data_2,span=5/20)
plot(data_2$y~data_2$x,xlab="x",ylab="y",main="optimal loess")
lines(data_2$x,predict(fitloess2),type="l")
lines(data_2$x,conf[,1],col="blue",na.rm=TRUE)
lines(data_2$x,conf[,2],col="red",na.rm=TRUE)

#c
#library(splines)
# fit<-nls2(y~x^(h)*sin(a*x), data=keep,start=c(h=1,a=1),algorithm="brute-force")

#for(h in 1:50){
  # for(i in 1:nrow(data_2)){
   # drop<-data_2[i,]
   # keep<-data_2[-i,]
   # fit<-lm(y~bs(x,h),data_2)
   # dropfit<-predict(fit,drop,se=FALSE)
   # sqrerr<-(drop$y-as.numeric(dropfit))^2
   # if(i*h==1){results<-data.frame(h,i,sqrerr)}
    #if(i*h>1){results<-rbind(results,data.frame(h,i,sqrerr))}
 # }
#}
#tapply(results$sqrerr,results$h,FUN=sum,na.rm=TRUE)
#use_h<-min(tapply(results$sqrerr,results$h,FUN=sum,na.rm=TRUE))
#use_h

aparameter<-seq(from=1,to=2,by=0.1)
hparameter<-seq(from=0,to=1,by=0.1)
for(h in 1:length(hparameter)){
  for(a in 1:length(aparameter)){
    for(i in 1:nrow(data_2)){
      drop<-data_2[i,]
      keep<-data_2[-i,]
      x<-data_2$x^(h)*sin(a*h)
      fit<-lm(y~x,data_2)
      dropfit<-predict(fit,drop,se=FALSE)
      sqrerr<-(drop$y-as.numeric(dropfit))^2
      if(i*h==1){results<-data.frame(hparameter[h],aparameter[a],i,sqrerr)}
      if(i*h>1){results<-rbind(results,data.frame(hparameter[h],aparameter[a],i,sqrerr))}
      print(c(aparameter,hparameter,i))
    }
  }
}

tapply(results$sqrerr,results$h,FUN=sum,na.rm=TRUE)
use_h<-min(tapply(results$sqrerr,results$h,FUN=sum,na.rm=TRUE))
use_h






