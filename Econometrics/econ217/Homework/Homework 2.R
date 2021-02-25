#(View) for viewing 

# Number 2

Newton<-function(x){
  counter <- 1
  while( abs(-1/3+2*(exp(-x))/3)>.00000001 & counter<=10000 ){
    x <- x-1/3+2*(exp(-x))/3
    counter <- counter+1
  }
  x
}

x<-c(0)
Newton(x)


Newton2<-function(x,n){
  counter <- 1
  while( abs((exp(x)-2)/n*exp(x))>.00000001 & counter<=10000){
    x <- x-(exp(x)-2)/n*exp(x)
    counter <- counter+1
  }
  c(x,counter)
}

for(i in 2:8){
  print(Newton2(0,i)[c(1,2)])
}


Newton2(0,2)
Newton2(0,3)
Newton2(0,4)
Newton2(0,5)
Newton2(0,6)
Newton2(0,7)
Newton2(0,8)

#Number 3
#(a)
library("foreign")
data<-read.dta("C:/Users/Aj/Documents/UCSC Coursework/Winter Quarter 2017/Econ 217 - Applied Econometrics II/Data/org_example.dta")
subdata<-subset(data,state=="CA" & year==2013)
summary(subdata$rw)
reg1<-glm(rw~age+female+wbho+educ,subdata,family="poisson"(link=("log")))
summary(reg1)
library(stargazer)
stargazer(reg1,type="text")
library(mfx)
idk<-poissonmfx(reg1,subdata,atmean= TRUE)
idk
#(b)
reg2<-glm(rw~age+female+educ,subdata,family="poisson"(link="log"))
LR<-(reg2$deviance-reg1$deviance)
criticalvalue<-qchisq(.95, df=3)
ifelse(LR>criticalvalue,"Reject", "Dont Reject")
#(c)
prediction<-predict(reg1, newdata = subdata, type = "response")
length(prediction)

plot(density(x = prediction), main="density", ylab = "density", xlab="wage", type="l", col="blue")
wage<-subdata$rw
lines(density(x = wage,na.rm=TRUE), col="red")
legend(40,.04, legend = c("predicted", "actual"), col=c("blue","red"),lty=1,cex=1)





