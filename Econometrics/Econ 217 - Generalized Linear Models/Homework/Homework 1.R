
#Number 1

paretocumdens<-function(k,y,ymax){
    cdf<-(y/ymax)^k
  return(cdf)
}

paretoprobdens<-function(k,y,ymax){
  pdf<-k*((y^(k-1))/(ymax)^k)
  return(pdf)
}

y<-seq(1,10,length.out=10000)


cdf1<-paretocumdens(0.5,y,10)
cdf2<-paretocumdens(1,y,10)
cdf3<-paretocumdens(2,y,10)

pdf1<-paretoprobdens(0.5,y,10)
pdf2<-paretoprobdens(1,y,10)
pdf3<-paretoprobdens(2,y,10)

plot(cdf1,type = "l",col = "red", xlab = "Y", ylab = "Density", 
     main = "Pareto CDF")

lines(cdf2, type = "l", col = "blue")
lines(cdf3, type="l", col ="green")


plot(pdf1,type = "l",col = "red", xlab = "Y", ylab = "Density", 
     main = "Pareto PDF")

lines(pdf2, type = "l", col = "blue")
lines(pdf3, type="l", col ="green")


#Number 2
rm(list = ls())

library(foreign)
assign1data<-read.dta("C:/Users/Aj/Documents/UCSC Coursework/Winter Quarter 2017/Econ 217/Data/org_example.dta")

d<-subset(assign1data,year=="2008")
d<-subset(assign1data,nilf=="0")

summary(assign1data$educ)
assign1data$educ<-as.factor(assign1data$educ)

reg1<-glm(unem ~ educ, family = binomial(link = "probit"), data = assign1data)
summary(reg1)
stargazer(reg1, type = "text")




pnorm(summary(reg1)$coefficient[1, 1] + summary(reg1)$coefficient[4, 1]) - pnorm(summary(reg1)$coefficient[1, 1] + summary(reg1)$coefficient[5, 1])


