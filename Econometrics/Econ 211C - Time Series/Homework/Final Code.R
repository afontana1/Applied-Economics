### Anthony Fontana
### Econ 211C Final Code

data<-read.csv("C:/Users/Aj/Documents/UCSC Coursework/Spring Quarter 2017/Econ 211C - Time Series/Homework/Finaldata.csv")
View(data)



### b
length(data$Year)

#Create a G(theta,W) function 
gam<-.7
beta<-.4
theta<-c(gam , beta)

gfunc<-function(theta){
  
  
  x<-data$Cons
  
  r<-data$SPY
  
  t<-length(x)

  eqn1<-t^(-1)*(1-theta[2]*((1+r[1])*(x[1]/x[2])^(-theta[1]))*1)
  
  eqn2<-t^(-1)*(1-theta[2]*((1+r[2])*(x[1]/x[2])^(-theta[1]))*x[2])
  
  eqn3<-t^(-1)*(1-theta[2]*((1+r[3])*(x[1]/x[2])^(-theta[1]))*x[3])
  
  eqn4<-t^(-1)*(1-theta[2]*((1+r[4])*(x[1]/x[2])^(-theta[1]))*r[2])
  
  eqn5<-t^(-1)*(1-theta[2]*((1+r[5])*(x[1]/x[2])^(-theta[1]))*r[3])
  
  mat1<-cbind(eqn1,eqn2,eqn3,eqn4,eqn5)
  
  mat2<-t(mat1)
  
  return(mat1%*%mat2)
  
}


result <- optim(theta = c(0, 0), gfunc, hessian=TRUE)


## What I want is to minimize the Q function with respect to my parameters
## With this i will be able to run the second portion of the algorithm
## To compute my optimal weight matrix
## But I cannot get the optimization procedure to work
## Maybe my function isnt correct




### c









