
s = 100
mu = 0.000000082
sig = 0.00048
t=252*6.5*60
u=exp(sig)
d=1/u

p<-(exp(mu)-d)/(u-d)
ret<-rep(NA,length=100)
s<-rep(NA,length=t+1)

s[1]<-100

for(i in 1:100){
  
  path<-runif(t)
  
  for(j in 1:t){
    
    ifelse(path[j]<p,s[j+1]<-s[j]*u,s[j+1]<-s[j]*d)
    
  }
  
  ret[i]<-(s[length(s)]-100)/100
  
}


ret<-data.frame(ret)

hist(ret$ret, breaks=20)









