### Number 2
n<-24000
sig<-.5
eps<-rnorm(n,0,sig)
epsilonmatrix<-matrix(eps,24,1000)
mu=.61
theta=.95

y1<-mu+epsilonmatrix[2:24,]+theta*epsilonmatrix[1:23,]
y2<-mu+epsilonmatrix[2:24,]-theta*epsilonmatrix[1:23,]
par(mfrow=c(2,1))
matplot(y1,type="l",main="Time Path for Y1",col=rgb(0,0,0,.2))
matplot(y2,type="l",main="Time Path for Y2",col=rgb(0,0,0,.2))



