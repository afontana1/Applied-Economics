###Number 1

#a

gam0<-17.5170
gam1<-15.9570
gam2<-12.4010
gam3<-8.3985
gam4<-5.0576
gam5<-3.0155

vec1<-c(gam0,gam1,gam2,gam3,gam4)
vec2<-c(gam1,gam0,gam1,gam2,gam3)
vec3<-c(gam2,gam1,gam0,gam1,gam2)
vec4<-c(gam3,gam2,gam1,gam0,gam1)
vec5<-c(gam4,gam3,gam2,gam1,gam0)

colvec<-t(c(gam1,gam2,gam3,gam4,gam5))

mat1<-rbind(vec1,vec2,vec3,vec4,vec5)

betas<-solve(mat1)%*%t(colvec)

#b

func1<-function(n){

phi<-c(1.3,-0.4)
theta<-c(0.7,0,0.1,-0.5,-0.2)
p<-length(phi)
q<-length(theta)
r<-max(p,q)
err<-rnorm(n+r,0,1)
y<-rep(0,n+r)
for( i in (r+1):(r+n)){
  y[i]<-err[i]+t(phi)%*%y[(i-1):(i-p)]+t(theta)%*%err[(i-1):(i-q)]
}
y<-y[-(1:r)]
}



#C

#create new vector containing forecasts

y<-func1(105)
forecastvec<-rep(0,5)
forecastvec[1]<-betas[1]*y[100]+betas[2]*y[99]+betas[3]*y[98]+betas[4]*y[97]+betas[5]*y[96]
forecastvec[2]<-betas[1]*forecastvec[1]+betas[2]*y[100]+betas[3]*y[99]+betas[4]*y[98]+betas[5]*y[97]
forecastvec[3]<-betas[1]*forecastvec[2]+betas[2]*forecastvec[1]+betas[3]*y[100]+betas[4]*y[99]+betas[5]*y[98]
forecastvec[4]<-betas[1]*forecastvec[3]+betas[2]*forecastvec[2]+betas[3]*forecastvec[1]+betas[4]*y[100]+betas[5]*y[99]
forecastvec[5]<-betas[1]*forecastvec[4]+betas[2]*forecastvec[3]+betas[3]*forecastvec[2]+betas[4]*forecastvec[1]+betas[5]*y[100]

forecastvec


meansqerr<-rep(0,1000)
for(i in 1:1000){
  y<-func1(105)
  forecastvec<-rep(0,5)
  forecastvec[1]<-betas[1]*y[100]+betas[2]*y[99]+betas[3]*y[98]+betas[4]*y[97]+betas[5]*y[96]
  forecastvec[2]<-betas[1]*forecastvec[1]+betas[2]*y[100]+betas[3]*y[99]+betas[4]*y[98]+betas[5]*y[97]
  forecastvec[3]<-betas[1]*forecastvec[2]+betas[2]*forecastvec[1]+betas[3]*y[100]+betas[4]*y[99]+betas[5]*y[98]
  forecastvec[4]<-betas[1]*forecastvec[3]+betas[2]*forecastvec[2]+betas[3]*forecastvec[1]+betas[4]*y[100]+betas[5]*y[99]
  forecastvec[5]<-betas[1]*forecastvec[4]+betas[2]*forecastvec[3]+betas[3]*forecastvec[2]+betas[4]*forecastvec[1]+betas[5]*y[100]
  meansqerr[i]<-(forecastvec[5]-y[105])^2
  
}

mse<-mean(meansqerr)
mse

#d


autocov<-c(gam0,gam1,gam2,gam3,gam4,gam5,0,0,0,0)
for(i in 7:10){
  autocov[i]<-1.3*autocov[i-1]-0.4*autocov[i-2]
}
vector1<-as.matrix(autocov[6:10],nrow=5)
betas1<-solve(mat1)%*%vector1
betas1

#e

##Five Step Forecast

forecastvec2<-betas1[1]*y[100]+betas1[2]*y[99]+betas1[3]*y[98]+betas1[4]*y[97]+betas1[5]*y[96]
forecastvec2


#f

### 5 Step Squared Error

meansqerr5<-rep(0,1000)
for(i in 1:1000){
  y<-func1(105)
  forecastvec2<-betas1[1]*y[100]+betas1[2]*y[99]+betas1[3]*y[98]+betas1[4]*y[97]+betas1[5]*y[96]
  meansqerr5<-(forecastvec2-y[105])^2
}

mean(meansqerr5)



#g

#### Same procedure but using coef from arima


library(forecast)

y<-func1(105)
reg1<-arima(y[1:100],order=c(2,0,5))
reg1

### obtain the coefficients from the regression


#h

phi<-c(reg1$coef[1],reg1$coef[2])
theta<-c(reg1$coef[3],reg1$coef[4],reg1$coef[5],reg1$coef[6],reg1$coef[7])

#Autcorrelations
autos<-ARMAacf(ar=phi,ma=theta,lag.max=10)
#Autocovs
autocovs<-autos*var(y)

col1<-c(autocovs[1],autocovs[2],autocovs[3],autocovs[4],autocovs[5])
col2<-c(autocovs[2],autocovs[1],autocovs[2],autocovs[3],autocovs[4])
col3<-c(autocovs[3],autocovs[2],autocovs[1],autocovs[2],autocovs[3])
col4<-c(autocovs[4],autocovs[3],autocovs[2],autocovs[1],autocovs[2])
col5<-c(autocovs[5],autocovs[4],autocovs[3],autocovs[2],autocovs[1])

mat2<-rbind(col1,col2,col3,col4,col5)
mat2

gams<-matrix(c(autocovs[2],autocovs[3],autocovs[4],autocovs[5],autocovs[6]),nrow=5)
gams
betas2<-solve(mat2)%*%gams
betas2


#i/j

###New forecasts
y<-func1(105)
newforecast<-rep(0,5)
for(i in 1:5){
  newforecast[i]<-betas2[1]*y[99+i]+betas2[2]*y[98+i]+betas2[3]*y[97+i]+betas2[4]*y[96+i]+betas2[5]*y[95+i]
}

newsqerr<-rep(0,1000)
for(j in 1:1000){
  y<-func1(105)
  for(i in 1:5){
    newforecast[i]<-betas2[1]*y[99+i]+betas2[2]*y[98+i]+betas2[3]*y[97+i]+betas2[4]*y[96+i]+betas2[5]*y[95+i]
  }
  newsqerr[j]<-(newforecast[5]-y[105])^2
}
mean(newsqerr)


###
y<-func1(105)
forecast_<-rep(0,5)
forecast_[1]<-betas2[1]*y[100]+betas2[2]*y[99]+betas2[3]*y[98]+betas2[4]*y[97]+betas2[5]*y[96]
forecast_[2]<-betas2[1]*forecast_[1]+betas2[2]*y[100]+betas2[3]*y[99]+betas2[4]*y[98]+betas2[5]*y[97]
forecast_[3]<-betas2[1]*forecast_[2]+betas2[2]*forecast_[1]+betas2[3]*y[100]+betas[4]*y[99]+betas[5]*y[98]
forecast_[4]<-betas2[1]*forecast_[3]+betas2[2]*forecast_[2]+betas2[3]*forecast_[1]+betas2[4]*y[100]+betas2[5]*y[99]
forecast_[5]<-betas2[1]*forecast_[4]+betas2[2]*forecast_[3]+betas2[3]*forecast_[2]+betas2[4]*forecast_[1]+betas2[5]*y[100]

forecast_


anothermeansqerr<-rep(0,1000)
for(i in 1:1000){
  y<-func1(105)
  forecast_<-rep(0,5)
  forecast_[1]<-betas2[1]*y[100]+betas2[2]*y[99]+betas2[3]*y[98]+betas2[4]*y[97]+betas2[5]*y[96]
  forecast_[2]<-betas2[1]*forecast_[1]+betas2[2]*y[100]+betas2[3]*y[99]+betas2[4]*y[98]+betas2[5]*y[97]
  forecast_[3]<-betas2[1]*forecast_[2]+betas2[2]*forecast_[1]+betas2[3]*y[100]+betas[4]*y[99]+betas[5]*y[98]
  forecast_[4]<-betas2[1]*forecast_[3]+betas2[2]*forecast_[2]+betas2[3]*forecast_[1]+betas2[4]*y[100]+betas2[5]*y[99]
  forecast_[5]<-betas2[1]*forecast_[4]+betas2[2]*forecast_[3]+betas2[3]*forecast_[2]+betas2[4]*forecast_[1]+betas2[5]*y[100]
  anothermeansqerr[i]<-(forecast_[5]-y[105])^2
  
}

mean(anothermeansqerr)

#k/l

### Part D again
vec_<-matrix(NA,nrow=5,ncol=1)
for(i in 1:5){
  vec_[i,1]<-autocovs[i+5]
}

betas3<-solve(mat2)%*%vec_
betas3

anotherforecast<-betas3[1]*y[100]+betas3[2]*y[99]+betas3[3]*y[98]+betas3[4]*y[97]+betas3[5]*y[96]
anotherforecast


squarerror<-rep(NA,1000)
for(i in 1:1000){
  y<-func1(105)
  forecastvec_=betas3[1]*y[100]+betas3[2]*y[99]+betas3[3]*y[98]+betas3[4]*y[97]+betas3[5]*y[96]
  squarerror[i]<-(forecast_-y[105])^2
}
mean(squarerror)

###########Question 2

#a

d<-read.csv("C:/Users/Aj/Documents/UCSC Coursework/Spring Quarter 2017/Econ 211C - Time Series/Homework/Assignment3Data.csv")
size<-nrow(d)
reg1<-lm(d$Returns[3:size]~d$Returns[2:(size-1)]+d$Returns[1:(size-2)]+d$OrderFlow[2:(size-1)]+d$OrderFlow[1:(size-2)])
reg2<-lm(d$OrderFlow[3:size]~d$OrderFlow[2:(size-1)]+d$OrderFlow[1:(size-2)]+d$Returns[2:(size-1)]+d$Returns[1:(size-2)])

summary(reg1)
summary(reg2)



#c
error<-matrix(0,nrow=2,ncol=length(residuals(reg2)))
error[1,]<-residuals(reg1)
error[2,]<-residuals(reg2)



#Variance and Covariance matrix of the residuals and find the eigenvectors
covmat<-matrix(NA,nrow=2,ncol=2)
covmat[1,1]=var(residuals(reg1))
covmat[2,2]=var(residuals(reg2))
covmat[1,2]=cov(residuals(reg1),residuals(reg2))
covmat[2,1]=cov(residuals(reg1),residuals(reg2))

covmat

eigenvec<-eigen(covmat)
vec_1<-eigenvec$vectors[,1]
vec_2<-eigenvec$vectors[,2]

mat_<-rbind(vec_1,vec_2)

newvec<-t(mat_)%*%error
newvec

## Mat_ is the matrix that orthogonalizes the error vector. This gets rid of contemporaneous correlation. 



