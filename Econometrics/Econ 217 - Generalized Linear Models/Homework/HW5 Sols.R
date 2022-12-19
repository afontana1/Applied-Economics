###################################################################
################  HW 5 Answers
###################################################################

################  Problem 1
################  Part A

#Step 1: Get data for google and amazon

library(quantmod)
getSymbols('GOOG',from='2014-04-01',to='2016-01-01')
price_G=GOOG$GOOG.Open

getSymbols('AMZN',from='2014-04-01',to='2016-01-01')
price_A=AMZN$AMZN.Open

####  Plot series in a panel chart
	par(mfrow=c(2,1))
	plot(price_A)
	plot(price_G)

#### VAR with one lag

	n=length(price_A)
	regG=lm(price_G[2:n]~price_G[1:(n-1)]+price_A[1:(n-1)])
	regA=lm(price_A[2:n]~price_G[1:(n-1)]+price_A[1:(n-1)])


	summary(regG)
	summary(regA)
	
	
#	> 	summary(regG)
#
# Call:
# lm(formula = price_G[2:n] ~ price_G[1:(n - 1)] + price_A[1:(n - 1)])
#
# Residuals:
#    Min      1Q  Median      3Q     Max 
#-67.296  -4.957  -0.409   4.412  81.421 
#
# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        13.623692   6.078816   2.241  0.02551 *  
# price_G[1:(n - 1)]  0.960612   0.015609  61.543  < 2e-16 ***
# price_A[1:(n - 1)]  0.023833   0.009137   2.608  0.00941 ** 
#---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 10.48 on 439 degrees of freedom
# Multiple R-squared:  0.9761,	Adjusted R-squared:  0.9759 
# F-statistic:  8949 on 2 and 439 DF,  p-value: < 2.2e-16
#
#> 	summary(regA)
#
# Call:
# lm(formula = price_A[2:n] ~ price_G[1:(n - 1)] + price_A[1:(n - 1)])
#
# Residuals:
#    Min      1Q  Median      3Q     Max 
#-52.661  -4.200  -0.300   3.799  86.672 
#
# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         3.112405   5.631642   0.553    0.581    
# price_G[1:(n - 1)] -0.009105   0.014461  -0.630    0.529    
# price_A[1:(n - 1)]  1.007254   0.008465 118.993   <2e-16 ***
#---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 9.713 on 439 degrees of freedom
# Multiple R-squared:  0.9929,	Adjusted R-squared:  0.9929 
# F-statistic: 3.08e+04 on 2 and 439 DF,  p-value: < 2.2e-16

# From the results in regG, we see that lags in the price of google and the price of amazon have a positive and significant
# association with the price of google.  In Reg A, lags in the price of amazon have a positive and significant association with 
# the price of amazon, but lags in the price of google do not.
#
# Editorial note:  I wonder if this is because amazon specializing in products that google provides (tablets, OS, big data) along 
# with products that google does not (drones, delivery, etc.).  Just speculation, but I found this interesting.

	
################  Problem 1
################  Part B

####  Granger Test, with one lag (p=1)

	library(MSBVAR)
	y2<-ts(data.frame(price_A,price_G))
	granger.test(y2,p=1)
	
#                         F-statistic     p-value
# GOOG.Open -> AMZN.Open   0.3964686 0.529246259
# AMZN.Open -> GOOG.Open   6.8037687 0.009406833
#
# We see that Amazon granger causes google, but google does not granger cause amazon.  
# This is consistent with the earlier reduced form VAR in that the lagged price of google did not have a significant association
# with current price of amazon, but the lag in amazon had a significant association with the price of google.

################  Problem 1
################  Part C

#### Impulse Response Function

#Collect Coefficients from the reduced form VAR

	B<-rbind(as.numeric(coef(regG)),as.numeric(coef(regA)))

#Initialize the initial price, which is the last price of the series

	pG<-as.numeric(price_G[n])
	pA<-as.numeric(price_A[n])
	i<-0
	p<-data.frame(i,pG,pA)

#Initialize the initial constant/price vector
	z0<-matrix(nrow=3,ncol=1)
	z0[1,1]<-1
	z0[2,1]<-pG
	z0[3,1]<-pA

#Initialize shock matrix

	shock<-matrix(nrow=2,ncol=1)
	shock[1,]<-0
	shock[2,]<-60

for(i in 1:10){

	#for the first iteration, multiply the coefficient matrix by the price matrix + the shock
	if(i==1){
		z<-B%*%z0+shock
	}
	#for remaining iterations, multiply the coefficient matrix by the updated price matrix, but without the shock.
	if(i>1){
		z<-B%*%z0
	}

	## Save the updated price
	pG<-z[1,1]
	pA<-z[2,1]
	p<-rbind(p,data.frame(i,pG,pA))
	
	## update the initial price for the next period
	z0[2,]<-pG
	z0[3,]<-pA
}

### Plot the impulse response
plot(pA~i,p,type="l",ylim=c(675,780))
lines(pG~i,p,col='blue')


################  Problem 2
################  Part A

#### Test for stationarity by running a Dickey-Fuller test with 

adf.test(price_G,k=0)

#	Augmented Dickey-Fuller Test
#
# data:  price_G
# Dickey-Fuller = -1.6828, Lag order = 0, p-value = 0.7118
# alternative hypothesis: stationary

adf.test(price_A,k=0)

#	Augmented Dickey-Fuller Test
#
# data:  price_A
# Dickey-Fuller = -1.8978, Lag order = 0, p-value = 0.6209
# alternative hypothesis: stationary

# These results both suggest that both price series are not stationary.  So, let's take differences and see whether that takes care of the problem.

dG<-diff(price_G,lag=1)
dA<-diff(price_A,lag=1)
dG<-dG[!is.na(dG)]
dA<-dA[!is.na(dA)]

adf.test(dG,k=0)

# 	Augmented Dickey-Fuller Test
#
# data:  dG
# Dickey-Fuller = -24.1344, Lag order = 0, p-value = 0.01
# alternative hypothesis: stationary
#
# Warning message:
# In adf.test(dG, k = 0) : p-value smaller than printed p-value
#

adf.test(dA,k=0)

#
# 	Augmented Dickey-Fuller Test
#
# data:  dA
# Dickey-Fuller = -24.592, Lag order = 0, p-value = 0.01
# alternative hypothesis: stationary
#
# In both DF tests on the differenced price series, we see that the unit root is REJECTED in favor of stationarity.
# Thus, both series are I(1)

################  Problem 2
################  Part B

#step2
coint <- lm(price_G ~ price_A)
summary(coint)

# Call:
# lm(formula = price_G ~ price_A)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -75.443 -20.873   0.021  28.108  56.377 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 370.12193    5.64070   65.62   <2e-16 ***
# price_A       0.51656    0.01325   39.00   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 32.06 on 441 degrees of freedom
# Multiple R-squared:  0.7752,	Adjusted R-squared:  0.7747 
# F-statistic:  1521 on 1 and 441 DF,  p-value: < 2.2e-16
# 
# These are the results from the first part of the cointegration test.  Not required for the answer.

beta<-coint$coef
resid1 <- price_G - (beta[1] + beta[2]*price_A)
resid<-as.numeric(coint$resid)
adf.test(resid,k=0)

#	Augmented Dickey-Fuller Test

# data:  resid
# Dickey-Fuller = -2.6601, Lag order = 0, p-value = 0.2987
# alternative hypothesis: stationary

#  We fail to reject a unit root in favor of stationarity.  Let's plot to find out what this looks like.

plot(resid)

#  This plot shows a relationship that certainly looks more like a random walk as opposed to something that
# is stationary.  It almost appear to follow a "cyclical" relationship.







