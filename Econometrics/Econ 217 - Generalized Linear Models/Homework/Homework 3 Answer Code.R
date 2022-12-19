# ECON 217 Answer and Code to Homework 3 #

# libraries we are going to use. If you do it for the first time, don't forget to "install" them.
  library(survival)
  library(foreign)
  library(gam)

############ Problem 1 ############

###### Part a ######
  
  data_gehan <- read.dta("https://people.ucsc.edu/~aspearot/Econ_217/gehan.dta")

# Model 

  hazard_glm<-glm(relapse~group+offset(log(weeks)), family=poisson("log"),data=data_gehan)
  summary(hazard_glm)
  
  # Note: "offset" restricts the coefficient to be 1.

# Calculate the parameter (theta): control group

  # Note: "coef(your saved regression)" returns the coefficients. the first element coef(..)[1] returns intertcept
  # and from the [2], it returns each coefficient, in order.
  theta_control<-exp(coef(hazard_glm)[1]) 
  print(theta_control)
  
  #Answer:  The hazard (instantaneous probability of death given survival up to a given time) for the control group is 0.115
  
  # treatment group
  theta_treat<-exp(coef(hazard_glm)[1]+coef(hazard_glm)[2])
  print(theta_treat)

  #Answer:  The hazard (instantaneous probability of death given survival up to a given time) for the treatment group is 0.025
  
  # Treatment effect:
  te<-exp(coef(hazard_glm)[2])-1
  print(te)
  
  #Answer: the treatment reduces by 78% the hazard of relapse.

###### Part b ######

# Resampling
  
  for(i in 1:nrow(data_gehan)){
      smalldrop<-data_gehan [i,]
      smallkeep<-data_gehan [-i,]
      fit<-glm(relapse~group+offset(log(weeks)), family=poisson("log"),data=smallkeep)
      coefficient<-coef(fit)[2]
      if(i==1){results<-data.frame(coefficient)}
      if(i>1){results<-rbind(results,data.frame(coefficient))}
  }
  
# Calculation and plot
  
  mean<-mean(results$coefficient)
  print(mean)
  plot(density(results$coefficient))
  abline(v=mean,col="black", lty = 1)
  abline(v=coef(hazard_glm)[2],col="red", lty = 1)
  
# ANSWER:   The original estimate appears to be below the mode of the distribution of estimates.
# However, note that we have ploted both the mean of the distribution of estimates and the actual estimate, where we find that they are pretty much the same.


###### Part c ######

# The issue with this question is tricky.  First, everybody in the control group relapses at some point, so
# attempting to regress relapse on some interaction of group and weeks will not work.  So, what we're going to do
# is rearrange the data a bit to plot something similar to Kaplan-Meier estimates using gam.  

# First, define a variable that reports the maximum number of weeks:
  
  maxweeks<-max(data_gehan$weeks,na.rm=TRUE)

# Then, iterate through all possible weeks up to max weeks and calculate the probability of relapse, both within
# group and within the entire sample.

  for(i in 0:maxweeks){
    
    #  For week i, create a variable identifying whether a respondent has relapsed
    data_gehan$relapse_week<-ifelse(data_gehan$relapse==1&data_gehan$weeks<=i,1,0)
    
    #  Then, create two new data frames - one for treated and one for control
    tg<-subset(data_gehan,group=="treated")
    cg<-subset(data_gehan,group=="control")
    
    #  At week i, calculate the share of the sample, and of each group, that has relapsed
    share_relapse<-mean(data_gehan$relapse_week,na.rm=TRUE)
    share_relapse_treatment<-mean(tg$relapse_week,na.rm=TRUE)
    share_relapse_control<-mean(cg$relapse_week,na.rm=TRUE)
    
    #  Save these shares
    if(i==0){newg<-data.frame(i,share_relapse,share_relapse_treatment,share_relapse_control)}
    if(i>0){newg<-rbind(newg,data.frame(i,share_relapse,share_relapse_treatment,share_relapse_control))}  
  }

#  Create GAM objects 
  gam_all<-gam(share_relapse~s(i),data=newg,family=gaussian)
  gam_treat<-gam(share_relapse_treatment~s(i),data=newg,family=gaussian)
  gam_control<-gam(share_relapse_control~s(i),data=newg,family=gaussian)

# Plot the relative probability of relapse by week, one plot for the treated group, and another for the control
  par(mfrow=c(1,2))
  plot(gam_treat,se=TRUE,main="Treated Group",ylim=c(-0.7,0.2))
  plot(gam_control,se=TRUE,main="Control Group",ylim=c(-0.7,0.2))

# ANSWER:  To interpret, recall that the probability of relapse at week 0 is zero for all groups (by contruction)
# Thus over a 35 week period, the probability of relapse in the treated group goes up by 0.5
# In the control group, this probability goes up by almost 0.95

# Note: The goal of this part is combining non-parametric estimation (gam) with survival analysis.
# Your answer in HW3 problem 1c is not graded, as long as you have tried it, it's ok.



############ Problem 2 ############
  
###### Part a ######

  wage_series<-read.csv("https://people.ucsc.edu/~aspearot/Econ_217/WageTimeSeries.csv",header=TRUE)

# Here is a (tricky) way to sort the data by month
# If you do not sort it, it will probably get messy when you plot the fitted value in a line.   
  wage_series<-wage_series[order(wage_series$month),] 

# Choice of span is flexible here; later in Part c, the vross validation will tell the optimal span.
  fit.loess<-loess(realwage ~ month, data=wage_series, span=.75, degree=1)
  plot(wage_series$month,predict(fit.loess), type="l", col=1)

###### Part b ######

# Here are three possible answers. You don't need to do all of them.
# Choice of smoothing parameter is flexible, but must not be linear
  
  par(mfrow=c(3,2))
  gamresults<-gam(realwage ~ s(month,4)+s(year,4), data=wage_series)
  plot(gamresults,se=TRUE,rug=FALSE,terms="s(month, 4)")
  plot(gamresults,se=TRUE,rug=FALSE,terms="s(year, 4)")
  gamresults<-gam(realwage ~ s(month,4)+as.factor(year), data=wage_series)
  plot(gamresults,se=TRUE,rug=FALSE,terms="s(month, 4)")
  plot(gamresults,se=TRUE,rug=FALSE,terms="as.factor(year)")
  gamresults<-gam(realwage ~ as.factor(month)+as.factor(year), data=wage_series)
  plot(gamresults,se=TRUE,rug=FALSE,terms="as.factor(month)")
  plot(gamresults,se=TRUE,rug=FALSE,terms="as.factor(year)")

###### Part c ######

# Cross Validation
  rm(results)
  for(h in 1:100){
    for(i in 1:nrow(wage_series)){
      wagedrop<-wage_series[i,]
      wagekeep<-wage_series[-i,]
      wagefit<-loess(realwage ~ month,span=(h/5), degree=1, data=wagekeep)
      dropfitwage<-predict(wagefit,wagedrop,se=FALSE)
      sqrerr<-(wagedrop$realwage-as.numeric(dropfitwage))^2
      if(exists("results")==TRUE){results<-rbind(results,data.frame(h,i,sqrerr))}
      if(exists("results")==FALSE){results<-data.frame(h,i,sqrerr)}
    }
  }
# Pick the optimal value (i.e., one with the smallest square error)
  tapply(results$sqrerr,results$h,FUN=sum,na.rm=TRUE)
  
  use_h<-which.min(tapply(results$sqrerr,results$h,FUN=sum,na.rm=TRUE))
  use_h
  
  par(mfrow=c(1,1))
  fit.loess1<-loess(wage_series$realwage~wage_series$month, family="gaussian", span=use_h/5, degree=1)
  fit.loess2<-loess(wage_series$realwage~wage_series$month, family="gaussian", span=use_h/5, degree=2)
  plot(wage_series$month,predict(fit.loess1),type="l", col=1)
  lines(wage_series$month,predict(fit.loess2), type="l", col=2)

# Note: we haven't covered the detail about arguments in loess. The "degree", must be 0,1 or 2.
# Here we show the difference of degree=1 and degree=2. You may do either.

# Note that the CV procedure always pushes the estimate to the highest span that we can find.  This due to the massive amount of noise in the data around each month.  To see this, plot the estimated lines again but simply plot the data first and then add the estimates using lines

  par(mfrow=c(1,1))
  fit.loess1<-loess(wage_series$realwage~wage_series$month, family="gaussian", span=use_h/5, degree=1)
  fit.loess2<-loess(wage_series$realwage~wage_series$month, family="gaussian", span=use_h/5, degree=2)
  plot(realwage~month,wage_series)
  lines(wage_series$month,predict(fit.loess1),type="l", col=1)
  lines(wage_series$month,predict(fit.loess2), type="l", col=2)

# We're not doing a great job in fitting the data.  Overall, this suggests that real wages are determined by far more than just monthly cyclicality
  

############ Problem 3 ############

###### Part a ######
  
  data_org<-read.dta("https://people.ucsc.edu/~aspearot/Econ_217/org_example.dta")
  data_sub<-subset(data_org, (state=="CA"|state=="NV")&(year==2008|year==2013)& rw>0 & is.na(educ)==FALSE&is.na(age)==FALSE)
  summary(data_sub)

  ca<-as.numeric(data_sub$state=="CA")
  yr13<-as.numeric(data_sub$year=="2013")
  # Note: There are many ways to deal with interaction term. You can:
  # 1. Generate a new variable, say, interaction=ca*yr13, then add it into regression equation
  # 2. Use ":"  ca:yr13
  # 3. Use "*", it will be automatically converted to ":"
  # 4. Use I()

  DID_glm<-lm(log(rw)~ca+yr13+I(ca*yr13)+educ+age,data_sub)
  summary(DID_glm)
  
  ###  The estimate suggests that California recovered roughly 3.7% faster than Nevada between the period 08 and 13

  #Confidence Interval
  print(confint(DID_glm,4,level=.95))

###### Part b ######
  
  # Define a function that returns bootstrap-resampled sample
  randomSample = function(df,n) {
    return (df[sample(nrow(df),n, replace=TRUE),])
  }

rm(results)
  for(rep in 1:1000){
    fit.B<-lm(log(rw)~ca+yr13+I(ca*yr13)+educ+age,randomSample(data_sub,nrow(data_sub)))
    coef.B<-as.numeric(coef(fit.B))
    save.B<-data.frame(rep,t(as.matrix(coef.B)))
    if(rep==1){results<-save.B}
    if(rep>1){results<-rbind(results,save.B)}
  }

  quantile(results[,5],prob=c(.025,.975),na.rm=TRUE)
  
  plot(density(results[,5]),main="Coefficient of Interaction")
  abline(v=coef(DID_glm)[4])
  
  #Answer:  It appears that the confidence interval is approximately as wide but shifted downward by about 0.04.

###### Part c ######

# The key idea is, do a placebo test before the "treatment" window (2008-2013).  
  data_sub2<-subset(data_org, (state=="CA" | state=="NV") & (year==2003 | year==2008) & rw>0 & is.na(educ)==FALSE&  is.na(age)==FALSE)

  ca2<-as.numeric(data_sub2$state=="CA")
  yr08<-as.numeric(data_sub2$year=="2008")
  
  pre_trend<-lm(log(rw)~ca2+yr08+I(ca2*yr08)+educ+age,data_sub2)
  summary(pre_trend)
  
# This gives you residuals and fitted values
  residuals.full<-as.numeric(pre_trend$residuals)
  predict.full<-as.numeric(pre_trend$fitted.values)

rm(results)
# residual resampling    
  for(rep in 1:1000){
    rand.resid<-sample(residuals.full, nrow(data_sub2),replace=TRUE)
    data_sub2$rw_boot<-predict.full+rand.resid
    fit.B<-lm(rw_boot~ca2+yr08+I(ca2*yr08)+educ+age,data_sub2)
    coef.B<-as.numeric(coef(fit.B))
    save.B<-data.frame(rep,t(as.matrix(coef.B)))
    if(rep==1){results<-save.B}
    if(rep>1){results<-rbind(results,save.B)}
  }
  
  quantile(results[,5],prob=c(.025,.975),na.rm=TRUE)
  
  plot(density(results[,5]),main="Coefficient of Interaction")
  abline(v=coef(pre_trend)[4])

#ANSWER: There is clearly a pre-trend.  From the confidence interval, California real wages are growing between 0.6% and 9.1% less than Nevada wages.


