
######  PROBLEM #1 ######

#load the foreign library and the org data
library(foreign)
library(gam)
org<-read.dta("https://people.ucsc.edu/~aspearot/Econ_217/org_example.dta")
#clean the data
org<-subset(org,is.na(hourslw)==FALSE)
org<-subset(org,is.na(educ)==FALSE)
#treat "month" as a factor variable
org$month<-as.factor(org$month)

#a.
##run the GAM model
gamfit<-gam(log(hourslw)~s(age)+educ+wbho+month,org,family="gaussian")
summary(gamfit)
##plot results
par(mfrow=c(2,2))
plot(gamfit,terms="s(age)",se=TRUE,rug=FALSE,main="log(hourslw)~s(age)")
abline(h=0)
abline(v=26.8)
abline(v=57.6)
plot(gamfit,terms="educ",se=TRUE,rug=FALSE,main="log(hourslw)~educ")
abline(h=0)
plot(gamfit,terms="wbho",se=TRUE,rug=FALSE,main="log(hourslw)~wbho")
abline(h=0)
plot(gamfit,terms="month",se=TRUE,rug=FALSE,main="log(hourslw)~month")
abline(h=0)



#[Age] There's a inverse U-shape relation between age and hours worked. Individuals between 26 and 57 tend to work more than average, while the younger and older work less than average. 
#[Educ] Individuals with "High School", "College", and "Advanced" degrees have statistically significant higher working hours than the sample average.  Individuals with "Less Than High School" and "Some College" have statistically significant lower working hours.
#[Wbho] Individuals categorized as "Hispanic" work significantly more than average. "White" and "Others" work less than average, and the "Black" does not significantly differ from average. 
#[Month] Individuals work less than year average working time from November to March, and work more than average from May to August.




######  PROBLEM #2 ######


####Use a Random Dataset for the answer key
## But of course you may use your own data
  x<-seq(0,10,length.out=100)
  freq<-round(runif(1,1,2),digits=1)
  exponent<-round(runif(1,0,1),digits=1)
  u<-rnorm(length(x),0,1)
  y<-x^exponent*sin(freq*x)+u

  d<-data.frame(x,y)
  
  
#### Part A

	rm(results)
	for(h in seq(0.1,2,by=0.05)){
	for(i in 1:nrow(d)){
  
	  smalldrop<-d[i,]
	  smallkeep<-d[-i,]
	  fit<-loess(y~x,smallkeep, family="gaussian",span=h, degree=1)
	  dropfit<-predict(fit,smalldrop,se=FALSE)
	  sqrerr<-(smalldrop$y-as.numeric(dropfit))^2
	  if(exists('results')==TRUE){results<-rbind(results,data.frame(h,i,sqrerr))}
	  if(exists('results')==FALSE){results<-data.frame(h,i,sqrerr)}
	  
	  #print(c(h,i))
	}}
	
	tapply(results$sqrerr,results$h,FUN=sum,na.rm=TRUE)

	h_ans<-as.numeric(names(which.min(tapply(results$sqrerr,results$h,FUN=sum,na.rm=TRUE))))

	fit_opt<-loess(y~x,data=d, family="gaussian",span=h_ans, degree=1)

		plot(y~x,d,main=paste("Optimal Bandwidth =",h_ans))
		lines(d$x,predict(fit_opt),type="l",lwd=3,ylim=c(-2,2))


	
#### Part B (hard)

	library(matrixStats)


	predict.full<-as.numeric(predict(fit_opt))
	resid.full<-as.numeric(fit_opt$residual)
	
	B<-100
	rm(results)
	results<-matrix(nrow=B,ncol=nrow(d))

	for(rep in 1:B){
		newresid<-ifelse(runif(nrow(d),0,1)>0.5,resid.full,-resid.full)
		d$boot<-predict.full+newresid
		fit.B<-loess(boot~x,data=d, family="gaussian",span=h_ans, degree=1)
		results[rep,1:ncol(results)]<-as.numeric(predict(fit.B))
		#print(rep)
	}
	
	d$l_confint<-colQuantiles(results,prob=0.05)
	d$u_confint<-colQuantiles(results,prob=0.95)

	### You can also use the quantile function for this, column by column.


		plot(y~x,d,main="90% Confidence Interval")
		lines(d$x,predict(fit_opt),type="l",lwd=3)	
		lines(d$x,d$l_confint,type="l",col=2)		
		lines(d$x,d$u_confint,type="l",col=2)	


#### Part C

	rm(results)
	
	results<-matrix(nrow=21*21*nrow(d),ncol=4)
	j<-1
	for(h in seq(0,1,by=0.05)){
	for(A in seq(1,2,by=0.05)){
	for(i in 1:nrow(d)){
		
	  smalldrop<-d[i,]
	  dropfit<-smalldrop$x^h*sin(A*smalldrop$x)
	  sqrerr<-(smalldrop$y-as.numeric(dropfit))^2
	  results[j,1]<-h
	  results[j,2]<-A
	  results[j,3]<-i
	  results[j,4]<-sqrerr
	  
	  j<-j+1
	}}}

	results<-as.data.frame(results)
	names(results)<-c("h","A","i","sqrerr")

	tapply(results$sqrerr,paste(results$h,results$A),FUN=sum,na.rm=TRUE)

	CVans<-(names(which.min(tapply(results$sqrerr,paste(results$h,results$A),FUN=sum,na.rm=TRUE))))

  	exponent_ANS_2c<-unlist(strsplit(CVans,split=" "))[1]
  	freq_ANS_2c<-unlist(strsplit(CVans,split=" "))[2]


exponent_ANS_2c

freq_ANS_2c



