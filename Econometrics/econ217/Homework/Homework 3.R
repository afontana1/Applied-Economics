## Anthony Fontana
## Econ 217
## Assignment 3

## Number 1
install.packages("stargazer")
library(foreign)
library(survival)
library(stargazer)

dataset<-read.dta("C:/Users/Aj/Documents/UCSC Coursework/Winter Quarter 2017/Econ 217 - Applied Econometrics II/Homework/homework3_data.dta")
View(dataset)
haz_glm<-glm(relapse~group+offset(log(weeks)),family=poisson("log"),dataset)
summary(haz_glm)
stargazer(haz_glm,type="text")
#b
results <- matrix(NA, nrow = nrow(dataset), ncol = (length(coef(haz_glm)) + 1))

for (i in 1:nrow(dataset)) {
  drop <- dataset [i,]
  keep <- dataset [-i,]
  haz_glm <- glm(relapse ~ group + offset(log(weeks)), family = poisson("log"), data = keep)
  coef_ <- as.numeric(coef(haz_glm))
  results[i,1] <- i
  results[i,2:ncol(results)] <- t(as.matrix(coef_))
}
results <- as.data.frame(results)
names(results) <- c("id", "intercept", "treat")
plot(density(results$intercept),main="intercept")
abline(v=-2.1595)
plot(density(results$treat),main="treat")
abline(v=-1.5266)
#c
install.packages("gam")
library(gam)
library(mgcv)
View(dataset)

gam_1<-gam(relapse~s(weeks,3),data=subset(dataset,treated==1,na.rm=TRUE))
gam_2<-gam(relapse~s(weeks,3),data=subset(dataset,treated==0,na.rm=TRUE))
plot(gam_1,se=TRUE,rug=FALSE,terms="s")
plot(gam_2,se=TRUE,rug=FALSE,terms="s")
## Number 2
#a)
install.packages("data.table")
install.packages("tidyr")
install.packages("ggplot2")
library(data.table)
library(tidyr)
library(ggplot2)
dataset_2 <- read.csv("C:/Users/Aj/Documents/UCSC Coursework/Winter Quarter 2017/Econ 217 - Applied Econometrics II/Homework/WageTimeSeries_data.csv",header=TRUE)
fit.loess<-loess(realwage ~ month, family = "gaussian", span = 1, degree = 1, data = dataset_2)

data_2<- dplyr::mutate(dataset_2, predicted  = predict(fit.loess))
##predicted<-predict(fit.loess)??
##data_2<-cbind(dataset_2,predicted)??
ggplot(data_2, aes(x = month, y = realwage)) + geom_line(aes(x = month, y = x)) + ggtitle("Loess")

#b)
generaladditive<-gam(realwage~s(month,4)+s(year,4),data=dataset_2)
summary(generaladditive)
## Error; object length zero??
ggplot(dataset_2, aes(x = month, y = realwage)) + stat_smooth(method = "gam") + ggtitle("Generalized Additive")

#c)

for(h in 1:20){
  for(i in 1:nrow(data_2)){
    drop_w <- data_2[i,]
    keep_w <- data_2[-i,]
    fit.loess2 <- loess(realwage ~ month,span=(h/20), degree=1, data=keep_w)
    dropfitwage <- predict(fit.loess2,drop_w,se=FALSE)
    sqrerr <- (drop_w$realwage-as.numeric(dropfitwage))^2
    if(i*h==1){results<-data.frame(h,i,sqrerr)}
    if(i*h>1){results<-rbind(results,data.frame(h,i,sqrerr))}
  }
}
tapply(results$sqrerr,results$h,FUN=sum,na.rm=TRUE)
which.min(tapply(results$sqrerr,results$h,FUN=sum,na.rm=TRUE))
optimalfit<-loess(realwage~month,	family="gaussian",span=19/20,	degree=1,data=keep_w)
predict_2<-predict(optimalfit)
ggplot(keep_w, aes(x = month, y = realwage)) + geom_line(aes(x = month, y = predict_2)) + ggtitle("Cross Validation")



## Number 3
data <- read.dta("C:/Users/Aj/Documents/UCSC Coursework/Winter Quarter 2017/Econ 217 - Applied Econometrics II/Homework/org_example.dta")
sub_data <- subset(data, (state=="CA"|state=="NV")&(year==2008|year==2013)& rw>0 & is.na(educ)==FALSE&is.na(age)==FALSE)
View(sub_data)
cal<-as.numeric(sub_data$state=="CA")
y2013<-as.numeric(sub_data$year=="2013")
diff_in_diff<-lm(log(rw)~cal+y2013+I(cal*y2013)+educ+age,sub_data)
summary(diff_in_diff)
stargazer(diff_in_diff,type="text")
conf<-confint(diff_in_diff,4,level=.95)
conf
#b

randomSample = function(df,n) {
  return (df[sample(nrow(df),n, replace=TRUE),])
}

for(rep in 1:1000){
  diff_in_diff_2 <- lm(log(rw)~cal+y2013+I(cal*y2013)+educ+age,randomSample(sub_data,nrow(sub_data)))
  coeff <- as.numeric(coef(diff_in_diff_2))
  newdata<- data.frame(rep,t(as.matrix(coeff)))
  if(rep==1){results <- newdata}
  if(rep>1){results <- rbind(results,newdata)}
}
quantile(results[4],prob=c(.025,.975),na.rm=TRUE)

#c)




