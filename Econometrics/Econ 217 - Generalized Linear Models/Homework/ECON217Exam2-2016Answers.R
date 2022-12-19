
########################################
############   PROBLEM #1   ############
########################################

### Load the GAM library

  library(gam)

### Load data from website

  x<-read.csv("http://people.ucsc.edu/~aspearot/Econ_217/Wages1983.csv")

### Clean the Data

  x<-subset(x,is.na(feduc)==FALSE)
  x<-subset(x,is.na(meduc)==FALSE)

### Run the GAM model

  gamfit<-gam(log(wage)~s(educ)+s(exper)+s(feduc)+s(meduc),x,family="gaussian")

### Run the GAM model

  par(mfrow=c(2,2))                                   ### inititalize the 2X2 plot
  plot(gamfit,terms="s(educ)",se=TRUE,rug=FALSE, main="log(wage)~s(educ)") ### plot the smooth term in education 
  abline(h=0)                                         ### Add a horizontal line at zero
  abline(v=12.7)                                      ### Add in important points in the figure (this was done by trial and error)
  abline(v=14.1)
  plot(gamfit,terms="s(exper)",se=TRUE,rug=FALSE, main="log(wage)~s(exper)")     ### plot the smooth term in experience
  abline(h=0)                                         ### Add a horizontal line at zero
  abline(v=10.5)                                      ### Add in important points in the figure (this was done by trial and error)
  abline(v=13)
  plot(gamfit,terms="s(feduc)",se=TRUE,rug=FALSE, main="log(wage)~s(feduc)")     ### plot the smooth term in father's education
  abline(h=0)                                         ### Add a horizontal line at zero
  abline(v=4.0)                                       ### Add in important points in the figure (this was done by trial and error)
  abline(v=7.4)
  plot(gamfit,terms="s(meduc)",se=TRUE,rug=FALSE, main="log(wage)~s(meduc)")     ### plot the smooth term in father's education
  abline(h=0)                                         ### Add a horizontal line at zero
  abline(v=6)                                         ### Add in important points in the figure (this was done by trial and error)
  abline(v=7.8)
  abline(v=12)




########################################
############   PROBLEM #2A   ###########
########################################

###  In the problem you use the dataset provided.  For this answer, I will 
###  create another fake dataset like the random datasets for the problem
  
  x<-seq(0,10,length.out=100)           ###  Create a uniform support of x's
  cutoff<-runif(1,2,8)                  ###  Randomly choose a kink
  b<-runif(1,4,5)                       ###  Randomly generate a coefficient on the kink
  u<-rnorm(length(x),0,2)               ###  Create a random vector of noise
  y<-x+b*ifelse(x>cutoff,x-cutoff,0)+u    ###  Generate outcome variable


####  Find the optimal Kink

  d<-data.frame(y,x)
  plot(y~x,d)

  hs<-seq(1,9,0.1)  ### Create a sequence of test points

  for(h in 1:length(hs)){  ### iterate through all possible kinks defined by hs
    for(i in 1:nrow(d)){  ### iterate through all rows in the dataset

      d_drop<-d[i,]            ### create a dataset with the dropped observation
      d_keep<-d[-i,]           ### define data frame with the remaining observations
      fit<-lm(y~x+ifelse(x>hs[h],x-hs[h],0),d_keep)  ### run at this particular kink guess
      dropfit<-predict(fit,d_drop,se=FALSE)   ###  Predict fit of the dropped observation
      sqrerr<-(d_drop$y-as.numeric(dropfit))^2       ###  calculate squarred error of dropped observation relative to fit
      if(i*h==1){results<-data.frame(hs[h],i,sqrerr)}   ### Save results
      if(i*h>1){results<-rbind(results,data.frame(hs[h],i,sqrerr))}
    
    }
  }


  SSRs<-sort(tapply(results$sqrerr,results$h,FUN=sum,na.rm=TRUE))   ### sort SSRs, minimum first
  cutoff_hat<-as.numeric(names(SSRs[1]))    ### identify the cutoff values from the labels


########################################
############   PROBLEM #2B   ###########
########################################

###  Create a function to randomly sample from a data frame (of course this can be done later, but it's easier with a function)

  randomSample = function(df,n) { 
    return (df[sample(nrow(df), n,replace=TRUE),])
  }

###  Run 20 bootstrap iterations

  for(r in 1:20){       ###  Iterate through replications
  
    sd<-randomSample(d,nrow(d))   ###  generate random sample
  
    for(h in 1:length(hs)){ ###  Iterate through possible kink points
      for(i in 1:nrow(sd)){ ###  Iterate through rows of the replicated sample
      
        d_drop<-sd[i,]            ### create a dataset with the dropped observation
        d_keep<-sd[-i,]           ### define data frame with the remaining observations
        fit<-lm(y~x+ifelse(x>hs[h],x-hs[h],0),d_keep)  ### run at this particular kink guess
        dropfit<-predict(fit,d_drop,se=FALSE)   ###  Predict fit of the dropped observation
        sqrerr<-(d_drop$y-as.numeric(dropfit))^2       ###  calculate squarred error of dropped observation relative to fit
        if(i*h==1){results<-data.frame(hs[h],i,sqrerr)}   ### Save results
        if(i*h>1){results<-rbind(results,data.frame(hs[h],i,sqrerr))}
      
      }
    }
  
    SSRs<-sort(tapply(results$sqrerr,results$h,FUN=sum,na.rm=TRUE))   ### sort SSRs, minimum first
    h_hat<-as.numeric(names(SSRs[1]))    ### identify the cutoff values from the labels

    if(r==1){results_h<-data.frame(r,h_hat)}  ### Save Results
    if(r>1){results_h<-rbind(results_h,data.frame(r,h_hat))}  
    print(c(r,h_hat))
  }

  confid_int<-quantile(results_h$h_hat,prob=c(0.05,0.95),na.rm=TRUE)  ###  For the 90% confidence interval, get the 5th and 95th percentiles
  
  print(confid_int)






