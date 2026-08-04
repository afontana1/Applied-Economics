rm(list=ls())

## enumerate all possible matched pair assignment 
## using binary representation
MP_enumerate = function(i, n.pairs) 
{
 if(i > 2^n.pairs)  print("i is too large.")
 a = 2^((n.pairs-1):0)
 b = 2*a
 2*sapply(i-1, 
          function(x) 
            as.integer((x %% b)>=a)) - 1
}



## Darwin's data from Fisher's book - Chapter 7.5.1
library("HistData")
ZeaMays
difference = ZeaMays$diff 
n.pairs    = length(difference)
abs.diff   = abs(difference)
t.obs      = mean(difference)
t.ran      = sapply(1:2^15, 
                    function(x){ 
                      sum(MP_enumerate(x, 15)*abs.diff) 
                      })/n.pairs
pvalue     = mean(t.ran>=t.obs)
pvalue

pdf("frt_darwin.pdf", height = 5, width = 8)
par(mai = c(1, 0.1, 1, 0.1))
hist(t.ran, breaks = 50, col = "grey", border = NA,
     xlab = "paired t-statistic", 
     ylab = "", yaxt = 'n', 
     main = "exact randomization distribution: Darwin's data")
abline(v = t.obs)
text(2.8, 1000, 
     paste("p-value = ", round(pvalue, 3), sep = ""))
dev.off()




## Imbens and Rubin book: matched pair data - Chapter 7.5.2
## television program aimed at improving reading skills for children
dataxy = c(12.9, 12.0, 54.6, 60.6,
           15.1, 12.3, 56.5, 55.5,
           16.8, 17.2, 75.2, 84.8,
           15.8, 18.9, 75.6, 101.9,
           13.9, 15.3, 55.3, 70.6,
           14.5, 16.6, 59.3, 78.4,
           17.0, 16.0, 87.0, 84.2,
           15.8, 20.1, 73.7, 108.6)
dataxy = matrix(dataxy, 8, 4,  byrow = TRUE)           
diffx = dataxy[, 2] - dataxy[, 1]
diffy = dataxy[, 4] - dataxy[, 3]
dataxy = cbind(dataxy, diffx, diffy)
rownames(dataxy) = 1:8
colnames(dataxy) = c("x.control", "x.treatment", 
                     "y.control", "y.treatment",
                     "diffx", "diffy")
dataxy = as.data.frame(dataxy)
dataxy

## analysis without covariates
n      = dim(dataxy)[1]
tauhat = mean(dataxy[, "diffy"])
vhat   = var(dataxy[, "diffy"])/n
tauhat
sqrt(vhat)

## regression analysis
unadj = summary(lm(diffy ~ 1, data = dataxy))$coef
round(unadj, 3) 

adj = summary(lm(diffy ~ diffx, data = dataxy))$coef
round(adj, 3) 


## FRT with studentized statistics 
t.ran = sapply(1:2^8, function(x){ 
  z.mpe = MP_enumerate(x, 8)
  diffy.mpe = diffy*z.mpe
  diffx.mpe = diffx*z.mpe
  
  c(summary(lm(diffy.mpe ~ 1))$coef[1, 3],
    summary(lm(diffy.mpe ~ diffx.mpe))$coef[1, 3]) 
})
p.unadj = mean(abs(t.ran[1, ]) >= abs(unadj[1, 3]))
p.unadj
p.adj = mean(abs(t.ran[2, ]) >= abs(adj[1, 3]))
p.adj

pdf("frt_television.pdf", height = 4, width = 8)
par(mfrow = c(1, 2), mai = c(1, 0.2, 0.2, 0.2))
hist(t.ran[1, ], breaks = 50, col = "grey", border = NA,
     xlab = expression(hat(tau)), 
     ylab = "", yaxt = 'n', 
     main = paste("p-value = ", round(p.unadj, 3), sep = ""))
abline(v = unadj[1, 3])

hist(t.ran[2, ], breaks = 50, col = "grey", border = NA,
     xlab = expression(hat(tau)[adj]), 
     ylab = "", yaxt = 'n', 
     main = paste("p-value = ", round(p.adj, 3), sep = ""))
abline(v = adj[1, 3])
dev.off()


