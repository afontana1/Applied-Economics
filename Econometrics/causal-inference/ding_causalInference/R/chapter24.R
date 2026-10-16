rm(list=ls())
library("ggplot2")
library("gridExtra")

## Novosad data 
library("car")
road_dat = read.csv("indianroad.csv")
table(road_dat$t, road_dat$r2012)
road_dat$runv = road_dat$left + road_dat$right
## sensitivity analysis 
seq.h  = seq(10, 80, 1)
frd_sa = lapply(seq.h, function(h){
  road_sub = subset(road_dat, abs(runv)<=h)
  road_sub$r2012hat = lm(r2012 ~ t + left + right,
                         data = road_sub)$fitted.values
  tslsreg = lm(occupation_index_andrsn ~ r2012hat + left + right,
               data = road_sub)
  res = with(road_sub,
             {
               occupation_index_andrsn -
                 cbind(1, r2012, left, right)%*%coef(tslsreg)
             })
  tslsreg$residuals = as.vector(res)
  
  c(coef(tslsreg)[2],
    sqrt(hccm(tslsreg, type = "hc2")[2, 2]),
    length(res))
})
frd_sa = do.call(rbind, frd_sa)

z95 = qnorm(0.975)
frd_sa = cbind(seq.h, 
               frd_sa,
               frd_sa[, 1] - z95*frd_sa[, 2],
               frd_sa[, 1] + z95*frd_sa[, 2])
colnames(frd_sa) = c("h", "est", "se", "n", 
                     "l.ci95", "u.ci95")
frd_sa = as.data.frame(frd_sa)
plot_est = ggplot(frd_sa, aes(x=h, y=est)) + 
  geom_smooth(aes(ymin = l.ci95, ymax = u.ci95),
              stat = "identity",
              color = "black",
              size = 0.5) + 
  geom_hline(aes(yintercept = 0),
             alpha = 0.5,
             linetype = 2) +
  xlab("bandwidth h") +
  ylab("estimate") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


plot_n = ggplot(frd_sa) + 
  geom_line(aes(x=h, y=n)) + 
  xlab("bandwidth h") +
  ylab("sample size") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

pdf("frd_indian.pdf", height = 8, width = 8)
grid.arrange(plot_est, plot_n, nrow = 2)
dev.off()

## data-driven bandwidth
library("rdrobust")
frd_road = with(road_dat,
                {
                  rdrobust(y = occupation_index_andrsn,
                           x = runv,
                           c = 0,
                           fuzzy = r2012)
                })
res = cbind(frd_road$coef, frd_road$se)
round(res, 3)





## Li et al data 
library("car")
italy = read.csv("italy.csv")
italy$left  = pmin(italy$rv0, 0)
italy$right = pmax(italy$rv0, 0)
## sensitivity analysis 
seq.h  = seq(0.1, 1, 0.01)
frd_sa = lapply(seq.h, function(h){
  italy_sub = subset(italy, abs(rv0)<=h)
  italy_sub$Dhat = lm(D ~ Z + left + right,
                      data = italy_sub)$fitted.values
  tslsreg = lm(outcome ~ Dhat + left + right,
               data = italy_sub)
  res = with(italy_sub,
             {
               outcome -
                 cbind(1, D, left, right)%*%coef(tslsreg)
             })
  tslsreg$residuals = as.vector(res)
  
  c(coef(tslsreg)[2],
    sqrt(hccm(tslsreg, type = "hc2")[2, 2]),
    length(res))
})
frd_sa = do.call(rbind, frd_sa)

z95 = qnorm(0.975)
frd_sa = cbind(seq.h, 
               frd_sa,
               frd_sa[, 1] - z95*frd_sa[, 2],
               frd_sa[, 1] + z95*frd_sa[, 2])
colnames(frd_sa) = c("h", "est", "se", "n", 
                     "l.ci95", "u.ci95")
frd_sa = as.data.frame(frd_sa)
plot_est = ggplot(frd_sa, aes(x=h, y=est)) + 
  geom_smooth(aes(ymin = l.ci95, ymax = u.ci95),
              stat = "identity",
              color = "black",
              size = 0.5) + 
  geom_hline(aes(yintercept = 0),
             alpha = 0.5,
             linetype = 2) +
  xlab("bandwidth h") +
  ylab("estimate") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


plot_n = ggplot(frd_sa) + 
  geom_line(aes(x=h, y=n)) + 
  xlab("bandwidth h") +
  ylab("sample size") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

pdf("frd_italy.pdf", height = 8, width = 8)
grid.arrange(plot_est, plot_n, nrow = 2)
dev.off()

## data-driven bandwidth
library("rdrobust")
frd_italy = with(italy,
                 {
                   rdrobust(y = outcome,
                            x = rv0,
                            c = 0,
                            fuzzy = D)
                 })
res = cbind(frd_italy$coef, frd_italy$se)
round(res, 3)





