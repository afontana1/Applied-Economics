rm(list=ls())
library("ggplot2")
library("gridExtra")

## fisher weighting
fisher.weight = function(est, se)
{
  n = sum(est/se^2)
  d = sum(1/se^2)
  res = c(n/d, sqrt(1/d))
  names(res) = c("est", "se")
  res
}


# library("mr.raps")
# bmisbp = subset(bmi.sbp,
#                 select = c("beta.exposure",
#                            "beta.outcome",
#                            "se.exposure",
#                            "se.outcome"))
# write.csv(bmisbp, "mr_bmisbp.csv")
bmisbp = read.csv("mr_bmisbp.csv")
bmisbp$iv      = with(bmisbp, beta.outcome/beta.exposure)
bmisbp$se.iv   = with(bmisbp, se.outcome/beta.exposure)
bmisbp$se.iv1  = with(bmisbp,
                      sqrt(se.outcome^2 + iv^2*se.exposure^2)/beta.exposure)
fisher.weight(bmisbp$iv, bmisbp$se.iv)
fisher.weight(bmisbp$iv, bmisbp$se.iv1)

## egger regression 
## pay attention to the standard errors 
mr.egger = lm(beta.outcome ~ 0 + beta.exposure,
              data = bmisbp,
              weights = 1/se.outcome^2)
summary(mr.egger)$coef

mr.egger.w = lm(beta.outcome ~ beta.exposure,
                data = bmisbp,
                weights = 1/se.outcome^2)
summary(mr.egger.w)$coef 

 
## plots 
Const = 3*10^(-7)
ggplot(bmisbp) + 
  geom_tile(aes(x = beta.exposure,
                y = beta.outcome,
                height = Const/se.exposure^2,
                width = Const/se.outcome^2),
            alpha = 0.4) +
  geom_smooth(method = "lm",
              aes(x = beta.exposure,
                  y = beta.outcome,
                  weight = 1/se.outcome^2),
              color = "black", se = FALSE, 
              size = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab(expression(hat(gamma))) + 
  ylab(expression(hat(Gamma))) 
ggsave("MR_bmi_sdp.pdf", height = 5, width = 7)





## Homework problem
# library("mr.raps")
## data for homework
# bmibmi = subset(bmi.bmi,
#                 select = c("beta.exposure",
#                            "beta.outcome",
#                            "se.exposure",
#                            "se.outcome"))
# write.csv(bmibmi, "mr_bmibmi.csv")
bmibmi = read.csv("mr_bmibmi.csv")
