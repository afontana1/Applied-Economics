rm(list=ls())
library("car")
## sometimes using vcovHC in sandwich is more stable than using hccm
library("sandwich")
library("ggplot2")
library("gridExtra")


## Warning: It may take a while to run the following code. 
## To reduce the time for computation, you can reduce the number of permutations in FRT,
## and reduce the number of Monte Carlo repetitions below. 


## Chapter 8.3
## compare studentized versus unstudentized statistics
## compare pseudo-outcome and  model-output strategies 
cafrt_stat_n = function(z, y)
{
  tau_n_fit = lm(y ~ z)
  tau_n     = tau_n_fit$coef[2]
  tau_n_ols = summary(tau_n_fit)$coef[2, 3]
  tau_n_ehw = tau_n/sqrt(diag(hccm(tau_n_fit))[2])
  
  c(tau_n, tau_n_ols, tau_n_ehw)
}      

cafrt_stat_f = function(z, y, x)
{	  
  tau_f_fit = lm(y ~ z  + x)
  tau_f     = tau_f_fit$coef[2]
  tau_f_ols = summary(tau_f_fit)$coef[2, 3]
  tau_f_ehw = tau_f/sqrt(diag(hccm(tau_f_fit))[2])
  
  c(tau_f, tau_f_ols, tau_f_ehw)
}

cafrt_stat_l = function(z, y, x)
{	
  x         = scale(x)
  tau_l_fit = lm(y ~ z  + x + z*x)
  tau_l     = tau_l_fit$coef[2]
  tau_l_ols = summary(tau_l_fit)$coef[2, 3]
  tau_l_ehw = tau_l/sqrt(diag(hccm(tau_l_fit))[2])
  
  c(tau_l, tau_l_ols, tau_l_ehw)
}

cafrt_pvalue = function(z, y, x, nfrt)
{
  
  ## Rosenbaum-style residual
  rr  = lm(y ~ x)$residual
  
  cafrt.stat.obs = c(cafrt_stat_n(z, y),
                     cafrt_stat_f(z, y, x),
                     cafrt_stat_l(z, y, x),
                     cafrt_stat_n(z, rr))
  
  
  cafrt.stat.rep = replicate(nfrt,
                             {  
                               zperm  = sample(z)
                               c(cafrt_stat_n(zperm, y),
                                 cafrt_stat_f(zperm, y, x),
                                 cafrt_stat_l(zperm, y, x),
                                 cafrt_stat_n(zperm, rr))     
                             })  
  
  ## p-values
  d.stat = (abs(cafrt.stat.rep) - abs(cafrt.stat.obs) >= 0)
  apply(d.stat, 1, mean)                     
  
}


## type one error simulation
nmc    = 500
nfrt   = 500

n      = 100
r      = 0.2
n1     = n*r
n0     = n - n1
sigma1 = 1
sigma0 = 0.5

x  = runif(n, -1, 1)
y1 = x^3 + rnorm(n, 0, sigma1)
y0 = - x^3 + rnorm(n, 0, sigma0)
y1 = y1 - mean(y1)
y0 = y0 - mean(y0)
zz = c(rep(1, n1), rep(0, n0))

simulation_frt = function()
{
  z = sample(zz)
  y = ifelse(z, y1, y0)
  cafrt_pvalue(z, y, x, nfrt)
}

pvalues_simulation = replicate(nmc, {simulation_frt()})

dat.p = rbind(data.frame(factor1 = "Unstudentized",
                         factor2 = "Neyman",
                         frt_pv  = pvalues_simulation[1, ]),
              data.frame(factor1 = "Studentized OLS",
                         factor2 = "Neyman",
                         frt_pv  = pvalues_simulation[2, ]),
              data.frame(factor1 = "Studentized EHW",
                         factor2 = "Neyman",
                         frt_pv  = pvalues_simulation[3, ]),
              data.frame(factor1 = "Unstudentized",
                         factor2 = "Fisher",
                         frt_pv  = pvalues_simulation[4, ]),
              data.frame(factor1 = "Studentized OLS",
                         factor2 = "Fisher",
                         frt_pv  = pvalues_simulation[5, ]),
              data.frame(factor1 = "Studentized EHW",
                         factor2 = "Fisher",
                         frt_pv  = pvalues_simulation[6, ]),
              data.frame(factor1 = "Unstudentized",
                         factor2 = "Lin",
                         frt_pv  = pvalues_simulation[7, ]),
              data.frame(factor1 = "Studentized OLS",
                         factor2 = "Lin",
                         frt_pv  = pvalues_simulation[8, ]),
              data.frame(factor1 = "Studentized EHW",
                         factor2 = "Lin",
                         frt_pv  = pvalues_simulation[9, ]),
              data.frame(factor1 = "Unstudentized",
                         factor2 = "Rosenbaum",
                         frt_pv  = pvalues_simulation[10, ]),
              data.frame(factor1 = "Studentized OLS",
                         factor2 = "Rosenbaum",
                         frt_pv  = pvalues_simulation[11, ]),
              data.frame(factor1 = "Studentized EHW",
                         factor2 = "Rosenbaum",
                         frt_pv  = pvalues_simulation[12, ]))

dat.p$factor2 = ordered(dat.p$factor2, 
                        levels = c("Neyman", "Rosenbaum", "Fisher", "Lin"))
dat.p$factor1 = ordered(dat.p$factor1, 
                        levels = c("Unstudentized", "Studentized OLS", "Studentized EHW"),
                        labels = c("unstudentized", "studentized classic", "studentized robust"))


ggplot(dat.p) + 
  geom_histogram(aes(x = frt_pv,  y = after_stat(density)),
                 breaks = (0:20)/20, 
                 fill = "grey", alpha = 0.7) + 
  geom_hline(yintercept = 1, linetype = 2, alpha = 0.8) + 
  facet_grid(factor1 ~ factor2) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank()) +
  xlab("p-values under the weak null hypothesis") + 
  xlim(c(0,1))
ggsave("typeoneerrorXFRT.pdf", height = 4.5, width = 8.5)




## Chapter 8.5
## functions for CRE
cre_stat = function(z, y, x)
{
  tau_n_fit = lm(y ~ z)
  tau_n     = tau_n_fit$coef[2]
  var_n     = vcovHC(tau_n_fit, type = "HC2")[2, 2]
  se_n      = sqrt(var_n)
  
  x         = scale(x)
  tau_l_fit = lm(y ~ z  + x + z*x)
  tau_l     = tau_l_fit$coef[2]
  var_l     = vcovHC(tau_l_fit, type = "HC2")[2, 2]
  se_l      = sqrt(var_l)
  
  c(tau_n, se_n, tau_n/se_n,
    tau_l, se_l, tau_l/se_l,
    length(z))
}      

cre_frt = function(z, y, x, n.frt = 10^3, n.round = 3)
{
  
  test.stat = cre_stat(z, y, x)
  ## p value based on Normal
  asy_p_n = 2*pnorm(abs(test.stat[3]), lower.tail = FALSE)
  asy_p_l = 2*pnorm(abs(test.stat[6]), lower.tail = FALSE)
  ## p value based on FRT
  null.dist = replicate(n.frt, {
    zrandom = sample(z)
    cre_stat(sample(z), y, x)[c(3, 6)]
  })
  frt_p_n = mean(abs(null.dist[1, ]) >= abs(test.stat[3]),
                 na.rm = TRUE)
  frt_p_l = mean(abs(null.dist[2, ]) >= abs(test.stat[6]),
                 na.rm = TRUE)
  
  ## result
  res = cbind(c(test.stat[1], test.stat[4]), 
              c(test.stat[2], test.stat[5]), 
              c(asy_p_n, asy_p_l), 
              c(frt_p_n, frt_p_l))
  res = round(res, n.round)
  row.names(res) = c("Neyman", "Lin")
  colnames(res) = c("estimate", 
                    "s.e.", 
                    "p (Normal)",
                    "p (FRT)")
  
  list(res = res, null.dist = null.dist)                    
}

## functions for SRE
sre_stat = function(z, y, block, x)
{
  x = as.matrix(x)
  str_est = sapply(unique(block),
                   function(k){
                     cre_stat(z[block == k], 
                              y[block == k], 
                              x[block == k, ])
                   })
  
  nn = length(z)
  tau_n_S = sum(str_est[1, ]*str_est[7, ])/nn
  var_n_S = sum((str_est[2, ])^2*(str_est[7, ])^2)/(nn^2)
  se_n_S  = sqrt(var_n_S)
  tau_l_S = sum(str_est[4, ]*str_est[7, ])/nn
  var_l_S = sum((str_est[5, ])^2*(str_est[7, ])^2)/(nn^2)
  se_l_S  = sqrt(var_l_S)
  
  c(tau_n_S, se_n_S, tau_n_S/se_n_S,  
    tau_l_S, se_l_S, tau_l_S/se_l_S)
}   


## random treatment vector in a SRE
sre_zrandom = function(z, block)
{
  for(k in unique(block)){
    z[block == k] = sample(z[block == k]) 
  }
  
  z
}

## FRT in SRE
sre_frt = function(z, y, block, x, n.frt = 10^3, n.round = 3)
{
  test.stat = sre_stat(z, y, block, x)
  ## p value based on Normal
  asy_p_n = 2*pnorm(abs(test.stat[3]), lower.tail = FALSE)
  asy_p_l = 2*pnorm(abs(test.stat[6]), lower.tail = FALSE)
  ## p value based on FRT
  null.dist = replicate(n.frt, {
    zrandom = sre_zrandom(z, block) 
    sre_stat(zrandom, y, block, x)[c(3, 6)]
  })
  frt_p_n = mean(abs(null.dist[1, ]) >= abs(test.stat[3]),
                 na.rm = TRUE)
  frt_p_l = mean(abs(null.dist[2, ]) >= abs(test.stat[6]),
                 na.rm = TRUE)
  
  ## result
  res = cbind(c(test.stat[1], test.stat[4]), 
              c(test.stat[2], test.stat[5]), 
              c(asy_p_n, asy_p_l), 
              c(frt_p_n, frt_p_l))
  res = round(res, n.round)
  row.names(res) = c("Neyman", "Lin")
  colnames(res) = c("estimate", 
                    "s.e.", 
                    "p (Normal)",
                    "p (FRT)")
  
  list(res = res, null.dist = null.dist)                    
}


## data
dat_chong = read.dta("chong.dta")
## two datasets
use.vars = c("treatment", "gradesq34", "class_level", "anemic_base_re")
dat_soccer = subset(dat_chong,
                    treatment != "Physician",
                    select = use.vars)
dat_soccer$z = (dat_soccer$treatment == "Soccer Player")
dat_soccer$y = dat_soccer$gradesq34
dat_soccer$x = scale(dat_soccer$anemic_base_re == "Yes")

dat_physician = subset(dat_chong,
                       treatment != "Soccer Player",
                       select = use.vars)
dat_physician$z = (dat_physician$treatment == "Physician")
dat_physician$y = dat_physician$gradesq34
dat_physician$x = (dat_physician$anemic_base_re == "Yes")

## analysis within strata
MC = 5*10^4
res_soccer = lapply(1:5, function(k){
  with(subset(dat_soccer, class_level == k),
       cre_frt(z, y, x, n.frt = MC))
})
res_physician = lapply(1:5, function(k){
  with(subset(dat_physician, class_level == k),
       cre_frt(z, y, x, n.frt = MC))
})
## results based on dat_soccer
for(k in 1:5) print(res_soccer[[k]]$res)
## results based on dat_physician 
for(k in 1:5) print(res_physician[[k]]$res)


## pooled analysis of the SRE
res_soccer_sre = with(dat_soccer,
                      sre_frt(z, y, class_level, x, n.frt = MC))
res_physician_sre = with(dat_physician,
                         sre_frt(z, y, class_level, x, n.frt = MC))
## results based on dat_soccer 
res_soccer_sre$res
## results based on dat_physician
res_physician_sre$res



### plots of the permutation distributions
## t statistics
t_obs_soccer = data.frame()
t_obs_physician = data.frame()
for(k in 1:5)
{
  t_obs_soccer = rbind(t_obs_soccer,
                       data.frame(method = "Neyman",
                                  str = k,
                                  t.stat = res_soccer[[k]]$res[1, 1]/res_soccer[[k]]$res[1, 2]),
                       data.frame(method = "Lin",
                                  str = k,
                                  t.stat = res_soccer[[k]]$res[2, 1]/res_soccer[[k]]$res[2, 2]))
  
  t_obs_physician = rbind(t_obs_physician,
                          data.frame(method = "Neyman",
                                     str = k,
                                     t.stat = res_physician[[k]]$res[1, 1]/res_physician[[k]]$res[1, 2]),
                          data.frame(method = "Lin",
                                     str = k,
                                     t.stat = res_physician[[k]]$res[2, 1]/res_physician[[k]]$res[2, 2]))
  
}
t_obs_soccer = rbind(t_obs_soccer,
                     data.frame(str = "all",
                                method = "Neyman",
                                t.stat = res_soccer_sre$res[1, 1]/res_soccer_sre$res[1, 2]),
                     data.frame(str = "all",
                                method = "Lin",
                                t.stat = res_soccer_sre$res[2, 1]/res_soccer_sre$res[2, 2]))
t_obs_physician = rbind(t_obs_physician,
                        data.frame(str = "all",
                                   method = "Neyman",
                                   t.stat = res_physician_sre$res[1, 1]/res_physician_sre$res[1, 2]),
                        data.frame(str = "all",
                                   method = "Lin",
                                   t.stat = res_physician_sre$res[2, 1]/res_physician_sre$res[2, 2]))

## null distributions
frt_dist_soccer = data.frame()
frt_dist_physician = data.frame()
for(k in 1:5)
{
  frt_dist_soccer =  rbind(frt_dist_soccer,
                           data.frame(method = "Neyman",
                                      str = k,
                                      t.stat = res_soccer[[k]]$null.dist[1, ]),
                           data.frame(method = "Lin",
                                      str = k,
                                      t.stat = res_soccer[[k]]$null.dist[2, ]))
  
  frt_dist_physician = rbind(frt_dist_physician,
                             data.frame(method = "Neyman",
                                        str = k,
                                        t.stat = res_physician[[k]]$null.dist[1, ]),
                             data.frame(method = "Lin",
                                        str = k,
                                        t.stat = res_physician[[k]]$null.dist[2, ]))
}

frt_dist_soccer = rbind(frt_dist_soccer,
                        data.frame(str = "all",
                                   method = "Neyman",
                                   t.stat = res_soccer_sre$null.dist[1, ]),
                        data.frame(str = "all",
                                   method = "Lin",
                                   t.stat = res_soccer_sre$null.dist[2, ]))
frt_dist_physician = rbind(frt_dist_physician,
                           data.frame(str = "all",
                                      method = "Neyman",
                                      t.stat = res_physician_sre$null.dist[1, ]),
                           data.frame(str = "all",
                                      method = "Lin",
                                      t.stat = res_physician_sre$null.dist[2, ]))

## plots based on "soccer" and "physician"
plot_soccer = ggplot(frt_dist_soccer) + 
  geom_histogram(aes(x = t.stat, y = after_stat(density)),
                 alpha = 0.4) + 
  facet_grid(method~str) + 
  stat_function(fun = dnorm, 
                args = list(mean = 0, sd = 1),
                alpha = 0.6) + 
  geom_vline(data = t_obs_soccer, 
             aes(xintercept = t.stat),
             alpha = 0.6,
             linetype = 2) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(0,0,1,0), "cm")) +
  xlab("(a) soccer versus control")

plot_physician = ggplot(frt_dist_physician) + 
  geom_histogram(aes(x = t.stat, y = after_stat(density)),
                 alpha = 0.4) + 
  facet_grid(method~str) + 
  stat_function(fun = dnorm, 
                args = list(mean = 0, sd = 1),
                alpha = 0.6) + 
  geom_vline(data = t_obs_soccer, 
             aes(xintercept = t.stat),
             alpha = 0.6,
             linetype = 2) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(0,0,1,0), "cm")) +
  xlab("(b) physician versus control")

pdf("frt_dist_chong.pdf", height = 5, width = 8.5)
grid.arrange(plot_soccer, plot_physician)
dev.off()

