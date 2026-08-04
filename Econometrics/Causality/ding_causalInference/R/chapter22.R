rm(list=ls())

## Chapter 22.3
## function for binary data (Z, D, Y)
## n_{zdy}'s are the counts from 2X2X2 table
IVbinary = function(n111, n110, n101, n100, 
                    n011, n010, n001, n000){
  
  n_tr = n111 + n110 + n101 + n100
  n_co = n011 + n010 + n001 + n000
  n    = n_tr + n_co
  
  ## proportions of the latent strata
  pi_n = (n101 + n100)/n_tr
  pi_a = (n011 + n010)/n_co
  pi_c = 1 - pi_n - pi_a
  
  ## four observed means of the outcomes (Z=z,D=d)
  mean_y_11 = n111/(n111 + n110)
  mean_y_10 = n101/(n101 + n100)
  mean_y_01 = n011/(n011 + n010)
  mean_y_00 = n001/(n001 + n000)
  
  ## means of the outcomes of two strata
  mu_n1 = mean_y_10
  mu_a0 = mean_y_01
  ## ER implies the following two means
  mu_n0 = mu_n1
  mu_a1 = mu_a0
  ## stratum (Z=1,D=1) is a mixture of c and a
  mu_c1 = ((pi_c + pi_a)*mean_y_11 - pi_a*mu_a1)/pi_c
  ## stratum (Z=0,D=0) is a mixture of c and n
  mu_c0 = ((pi_c + pi_n)*mean_y_00 - pi_n*mu_n0)/pi_c
  
  ## identifiable quantities from the observed data
  list(pi_c = pi_c, 
       pi_n = pi_n, 
       pi_a = pi_a, 
       mu_c1= mu_c1,
       mu_c0= mu_c0,
       mu_n1= mu_n1,
       mu_n0= mu_n0,
       mu_a1= mu_a1,
       mu_a0= mu_a0,
       tau_c= mu_c1 - mu_c0)
}



 

## Investigators et al.(2014) data 
investigators_analysis = IVbinary(n111 = 107,
                                  n110 = 42,
                                  n101 = 68,
                                  n100 = 42,
                                  n011 = 24,
                                  n010 = 8,
                                  n001 = 131,
                                  n000 = 79)
                        
investigators_analysis
## no violation of IV assumptions 



## McDonald, Hiu and Tierney (1992) data
flu_analysis = IVbinary(n111 = 31,
                        n110 = 422,
                        n101 = 84,
                        n100 = 935,
                        n011 = 30,
                        n010 = 233,
                        n001 = 99,
                        n000 = 1027)
flu_analysis
## violation of IV assumptions 