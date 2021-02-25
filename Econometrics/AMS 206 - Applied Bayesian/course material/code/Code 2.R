# Data
n = 1200
x = 450
m = 730
y = 280  # 28
 
# Priors
a = 1
b = 1
PrH0 = 1/2


# Marginal likelihoods
# theta1 = theta2
lm0 = lgamma(a+b) - lgamma(a) - lgamma(b) +
   lgamma(a+x+y) + lgamma(b+n+m-x-y) - lgamma(a+b+n+m) +
   lchoose(n, x) + lchoose(m, y)

# theta1 != theta2
lm1 = 2*(lgamma(a+b) - lgamma(a) - lgamma(b)) +
   lgamma(a+x) + lgamma(b+n-x) - lgamma(a+b+n) + lgamma(a+y) + lgamma(b+m-y) - lgamma(a+b+m) +
   lchoose(n, x) + lchoose(m, y)

# Bayes factor
B01 = exp(lm0 - lm1)

# Posterior probabilities 
PrH1.data = 1/(1 + B01*PrH0/(1-PrH0))
PrH1.data
