## Confidence intervals

# load packages
library(dplyr)
library(magrittr)

# read data
dat = read.csv("mice_pheno.csv")

# extract population of all female mice on chow diet
pop = filter(dat, Sex == 'F' & Diet == 'chow') %$% Bodyweight

# population mean (a constant)
mu = mean(pop)

# population standard deviation (a constant)
sigma = sd(pop)

# extract sample
N = 30
y = sample(pop, N)

# sample mean (a random variable)
y.mean = mean(y)

# sample standard deviation (a random variable)
y.sd = sd(y)

# according to the CLT, the sampling distribution of y is normal with mean mu and standard deviation sigma/sqrt(N), i.e., the standard error
# because we normally can't calculate sigma, we estimate it using the sample standard deviation and obtain the standard error
y.se = sd(y)/sqrt(N)

# according to the CLT, (mu - y.mean)/(0+y.se) ~ N(0,1) [assuming the null hypothesis is true and the sample is large]
# n.b., the population, being a constant and not a random variable, does not contribute to the denominator

# for the 95% confidence interval -Q < (mu - y.mean)/(0+y.se) < Q
# Q is 1.96 (~2)
Q = qnorm(1 - 0.05/2) # use the t-distribution instead of the normal distribution if the sample is small

# the 95% confidence interval can also be expressed by y.mean - Q * y.se < mu < y.mean + Q * y.se
interval = c(y.mean - Q * y.se, y.mean + Q * y.se)

interval

plot(mu + c(-3*sigma, 3*sigma), c(1,1), type='n', xlab='weight', ylab='intervals', ylim=c(1,100))
abline(v=mu)

for (i in 1:100){
  y = sample(pop, N)
  y.mean = mean(y)  
  y.se = sd(y)/sqrt(N)
  interval = c(y.mean - Q * y.se, y.mean + Q * y.se)
  color = ifelse(interval[1] <= mu & interval[2] >= mu, 1, 2)
  lines(interval, c(i,i), col=color)  
}
