library(dplyr)
library(magrittr)

set.seed(1)

babies = read.table("babies.txt", header=TRUE)
bwt.nonsmoke = filter(babies, smoke == 0) %$% bwt
bwt.smoke = filter(babies, smoke == 1) %$% bwt

# the difference in the two population means is a constant
popdiff = mean(bwt.nonsmoke) - mean(bwt.smoke)

N = 10

# the difference in the two sample means is a random variable
for(i in 1:10){
  bwt.nonsmoke.sample = sample(bwt.nonsmoke, N)
  bwt.smoke.sample = sample(bwt.smoke, N)
  samplediff = mean(bwt.smoke.sample) - mean(bwt.nonsmoke.sample)
  print(samplediff)
}


# assuming the null, the distribution of the t-statistc/value follows the standard normal distribution (with large sample sizes)
N = 30
null.tdistro = sapply(1:1000, function(i){
    bwt.nonsmoke.sample = sample(bwt.nonsmoke, N)
    bwt.smoke.sample = sample(bwt.nonsmoke, N) # sampling from the same population because we assume the null!!!
    tval = (mean(bwt.smoke.sample) - mean(bwt.nonsmoke.sample))/sqrt(var(bwt.smoke.sample)/N + var(bwt.nonsmoke.sample)/N)
    tval
})

hist(null.tdistro)
qqnorm(null.tdistro)
qqline(null.tdistro)

# assuming the null, the distribution of the t-statistc/value follows the t-distribution (with small sample sizes)
N = 3
null.tdistro = sapply(1:1000, function(i){
  bwt.nonsmoke.sample = sample(bwt.nonsmoke, N)
  bwt.smoke.sample = sample(bwt.nonsmoke, N) # sampling from the same population because we assume the null!!!
  tval = (mean(bwt.smoke.sample) - mean(bwt.nonsmoke.sample))/sqrt(var(bwt.smoke.sample)/N + var(bwt.nonsmoke.sample)/N)
  tval
})

qs = (seq(0,999)+0.5)/1000
qqplot(qt(qs, df=2*N-2), null.tdistro, xlim=c(-6,6), , ylim=c(-6,6))
abline(0,1)
