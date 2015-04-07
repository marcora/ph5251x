library(dplyr)
library(magrittr)

set.seed(1)

babies = read.table("babies.txt", header=TRUE)
bwt.nonsmoke = filter(babies, smoke == 0) %$% bwt

popvar = var(bwt.nonsmoke)

N = 50

vars = replicate(1000, (function(){
  bwt.nonsmoke.sample = sample(bwt.nonsmoke, N)
  var(bwt.nonsmoke.sample)
})())

hist(vars)
abline(v=popvar, col=2)

mean(vars > popvar * 1.5)

Ns = 2:400

var.estimates = sapply(Ns, function(N) var(sample(bwt.nonsmoke, N)))

plot(Ns, var.estimates)
abline(h=popvar, col=2)
