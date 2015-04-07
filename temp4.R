library(dplyr)
library(magrittr)

babies = read.table("babies.txt", header=TRUE)
bwt.nonsmoke = filter(babies, smoke == 0) %$% bwt
bwt.smoke = filter(babies, smoke == 1) %$% bwt

popdiff = mean(bwt.nonsmoke) - mean(bwt.smoke)

N = 30

calc.interval = function(){
  s1 = sample(bwt.nonsmoke, N)
  s2 = sample(bwt.smoke, N)
  
  test <- t.test(s1, s2)
  test
  test$p.value
  test$conf.int  
}

intervals = as.data.frame(t(replicate(1000, calc.interval())))

intervals %<>%
  mutate(length = abs(V2-V1), include.popdiff = V1 <= popdiff & V2 >= popdiff)

mean.interval.length = mean(intervals$length)

proportion.include.popdiff = sum(intervals$include.popdiff)/1000


dat.ns = sample(bwt.nonsmoke, N)
dat.s = sample(bwt.smoke, N)
X.ns = mean(dat.ns)
sd.ns = sd(dat.ns)
X.s = mean(dat.s)
sd.s = sd(dat.s)
sd.diff = sqrt(sd.ns^2/N + sd.s^2/N)
tval = (X.ns - X.s)/sd.diff

ci.upper = (X.ns-X.s) + sd.diff*1.96
ci.lower = (X.ns-X.s) - sd.diff*1.96
