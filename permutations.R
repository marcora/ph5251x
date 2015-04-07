library(dplyr)
library(magrittr)

set.seed(0)

babies = read.table("babies.txt", header=TRUE)
bwt.nonsmoke = filter(babies, smoke == 0) %$% bwt
bwt.smoke = filter(babies, smoke == 1) %$% bwt

popdiff = mean(bwt.smoke) - mean(bwt.nonsmoke)

N = 50

bwt.nonsmoke.sample = sample(bwt.nonsmoke, N)
bwt.smoke.sample = sample(bwt.smoke, N)

samplediff = mean(bwt.smoke.sample) - mean(bwt.nonsmoke.sample)

null.distro = replicate(1000, {
  # assign observations to the smoker and nonsmoker samples randomly
  bwt.combo.perm = sample(c(bwt.nonsmoke.sample, bwt.smoke.sample))
  bwt.smoke.perm = bwt.combo.perm[1:N]
  bwt.nonsmoke.perm = bwt.combo.perm[(N+1):(2*N)]
  return(mean(bwt.smoke.perm) - mean(bwt.nonsmoke.perm))
})

pval = mean(abs(null.distro) > abs(samplediff))

hist(null.distro)
abline(v=samplediff, col='red')
abline(v=popdiff, col='blue')


samplediff = median(bwt.smoke.sample) - median(bwt.nonsmoke.sample)

null.distro = replicate(1000, {
  # assign observations to the smoker and nonsmoker samples randomly
  bwt.combo.perm = sample(c(bwt.nonsmoke.sample, bwt.smoke.sample))
  bwt.smoke.perm = bwt.combo.perm[1:N]
  bwt.nonsmoke.perm = bwt.combo.perm[(N+1):(2*N)]
  return(median(bwt.smoke.perm) - median(bwt.nonsmoke.perm))
})

pval = mean(abs(null.distro) > abs(samplediff))

hist(null.distro)
abline(v=samplediff, col='red')
abline(v=popdiff, col='blue')