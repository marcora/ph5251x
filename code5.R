## Power calculations

# load packages
library(dplyr)
library(magrittr)

# read data
dat = read.csv("mice_pheno.csv")

# extract populations of all female mice on hf or chow diet
hfpop = filter(dat, Sex == 'F' & Diet == 'hf') %$% Bodyweight
chowpop = filter(dat, Sex == 'F' & Diet == 'chow') %$% Bodyweight

mean(hfpop) - mean(chowpop)

alpha = 0.05

N = 12

rejections = sapply(1:1000, function(i) {
  hf = sample(hfpop, N)
  chow = sample(chowpop, N)
  t.test(hf, chow)$p.value < alpha
})

power = mean(rejections) # proportion of times we rejected the null hypothesis (in this case knowing that it is not true)
power


Ns = seq(5,50,5)

powers = sapply(Ns, function(N){
  rejections = sapply(1:1000, function(i) {
    hf = sample(hfpop, N)
    chow = sample(chowpop, N)
    t.test(hf, chow)$p.value < alpha
  })  
  power = mean(rejections)
  power
})


