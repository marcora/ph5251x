library(dplyr)
library(magrittr)

babies = read.table("babies.txt", header=TRUE)
bwt.nonsmoke = filter(babies, smoke == 0) %$% bwt
bwt.smoke = filter(babies, smoke == 1) %$% bwt

popdiff = mean(bwt.nonsmoke) - mean(bwt.smoke)

N = 15

alphas = c(0.1, 0.05, 0.01)

powers = sapply(alphas, function(alpha){
  rejections = sapply(1:1000, function(i) {
    s1 = sample(bwt.nonsmoke, N)
    s2 = sample(bwt.smoke, N)
    t.test(s1, s2)$p.value < alpha
  })  
  power = mean(rejections) # power = 1 - beta = TP / P = sum(rejections)/length(rejections) = mean(rejections)
  power
})
