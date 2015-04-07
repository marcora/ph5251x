load("skew.RData")

dim(dat)

par(mfrow = c(3,3))
for(i in 1:ncol(dat)){
  qqnorm(dat[,i])
  qqline(dat[,i])
}

par(mfrow=c(1,2))
hist(dat[,4])
hist(dat[,9])

par(mfrow = c(1,1))

#===============================================================================

head(InsectSprays)
boxplot(split(InsectSprays$count, InsectSprays$spray))
boxplot(InsectSprays$count ~ InsectSprays$spray)

#===============================================================================

library(UsingR)
data(father.son)

plot(father.son$fheight, father.son$sheight)
cor(father.son$fheight, father.son$sheight)

identify(father.son$fheight, father.son$sheight)

x = father.son$fheight
y = father.son$sheight
n = nrow(father.son)

plot(scale(x), scale(y))
abline(h=0, v=0)

mean(scale(x) * scale(y))

sum(scale(x) * scale(y)) / (n - 1)

#===============================================================================

data(nym.2002)

hist(nym.2002$time)
plot(nym.2002$age, nym.2002$time)
plot(nym.2002$place, nym.2002$time)
qqnorm(nym.2002$time)
qqline(nym.2002$time)
tail(sort(table(nym.2002$home)),10)
boxplot(nym.2002$time ~ nym.2002$gender)

time = sort(nym.2002$time)
min(time)/median(time)
max(time)/median(time)

plot(time/median(time), ylim=c(1/4,4))
abline(h=c(1/2,1,2))

plot(log2(time/median(time)),ylim=c(-2,2))
abline(h=-1:1)

#===============================================================================

library(dplyr)

dat = read.csv('msleep_ggplot2.csv')

dat %>%
  mutate(sleep_rem_over_total = sleep_rem / sleep_total) %>%
  group_by(order) %>%
  summarise(sleep_rem_over_total.median = median(sleep_rem_over_total)) %>%
  arrange(sleep_rem_over_total.median) %>%
  head
