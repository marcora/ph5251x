d = read.csv("assoctest.csv")
t = table(d)
cs = chisq.test(t)
fs = fisher.test(t)
