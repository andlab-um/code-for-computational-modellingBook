p <- rep(NA, 100)

for (i in seq(0.01, 1, 0.01)){ 
  p[round(i*100)] <- dbinom(x=5, size = 6, prob = i)
}

#plot 
plot(c(1:100)/100, p, 
     xlab = "pheads", 
     ylab = "P(data|pheads)")
lines(c(1:100)/100, p, lwd=2)

max_L = which(p == max(p))/100
max_L

