##################################
#plot some betas
curve(dbeta(x, 2, 4), ylim=c(0,6), ylab="Probability Density", las=1)
curve(dbeta(x, 8, 16), add=TRUE, lty="dashed")
curve(dbeta(x, 2, 1), add=TRUE, lty="dashed")
legend("topright",c("Johnnie","Jane"), inset=.05,lty=c("solid","dashed"))

#the remaining lines are not listed in the book but perform some of the computations mentioned there
pbeta(.53, 8, 16)-pbeta(.13, 8, 16)
pbeta(.53, 2, 4)-pbeta(.13, 2, 4)

##################################
# minimize negative log likelihood
negloglik <- function (parms,x1,s1) {
  return( -log(dbinom(x=x1, size=s1, prob = parms)) ) # neg log likelihood
} 
xmin <- optimize(negloglik, c(0, 1), x1=6, s1=10)
print(xmin)  

# grid search
p = matrix(1,100)
for (i in seq(0.01, 1, 0.01)){ 
  p[round(i*100)] <- dbinom(x=6, size = 10, prob = i)
}

#plot the likelihood distribution
plot(c(1:100)/100, p, 
     xlab = "pheads", 
     ylab = "P(data|pheads)")
lines(c(1:100)/100, p, lwd=2)

max_L = which(p == max(p))/100
print(max_L)
abline(v=max_L,col="red")

      

##################################
x11(7,7)
alpha <- 12
beta <- 12
# prior distribution
curve(dbeta(x, alpha, beta),ylim=c(0,40),ylab="Probability Density",las=1,lwd=3)

t<-c(12,100,1000)
i<-0
for (h in c(14, 113, 1130)){
  i<-i+1
  # posterior distribution: prior * likelihood / evidence
  curve(dbeta(x, alpha+h, beta+t[i]), add=TRUE, lty=log10(t)+1)
  print(c((alpha+h)/(alpha+h+beta+t[i]),h/(h+t[i])))
}
legend("topright",c("{14, 26}","{113, 213}", "{1130, 2130}"), 
       inset=.05,lty=c(2:4))
abline(v=0.5,col="red")

pbeta(0.5, 1130, 1000)
pbeta(0.5305164, 1130, 1000)

pbeta(c(0.025, 0.975),  12+14, 12+12)
pbeta(c(0.025, 0.975),  12+1130, 12+1000)
