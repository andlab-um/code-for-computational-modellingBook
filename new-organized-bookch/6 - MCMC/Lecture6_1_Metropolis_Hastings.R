#perform MCMC with Uniform prior
burnin<-200

chain <- rep(0,5000)
obs <- 144      # observed data
propsd <- 2    # tuning parameter

chain[1] <- 150  #starting value
for (i in 2:length(chain)) {
    current <- chain[i-1]
    proposal <- current + rnorm(1,0,propsd)
    if (dnorm(obs,proposal,15) > dnorm(obs,current,15)) { 
       chain[i] <- proposal  #accept proposal
    } else {
       chain[i] <- ifelse(runif(1) < dnorm(obs,proposal,15)/dnorm(obs,current,15),  
                          proposal, 
                          current)
    }
}  

# x11(5,10)
# hist(chain)
mean(chain)

x11(5,10)
plot(density(chain),las=1,xlab=bquote("Sampled values of "*mu),
     yaxt="n",lwd=2,lty="dashed",
     main="",xlim=c(100,200),ylab="",
     ylim=c(0,max(max(density(chain)$y),
                  max(density(chain[-c(1:burnin)])$y),
                  max(dnorm(c(100:200),144,15)))*1.4))
lines(density(chain[-c(1:burnin)]),lwd=2,lty="solid")
lines(c(100:200),dnorm(c(100:200),144,15),col="red",lwd=2)
mtext("   Density",2,1)
legend("topright",inset=.02,c("Normal PDF","All MCMC","Excluding burnin"),
       lty=c("solid","dashed","solid"),col=c("red","black","black"),lwd=2)

x11(5,10)
plot(chain,type="l",las=1,xlab="Iteration",ylab="Value of accepted sample")
lines(1:burnin,chain[1:burnin],col="red")






###
#perform MCMC with Gaussian prior
burnin<-200

chain <- rep(0,5000)
obs <- 144      # observed data
propsd <- 2     # tuning parameter
priormu <- 103  # prior - mean
priorsd <- 20   # prior - sd

chain[1] <- 150  #starting value
for (i in 2:length(chain)) {
  current <- chain[i-1]
  proposal <- current + rnorm(1, 0, propsd)
  if (dnorm(obs,proposal,15)*dnorm(proposal,priormu,priorsd) > dnorm(obs,current,15)*dnorm(current,priormu,priorsd) )
    { chain[i] <- proposal  #accept proposal
  } else {
    ratio <- (dnorm(obs,proposal,15)*dnorm(proposal,priormu,priorsd)) /
      (dnorm(obs,current,15)*dnorm(current,priormu,priorsd) )
    chain[i] <- ifelse(runif(1) < ratio,  proposal, current)
  }
} 


mean(chain)

x11(5,10)
plot(density(chain),las=1,xlab=bquote("Sampled values of "*mu),
     yaxt="n",lwd=2,lty="dashed",
     main="",xlim=c(100,200),ylab="",
     ylim=c(0,max(max(density(chain)$y),
                  max(density(chain[-c(1:burnin)])$y),
                  max(dnorm(c(100:200),144,15)))*1.4))
lines(density(chain[-c(1:burnin)]),lwd=2,lty="solid")
lines(c(100:200),dnorm(c(100:200),priormu,priorsd),col="red",lwd=2)
mtext("   Density",2,1)
legend("topright",inset=.02,c("prior PDF","All MCMC","Excluding burnin"),
       lty=c("solid","dashed","solid"),col=c("red","black","black"),lwd=2)

x11(5,10)
plot(chain,type="l",las=1,xlab="Iteration",ylab="Value of accepted sample")
lines(1:burnin,chain[1:burnin],col="red")

  