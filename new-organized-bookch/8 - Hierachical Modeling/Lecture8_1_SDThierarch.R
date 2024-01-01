library(rjags)
#simulate data from experiment with 10 subjects
n <- 10  
sigtrials <- noistrials <- 100   
h <- rbinom(n,sigtrials, .8)    
f <- rbinom(n,noistrials,.2)   

#initialize for JAGS
oneinit <- list(mud=0, mub=0, taud=1, taub=1, d=rep(0,n), b=rep(0,n))
myinits <- list(oneinit)[rep(1,4)] 
sdtjh <- jags.model("Lecture8_1_SDThierarch.j", 
                   data = list("epsilon"=0.001,
                               "h"=h, "f"=f, "n"=n,
                               "sigtrials" =sigtrials,
                               "noistrials"=noistrials),
                   inits=myinits,
                   n.chains=4)  
# burnin
update(sdtjh,n.iter=1000)  
# perform MCMC
parameters <- c("d", "b", "taud", "taub", "mud", "mub", "phih", "phif") 
mcmcfin<-coda.samples(sdtjh, parameters, 5000)

summary(mcmcfin)
# plot(mcmcfin)

#Listing 8.10 from here on
allpost <- function(mcmcfin,pn) {
  return (unlist(lapply(mcmcfin,FUN=function(x) c(x[,pn]))))
}

print( mean( allpost(mcmcfin,"phih[1]") ))  # predicted from HSDT
print( mean( allpost(mcmcfin,"phih[2]") ))
print( mean( allpost(mcmcfin,"phih[3]") ))
print( mean( allpost(mcmcfin,"phih[4]") ))
print( mean( allpost(mcmcfin,"phih[5]") ))
print( h[1:5] )   # observed data

print( mean( allpost(mcmcfin,"phif[1]") ))  # predicted from HSDT
print( mean( allpost(mcmcfin,"phif[2]") ))
print( mean( allpost(mcmcfin,"phif[3]") ))
print( mean( allpost(mcmcfin,"phif[4]") ))
print( mean( allpost(mcmcfin,"phif[5]") ))
print( f[1:5] )   # observed data
