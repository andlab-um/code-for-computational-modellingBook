## 1. fit a power model

#discrepancy function for power forgetting function 
powdiscrep <- function (parms,rec,ri) {                    
  if (any(parms<0)||any(parms>1)) return(1e6)
  pow_pred <- parms["a"] *(parms["b"]*ri + 1)^(-parms["c"])   # prediction
  return(sqrt( sum((pow_pred-rec)^2)/length(ri) ))            # RMSE
}                                                          

#Carpenter et al. (2008) Experiment 1
rec <- c(.93,.88,.86,.66,.47,.34)  # y: recall proportion
ri  <- c(.0035, 1, 2, 7, 14, 42)   # x: retention interval

#initialize starting values 
sparms <- c(1,.05,.7)
names(sparms) <- c("a","b","c")
#obtain best-fitting estimates
pout <- optim(sparms, powdiscrep, rec=rec, ri=ri)             # optimization
pow_pred <- pout$par["a"] *(pout$par["b"]*c(0:max(ri)) + 1)^(-pout$par["c"])   # interpolation

#plot data and best-fitting predictions
x11()
par(cex.axis=1.2,cex.lab=1.4)
par(mar=(c(5, 5, 3, 2) + 0.1),las=1)
plot(ri,rec, 
     xlab = "Retention Interval (Days)", 
     ylab = "Proportion Items Retained",  
     ylim=c(0.3,1),xlim=c(0,43),xaxt="n",type="n")
lines(c(0:max(ri)),pow_pred,lwd=2)
points(ri,rec,pch=21, bg="dark grey",cex=2)
dev <- pow_pred[ri+1]
for (x in c(1:length(ri))) {
  lines(c(ri[x],ri[x]),c(dev[x],rec[x]),lwd=1)
}
axis(1,at=c(0:43))



## 2. fit an exponential model


rm(list=ls())  # clear the vairables

#discrepancy function for exponential forgetting function 
ediscrep <- function (parms,rec,ri) {                    
  if (any(parms<0)||any(parms>1)) return(1e6)
  e_pred <- parms["a"] + parms["b"]*exp( -parms["c"]*ri )  # Prediction
  return(sqrt( sum((e_pred-rec)^2)/length(ri) ))           # RMSE
}                                                          

#Carpenter et al. (2008) Experiment 1
rec <- c(.93,.88,.86,.66,.47,.34)  # y: recall proportion
ri  <- c(.0035, 1, 2, 7, 14, 42)   # x: retention interval

#initialize starting values 
sparms <- c(1,.05,.7)
names(sparms) <- c("a","b","c")
#obtain best-fitting estimates
pout <- optim(sparms, ediscrep, rec=rec, ri=ri)   
e_pred <- pout$par["a"] + pout$par["b"]*exp( -pout$par["c"]*c(0:(max(ri))) )  

#plot data and best-fitting predictions
x11()
par(cex.axis=1.2,cex.lab=1.4)
par(mar=(c(5, 5, 3, 2) + 0.1),las=1)
plot(ri,rec, 
     xlab = "Retention Interval (Days)", 
     ylab = "Proportion Items Retained",  
     ylim=c(0.3,1),xlim=c(0,43),xaxt="n",type="n")
lines(c(0:max(ri)), e_pred, lwd=2)
points(ri,rec,pch=21, bg="dark grey",cex=2)
dev <- e_pred[ri+1]
for (x in c(1:length(ri))) {
  lines(c(ri[x],ri[x]),c(dev[x],rec[x]),lwd=1)
}
axis(1,at=c(0:43))




