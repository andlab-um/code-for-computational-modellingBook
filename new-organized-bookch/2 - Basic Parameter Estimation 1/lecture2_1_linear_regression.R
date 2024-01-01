#plot data and current predictions          
getregpred <- function(parms, data) {
  getregpred <- parms["b0"] + parms["b1"]*data[ ,2]           
  
  # #wait with drawing a graph until key is pressed
  # par(ask=TRUE)
  # plot   (data[ ,2], type="n", las=1, ylim=c(-2,2), xlim=c(-2,2), xlab="X", ylab="Y")
  # par(ask=FALSE)
  # points (data[ ,2], data[ ,1], pch=21, bg="gray")
  # lines  (data[ ,2], getregpred, lty="solid")
  # 
  # return(getregpred)
}                                           

#obtain current predictions and compute discrepancy
rmsd <- function(parms, data1) {          
  preds <- getregpred(parms, data1)  # parms["b0"] + parms["b1"]*data[ ,2]    
  rmsd  <- sqrt(sum((preds-data1[ ,1])^2)/length(preds))  # calculate RMSD
}

#define parameters to generate data
nDataPts  <- 20
rho       <- 0.8
intercept <- 0.0

#generate synthetic data
data <- matrix(0,nDataPts,2)
data[ ,2] <- rnorm(nDataPts)     # x, data, independent variable
data[ ,1] <- rnorm(nDataPts)*sqrt(1.0-rho^2) + data[ ,2]*rho + intercept  # y, output

#do conventional regression analysis
lm1 = lm(data[,1] ~ data[,2])    # lm(y ~ x)
summary(lm1)

#assign starting values 
startParms <- c(-1., .2) 
names(startParms) <- c("b1", "b0")
#obtain parameter estimates using default Simplex method
xout <- optim(startParms, rmsd, data1=data)

#obtain parameter estimates using Simulated Annealing
xout <- optim(startParms, rmsd, data1=data, method='Nelder-Mead')
xout