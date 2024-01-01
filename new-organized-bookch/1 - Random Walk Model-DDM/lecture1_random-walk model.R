# random-walk model 
nreps <- 1000    # number of trials
nsamples <- 2000  # samples to collect evidence for each decision

drift <- 0.02        # noninformative stimulus 
sdrw <- 0.3          # s.t.d in the evidence
criterion <- 3      # distance between boundary and baseline

latencies <- rep (0, nreps)  # a vector to store the simulated response latencies
responses <- rep (0, nreps)  # a vector to store the simulated responses

evidence <- matrix (0, nreps, nsamples+1)  # a matrix to store the evidence for each response

for (i in c(1:nreps)) {
  evidence[i ,] <- cumsum ( c ( 0 , rnorm ( nsamples , drift , sdrw ) ) )
  p <- which ( abs (evidence[i, ] ) >= criterion) [1]   # find the first evidence bigger than criterion
  responses[i] <- sign (evidence[i,p])    # responses: positive (left) or negative (right)
  latencies[i] <- p                       # response latency
}


## plot up to 5 random-walk paths 
tbpn <- min(nreps, 5)
plot(1:max(latencies[1:tbpn])+10,type='n',las = 1,
     ylim = c(-criterion, criterion),
     ylab = 'Evidence', xlab='Decision time') 
for (i in c(1:tbpn)){
  lines(evidence[i, 1:(latencies[i])])
}
abline(h=c(criterion, -criterion), lty='dashed')


## plot histograms of latencies 
par(mfrow=c(2 , 1))   # to set graphical parameters (2 subplot)
toprt <- latencies[responses>0]   # response time for the left response
topprop <- length(toprt)/nreps    # probability to choose the left response
hist(toprt, col='gray',
     xlab='Decision time', xlim=c(0,max(latencies)),
     main=paste('Top responses (', as.numeric(topprop),')m=', as.character(signif(mean(toprt),4)),
                 sep=''),las=1)   # plot the historgram

botrt <- latencies[responses<0]
botprop <- length(botrt)/nreps
hist(botrt,col="gray",
     xlab="Decision time",xlim=c(0,max(latencies)),
     main=paste("Bottom responses (",as.numeric(botprop),
                ") m=",as.character(signif(mean(botrt),4)),
                sep=""),las=1)

