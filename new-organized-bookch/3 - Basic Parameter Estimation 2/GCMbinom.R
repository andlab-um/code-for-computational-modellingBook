#setwd("/Users/huahua/Documents/ML&NeuroEngineering_2020spring/5/code_Chapter4")
source("GCMpred.R")

N <- 2*80 # there were 2 responses per face from 80 subjects, in total 160 responses
N_A <- 155 # the number of A response. N_B is implicitly N - N_A
   
c <- 4        # a parameter for calculating similarity
w <- c(0.19, 0.12, 0.25, 0.45)   # parameters for the weights, corresponding to the 4 dimensions

# read the 4d features of face stimuli
stim <- as.matrix(read.table("faceStim.csv", sep=",")) 

# two categories (a & b) of examplars which have been stored in your memory
exemplars <- list(a=stim[1:5,], b= stim[6:10,]) 

preds <- GCMpred(stim[21,], exemplars, c, w) # the probability of A response
preds

likelihood <- dbinom(N_A, size = N, prob = preds[1])
likelihood

