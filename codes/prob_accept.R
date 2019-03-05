###############################################################################
### COMPUTES PROBABILITY THAT j PROPOSES CONDITIONAL ON i'S TYPE ##############
###############################################################################
#Inputs:
#S: matrix of sigmas
#D: matrix of signal likelihoods


prob_accept<- function(S,D){
  P_sum <- matrix(rep(colSums(D),3),nrow = 3,ncol = 3,byrow = TRUE)
  prob <- D%*%t(S)%*%D  / P_sum
  return(prob)
}