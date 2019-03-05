#######################################################
### COMPUTES QRE PROBABILITIES GIVEN ALL PARAMETERS ###
#######################################################

#Inputs:
#S: matrix of sigmas
#D: matrix of signal likelihoods
#M: matrix of matching values mus
#R: matrix of reservation values rhos
#chi: cursedness parameter (rationallity chi=0)
#lambda: QRE parameter
qre_probabilities <- function(S,D,M,R,lambda,chi){
  delta  <-  prob_accept(S,D) * (exp_match_pay(S,D,M,chi) - R)
  probs <- 1/(1/exp(lambda*(delta))+1)
  return(probs)
}