#############################################
### COMPUTES EXPECTED MATCH PAYOFF MATRIX ###
#############################################
#Inputs:
#S: matrix of sigmas
#D: matrix of signal likelihoods
#M: matrix of matching values mus
#chi: cursedness parameter (rationallity chi=0)
#Parameter examples:
#S=matrix(sigma,nrow = 3,ncol = 3,byrow = TRUE)
#D=matrix(c(0.5,0.5,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)
#M=matrix(rep(m,3),nrow = 3,ncol = 3,byrow = TRUE)

exp_match_pay<- function(S,D,M,chi){
  PI <- D/matrix(rep(colSums(D),3),nrow = 3,ncol = 3,byrow = TRUE)
  V <- chi * M%*%PI +(1-chi) * ((M*D%*%t(S))%*%D)/(D%*%t(S)%*%D)
  V[,1] <- M[,1]
  V[,3] <- M[,3]
  return(V)
}