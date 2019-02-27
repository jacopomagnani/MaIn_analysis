#####################################################################
### COMPUTES DISTANCE BETWEEN GIVEN SIGMA AND QRE-PREDICTED SIGMA ###
#####################################################################

#Inputs:
#s: vector of sigmas (not in matrix form, beause not allowed when optim this function )
#D: matrix of signal likelihoods
#M: matrix of matching values mus
#R: matrix of reservation values rhos
#chi: cursedness parameter (rationallity chi=0)
#lambda: QRE parameter
#Parameter examples:
#s=c(1,0,0,1,1,0,1,1,1)
#R=matrix(c(100,100,100,75,75,75,25,25,25),nrow = 3,ncol = 3,byrow = TRUE)
#M=matrix(c(160,80,40,160,80,40,160,80,40),nrow = 3,ncol = 3,byrow = TRUE)
#D=matrix(c(0.5,0.5,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)


# alternative expression for delta (but this does not include cursedness):
# delta=prob_accept(sigma)* exp_match_pay(sigma) + (1-prob_accept(sigma))*R - R
# delta=prob_accept(sigma)*(  exp_match_pay(sigma) - R  )
# prob_accept=p%*%t(sigma)%*%p  / P_sum
# where P_sum=matrix(rep(colSums(P),3),nrow = 3,ncol = 3,byrow = TRUE)
# exp_match_pay= ((M*P%*%t(S))%*%P) / P%*%t(S)%*%P
# substituting: delta= ( ((M*P%*%t(S))%*%P) - P%*%t(S)%*%P* R ) / P_sum


qre_distance<-function(s,lambda,chi,D,R,M){
  S=matrix(s,nrow = 3,ncol = 3,byrow = TRUE)
  delta = exp_match_pay(S,D,M,chi) - R
  diff=S-1/(1/exp(lambda*(delta))+1)
  dist<-sum(diff^2)
  return(dist)	
}


