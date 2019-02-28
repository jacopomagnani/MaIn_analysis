###############################################################################
### computes probability that j proposes conditional on i's type and signal ###
###############################################################################

prob_accept<- function(sigma){
  p=matrix(c(0.5,0.5,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)
  #  v= m%*%(p[,s]*(sigma%*%p[t,]))  / ( p[,s]%*%(sigma%*%p[t,]))
  #V=((M*p%*%t(sigma))%*%p) / p%*%t(sigma)%*%p
  prob= p%*%t(sigma)%*%p  / matrix(rep(colSums(p),3),nrow = 3,ncol = 3,byrow = TRUE)
  return(prob)
}