##################################
###  NUMERICAL SOLUTION OF QRE ###
##################################

#Parameters:
#D: matrix of signal likelihoods
#M: matrix of matching values mus
#R: matrix of reservation values rhos
#chi: cursedness parameter (rationality chi=0)
#lambda: QRE parameter
#Parameter examples:
#R=matrix(c(100,100,100,75,75,75,25,25,25),nrow = 3,ncol = 3,byrow = TRUE)
#M=matrix(c(160,80,40,160,80,40,160,80,40),nrow = 3,ncol = 3,byrow = TRUE)
#D=matrix(c(0.5,0.5,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)


solve_qre<-function(D,R,M,lambda,chi){
  optimiz<-optim(par = rep(0.5,9),
                 fn=qre_distance,
                 gr=NULL,
                 method="L-BFGS-B",
                 lower=rep(0.001,9),
                 upper=rep(0.999,9),
                 D=D,
                 R=R,
                 M=M,
                 lambda=lambda,
                 chi=chi,
                 control=list("fnscale"=1,"parscale"=c(1,1,1,1,1,1,1,1,1))
                 )
  return(optimiz$par)
}