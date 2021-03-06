################################################################################################################################
### COMPUTES THE (minus-log) LIKELIHOOD OF THE CQRE MODEL ON DATA FROM ALL TREATMENTS RESTRICTED TO SAME CHI IN BASE AND BEL ###
################################################################################################################################

likelihood_joint_restricted <- function(lambda, chi){
  treat=(treatment=="BASE")
  sigma_A=solve_qre(D = D_BASE, R = R_BASE_A, M = M_BASE, lambda, chi)
  sigma_B=solve_qre(D = D_BASE, R = R_BASE_B, M = M_BASE, lambda, chi)
  sigma=c(sigma_A,sigma_B)
  index= 9*(game[treat]-1)+3*(type[treat]-1)+signal[treat]
  sigma_data=sigma[index]
  nll_BASE=-sum(propose[treat]*log(sigma_data)+(1-propose[treat])*log(1-sigma_data))
  treat=(treatment=="BEL")
  sigma_A=qre_probabilities(S_BEL_A,D_BEL,M_BEL,R_BEL,lambda,chi)
  sigma_B=qre_probabilities(S_BEL_B,D_BEL,M_BEL,R_BEL,lambda,chi)
  sigma_C=qre_probabilities(S_BEL_C,D_BEL,M_BEL,R_BEL,lambda,chi)
  sigma_D=qre_probabilities(S_BEL_D,D_BEL,M_BEL,R_BEL,lambda,chi)
  sigma_E=qre_probabilities(S_BEL_E,D_BEL,M_BEL,R_BEL,lambda,chi)
  sigma=c(sigma_A[2,1:3],sigma_B[2,1:3],sigma_C[2,1:3],sigma_D[2,1:3],sigma_E[2,1:3])
  index= 3*(game[treat]-1)+signal[treat]
  sigma_data=sigma[index]
  nll_BEL=-sum(propose[treat]*log(sigma_data)+(1-propose[treat])*log(1-sigma_data))
  treat=(treatment=="COND")
  sigma_A=qre_probabilities(S_COND,D_COND_A,M_COND,R_COND,lambda,0)
  sigma_B=qre_probabilities(S_COND,D_COND_B,M_COND,R_COND,lambda,0)
  sigma_C=qre_probabilities(S_COND,D_COND_C,M_COND,R_COND,lambda,0)
  sigma_D=qre_probabilities(S_COND,D_COND_D,M_COND,R_COND,lambda,0)
  sigma_E=qre_probabilities(S_COND,D_COND_E,M_COND,R_COND,lambda,0)
  sigma=c(sigma_A[2,1:3],sigma_B[2,1:3],sigma_C[2,1:3],sigma_D[2,1:3],sigma_E[2,1:3])
  index= 3*(game[treat]-1)+signal[treat]
  sigma_data=sigma[index]
  nll_COND=-sum(propose[treat]*log(sigma_data)+(1-propose[treat])*log(1-sigma_data))
  nll=nll_BASE+nll_BEL+nll_COND
  return(nll)
}