### COMPUTES THE (minus-log) LIKELIHOOD OF THE CQRE MODEL ON DATA FROM COND TREATMENT ###

likelihood_COND <- function(lambda){
  treat=(treatment=="COND")
  sigma_A=1/(1/exp(lambda*(exp_match_pay(S_COND,D_COND_A,M_COND,0) - R_COND))+1)
  sigma_B=1/(1/exp(lambda*(exp_match_pay(S_COND,D_COND_B,M_COND,0) - R_COND))+1)
  sigma_C=1/(1/exp(lambda*(exp_match_pay(S_COND,D_COND_C,M_COND,0) - R_COND))+1)
  sigma_D=1/(1/exp(lambda*(exp_match_pay(S_COND,D_COND_D,M_COND,0) - R_COND))+1)
  sigma_E=1/(1/exp(lambda*(exp_match_pay(S_COND,D_COND_E,M_COND,0) - R_COND))+1)
  sigma=c(sigma_A[2,1:3],sigma_B[2,1:3],sigma_C[2,1:3],sigma_D[2,1:3],sigma_E[2,1:3])
  index= 3*(game[treat]-1)+signal[treat]
  sigma_data=sigma[index]
  nll=-sum(propose[treat]*log(sigma_data)+(1-propose[treat])*log(1-sigma_data))
  return(nll)
}