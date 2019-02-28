#########################################################################################
### COMPUTES THE (minus-log) LIKELIHOOD OF THE CQRE MODEL ON DATA FROM BEL TREATMENT ###
#########################################################################################

likelihood_BEL <- function(lambda, chi){
  treat=(treatment=="BEL")
  sigma_A=1/(1/exp(lambda*(exp_match_pay(S_BEL_A,D_BEL,M_BEL,chi) - R_BEL))+1)
  sigma_B=1/(1/exp(lambda*(exp_match_pay(S_BEL_B,D_BEL,M_BEL,chi) - R_BEL))+1)
  sigma_C=1/(1/exp(lambda*(exp_match_pay(S_BEL_C,D_BEL,M_BEL,chi) - R_BEL))+1)
  sigma_D=1/(1/exp(lambda*(exp_match_pay(S_BEL_D,D_BEL,M_BEL,chi) - R_BEL))+1)
  sigma_E=1/(1/exp(lambda*(exp_match_pay(S_BEL_E,D_BEL,M_BEL,chi) - R_BEL))+1)
  sigma=c(sigma_A[2,1:3],sigma_B[2,1:3],sigma_C[2,1:3],sigma_D[2,1:3],sigma_E[2,1:3])
  index= 3*(game[treat]-1)+signal[treat]
  sigma_data=sigma[index]
  nll=-sum(propose[treat]*log(sigma_data)+(1-propose[treat])*log(1-sigma_data))
  return(nll)
}