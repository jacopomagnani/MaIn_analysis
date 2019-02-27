### COMPUTES THE (minus-log) LIKELIHOOD OF THE CQRE MODEL ON DATA FROM BASE TREATMENT ###


likelihood_BASE <- function(lambda, chi){
  treat=(treatment=="BASE")
  sigma_A=solve_qre(D = D_BASE, R = R_BASE_A, M = M_BASE, lambda, chi)
  sigma_B=solve_qre(D = D_BASE, R = R_BASE_B, M = M_BASE, lambda, chi)
  sigma=c(sigma_A,sigma_B)
  index= 9*(game[treat]-1)+3*(type[treat]-1)+signal[treat]
  sigma_data=sigma[index]
  nll=-sum(propose[treat]*log(sigma_data)+(1-propose[treat])*log(1-sigma_data))
  return(nll)
}