#' Expected item information
#'
#' This function calculates the expected information contributed by the the j-th item in a provided age interval. Note: Assumes logit link.
#' @param alpha_j - (numeric) discrimination parameter estimate
#' @param tau_j - (numeric) difficulty parameter estimate
#' @param mo_min - (integer) Lower bound of the age bracket (in months)
#' @param mo_max - (integer) Upperbound of the age bracket (in months)
#' @param distribution_table (data.frame) The tablue used to interpolate mean and SD estimates of abilities. The data frame requires the following named columns: (1) months, (2) mu, & (3) sigma.  
#' @param ... Additional arguments passed to simulate_abilities()
#' @return A dataframe with an estimate of the expected information (est) along with the corresponding standard error (se).
#' @export
#' @examples
#' E_Ij(alpha_j=1, tau_j=GCDG_est$tau[1], mo_min=0, mo_max=3, distribution_table=reference, N=5E4)

E_Ij<-function(alpha_j, tau_j, mo_min, mo_max, distribution_table, ...){
  
  #This function calculates the expected information for the j-th item in the interval [month_min, month_max]
  #Inputs:
  #   N - (Integer) Number of Monte Carlo integration points
  #   alpha_j - (numeric) discrimination parameter estimate.
  #   tau_j - (numeric) difficulty parameter estimate
  #   month_min - (integer) Lower bound of the age bracket (in months)
  #   month_max - (integer) Upperbound of the age bracket (in months)
  #   datsrc - (character) One of "CREDI", "GCDG", or "IYCD" 
  #   ... - additional arguments passed to a function that interpolates ability distribution parameters, given age.
  # Output: 
  #   data.frame with an estimate of the expected item information (est) as well as the standard error of the estimate (se)
    

    # Simulate abilitites (assume age distributed uniformly) 
    theta_vec = do.call(what = "simulate_abilities", 
                        args = list(mo_min = mo_min, mo_max = mo_max, 
                                    distribution_table = distribution_table, ... )
                        )
    
    # Calcuate item information at each ability level
    explo_j_vec = exp(alpha_j*(theta_vec - tau_j)) #Exponentiated log odds of item endorsement at each simulated ability
    p_j_vec = explo_j_vec/(1+explo_j_vec) # Probability of item endorsement
    Ij_vec = alpha_j^2*p_j_vec*(1-p_j_vec) # Item information at each simulated ability (see Equation 5.5 in de Ayala, 2009, p. 102)
    
    # Calculate an estimate of the expected ability level given the age interval, as well as a corresponding standard error
    E_Ij = mean(Ij_vec)
    se_E_ij = sd(Ij_vec)/sqrt(length(Ij_vec))
    
  
  return(data.frame(est = E_Ij, se = se_E_ij))
}