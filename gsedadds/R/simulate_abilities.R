#' Simulate abilities
#'
#' This function simulates abilities in a specified age interval assuming age is distributed uniformly.
#' @param mo_min - (integer) Lower bound of the age bracket (in months)
#' @param mo_max - (integer) Upper bound of the age bracket (in months)
#' @param distribution_table (data.frame) The tablue used to interpolate mean and SD estimates of abilities. The data frame requires the following named columns: (1) months, (2) mu, & (3) sigma.  
#' @param N - (Integer) Number draws. Defaults to 10,000 draws.
#' @return (numeric vector) Simulated ability values.
#' @export
#' @examples
#' simulate_abilities(mo_min = 3, mo_max = 5.9999, distribution_table = reference)

simulate_abilities<-function(mo_min, mo_max, distribution_table, N = 1E4){
  
  #This function simulates abilities given a model
  #Inputs:
  #   mo_min - (integer) Lower bound of the age bracket (in months)
  #   mo_max - (integer) Upper bound of the age bracket (in months)
  #   distribution_table - (data.frame) The table used to interpolate mean and SD estimates of abilities. The data frame requires the following named columns: (1) months, (2) mu, & (3) sigma.  
  #   N - (Integer) Number draws for sampling abilities
  # Output: 
  #   data.frame with the estimate/approximation of the expected information statistic (est) as well as the standard error of the estimate (se)
  
    
    mu_sigma_df = do.call(what = "linterp_mu_sd", 
                          args = list(distribution_table = distribution_table, 
                                      mo = runif(n = N, min = mo_min, max = mo_max)
                                      )
                    )
  return(rnorm(n = N, mean = mu_sigma_df$mu, sd = mu_sigma_df$sigma))
}