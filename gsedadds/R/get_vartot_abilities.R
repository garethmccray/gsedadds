#' Total variability of abilities given an age interval
#'
#' This function estimates the total variability of abilities across age bins of a specified interval width between 0-36 months.
#' @param bin_width - (integer) Width of bin in months. Must be a multiple of 36.
#' @param distribution_table (data.frame) The tablue used to interpolate mean and SD estimates of abilities. The data frame requires the following named columns: (1) months, (2) mu, & (3) sigma.  
#' @param ... Additional arguments passed to simulate_abilities()
#' @return A dataframe with estimated values for the total variability of abilities in each age bin .
#' @export
#' @examples
#' get_vartot_abilities(mo_min = 3, mo_max = 5.9999, distribution_table = reference)

get_vartot_abilities<-function(bin_width=3, distribution_table, ...){
  
  require(plyr)
  require(tidyverse)  

  if(36%%bin_width!=0){stop("bin_width must be a multiple of 36.")}

  months_vec = seq(0,36,by=bin_width)
  nn = length(months_vec)
  
  var_abilities_df = data.frame(month_min = months_vec[-nn]) %>% 
                      transform(month_max = months_min+bin_width-1E-4) %>% 
                      transform(bin = mapvalues(month_min, from = months_vec[-nn], to = seq(1,nn-1,by=1)), var_est= NA)
  
  for (i in 1:nrow(var_abilities_df)){
    theta_vec_i = do.call(what = "simulate_abilities", 
                          args = list(mo_min = var_abilities_df$month_min[i], mo_max = var_abilities_df$month_max[i]-1E-4, 
                                      distribution_table = distribution_table, ... )
                  )
    var_abilities_df$var_est[i] = var(theta_vec_i)
  }
  
  return(var_abilities_df)

}
