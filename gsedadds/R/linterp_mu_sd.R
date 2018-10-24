#' Interpolate means and SD of abilities
#'
#' This function applies linear interpolation from a reference distribution table to obtain mean and standard devations of abilities at specified months of development.
#' @param distribution_table (data.frame) The tablue used to interpolate mean and SD estimates of abilities. The data frame requires the following named columns: (1) months, (2) mu, & (3) sigma.  
#' @param mo (numeric vector) Months where interpolated mean and SD estimates will be interpolated. Must be between 0<=mo<=60.
#' @return (data.frame) Interpolated means (mu) and standard deviations (sigma)
#' @export
#' @examples
#' linterp_mu_sd(distribution_table = reference, mo = seq(0,36, len = 100))

linterp_mu_sd<-function(distribution_table, mo){
  
  require(stats)
  out_df = data.frame(mu = rep(NA, length(mo)), sigma= rep(NA, length(mo)))
  mu_vec = approx(x = distribution_table$month, y = distribution_table$mu, xout = mo, method = "linear")$y
  sigma_vec = approx(x = distribution_table$month, y = distribution_table$sigma, xout = mo, method = "linear")$y
  
  return(data.frame(month = mo, mu = mu_vec, sigma = sigma_vec))
}
