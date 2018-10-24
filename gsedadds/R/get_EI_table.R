#' Obtain expected item information table
#'
#' This function is a wrapper to obtain the expected item information statistics for each item and across age bins.
#' @params_df - (data.frame) that has lex_gsed, tau, & alpha (optional) as variable names. If alpha is not included, a Rasch is model assumed. 
#' @param distribution_table (data.frame) The tablue used to interpolate mean and SD estimates of abilities. The data frame requires the following named columns: (1) months, (2) mu, & (3) sigma.  
#' @param N - (integer) Number of draws for Monte Carlo integration.
#' @param bin_width - (integer) Width of bin in months. Must be a multiple of 36.
#' @param processors (integer) Number of processors for parallel computing. Defaults to total detected by parallel::detectCores().
#' @param seed (integer) Random number generating seed used in doRNG package during parallel processing.
#' @return A data frame expected information statistic for each item (rows) and in each age bin (columns).
#' @export
#' @examples
#' get_EI_table(params_df = GCDG_est, distribution_table = reference, N = 1E6, bin_width=3, processors = 16, seed = 42)


get_EI_table<-function(params_df, distribution_table, N = 1E5, bin_width=3,  processors = NULL, seed = 4242){


    require(doParallel)
    require(foreach)
    require(doRNG)
    require(plyr)
    require(tidyverse)  
    require(miceadds)
    require(data.table)
    
    # If alpha is not in params_df, create assuming it takes a value of 1 for all observations
    if("alpha"%in%names(params_df) == FALSE){params_df = transform(params_df, alpha = 1)}
    
    # Establish age bins
    if(36%%bin_width!=0){stop("bin_width must be a multiple of 36.")}
    months_vec = seq(0,36,by=bin_width)
    nn = length(months_vec)
    
      
    # Create a conditions table that deliminates the item information statistics that need to be calculated 
    conditions_df = expand.grid(lex_gsed = params_df$lex_gsed, month_min = months_vec[-nn]) %>% transform(month_max = month_min+bin_width-1E-4) %>% 
      transform(bin = mapvalues(month_min, from = months_vec[-nn], to = seq(1,nn-1,by=1)))
    
    
    # Construct labels for the age bins
    bin_lbls =  rep(NA, nn-1)
    for(ag in seq(1,nn-1,by=1)){bin_lbls[ag] = paste0("month",months_vec[ag],"to",months_vec[ag+1])}
    conditions_df$bin = ordered(conditions_df$bin, levels = seq(1,nn-1,by=1), 
                                    labels = bin_lbls)
    
    # Bring in measurement parameters into the conditions table
    conditions_df = merge(x = params_df[,c("lex_gsed","alpha","tau")], y = conditions_df, sort = FALSE)
    
    
    if (is.null(processors)){processors=parallel::detectCores()}
    
    
    cl<-makePSOCKcluster(processors)
    registerDoParallel(cl)
    
    
    out_foreach<-foreach(ja = 1:nrow(conditions_df), .inorder = FALSE, .options.RNG = seed, .packages = "miceadds") %dorng% {
        
      EIj_df = do.call(what = "E_Ij", 
                          args = list(alpha_j=conditions_df$alpha[ja], 
                                      tau_j=conditions_df$tau[ja], 
                                      mo_min=conditions_df$month_min[ja], 
                                      mo_max= conditions_df$month_max[ja],
                                      distribution_table = distribution_table, N = N)
                      )
    
      return(cbind(conditions_df[ja, ], EIj_df))
      
    }
    stopImplicitCluster()
    
    
    EI_table = rbindlist(out_foreach) %>% 
      select("lex_gsed","bin","est") %>%
      spread(bin, est)
    
    return(data.frame(EI_table))

}