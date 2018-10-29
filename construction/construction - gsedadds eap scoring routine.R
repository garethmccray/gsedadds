rm(list= ls())

require(gseddata)
require(gsedadds)

# Acer Laptop
  #dropbox_wd = "C:/Users/marcu/Dropbox/"
# Gaby 
  #dropbox_wd = "Z:/Dropbox/"
# Queen Mary's Revenge
  dropbox_wd = "D:/Dropbox/"
    
construction_wd = paste0(dropbox_wd, "marcus credi/BMGF/Halloween Meeting/gsedadds/construction")


setwd(construction_wd)
#Load in 
load("trial-CREDI_Distribution_Table from post friday ver1_2aa.RData")
load("trial-CREDI_Item_Table from post friday ver1_2aa.RData")


# Obtain measurement parameters
mest_df = item_parameter_table[,c("lex_gsed","alpha","tau")]


# Create some simulated data
N = 36
sim_gsed = data.frame(id = 1:N, agedays = seq(0,36*30.5, len = N)) %>% transform(months = agedays/30.5) 


# Simulate (ability) theta values
musd_df = linterp_mu_sd(distribution_table, mo = sim_gsed$months)
sim_gsed = transform(sim_gsed, theta = NA)
for(i in 1:N){
 sim_gsed$theta[i] = rnorm(n = 1, mean = musd_df$mu[i], sd = musd_df$sigma[i]) 
}

# Simulate item level responses
Y_df = cbind(sim_gsed[,c("id","months")], data.frame(mat.or.vec(nr = N, nc = nrow(item_parameter_table))+NA))
names(Y_df)[-c(1:2)] = as.character(item_parameter_table$lex_gsed)
for(i in 1:N){
  
}

