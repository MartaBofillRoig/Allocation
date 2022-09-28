
##########################################
# Optimal allocation
# Case study
# 2022-09-28
# Marta Bofill Roig
##########################################

devtools::install_github("pavlakrotka/NCC@v1.0")
library(NCC)

# original study
n_control = 31
n_arm1 = 31
n_arm2 = 30
N = n_control + n_arm1 + n_arm2

# means
mean_control = 17.3/3.5
mean_arm1 = 66.2/3.5
mean_arm2 = 72.3/3.5

##########################################
# Design 1: only 1 period, multi-arm trial
##########################################

dsim_design1 <- function(n_c,n_a1,n_a2,mu_c,mu_a1,mu_a2){
  
  response = c(rnorm(n_c, mean=mu_c, sd=1),
               rnorm(n_a1, mean=mu_a1, sd=1),
               rnorm(n_a2, mean=mu_a2, sd=1))
  treatment = c(rep(0,n_c),
                rep(1,n_a1),
                rep(2,n_a2))
  
  period = c(rep(1,n_c + n_a1 + n_a2))
  
  data = data.frame(response,treatment,period)
  
}

data=dsim_design1(n_c=n_control,n_a1=n_arm1,n_a2=n_arm2,mu_c=mean_control,mu_a1=mean_arm1,mu_a2=mean_arm2)

head(data)
dim(data)

models <- function(data,alpha=0.025){
  
  lf_a1=fixmodel_cont(data,arm=1,alpha=0.025)
  lf_a2=fixmodel_cont(data,arm=2,alpha=0.025)
  
  ls_a1=sepmodel_adj_cont(data,arm=1,alpha=0.025)
  ls_a2=sepmodel_adj_cont(data,arm=2,alpha=0.025) 
  
  return(list(lf_a1,lf_a2,ls_a1,ls_a2))
}

 






