
##########################################
# Optimal allocation
# Simulation study
# 2022-Oct
# Marta Bofill Roig
##########################################

rm(list = ls())

setwd("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/case-study")#local
# setwd("~/GitHub/Allocation/case-study")


library(tidyverse)
library(dplyr)
# install.packages("plyr")
library(plyr)
# devtools::install_github("pavlakrotka/NCC@v1.1", force=T)
library(NCC)
# devtools::install_github("ianmoran11/mmtable2")
library(mmtable2)
# install.packages("gt")
library(gt)


##########################################
# Functions

source("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/case-study/aux_functions.R") #local
# source("~/GitHub/Allocation/case-study/aux_functions.R") #server

##########################################

# syntaxix 
# sim_designs(r1,r2,mu0,mu1,mu2,N,alloc="sqrt",sl=0.2) 

# Examples: 
r1=20/70;r2=30/70;mu0=0;mu1=1;mu2=1;N=70;alloc="opt"
db=sim_designs(r1=20/70,r2=30/70,mu0=0,mu1=1,mu2=1,N=70,alloc="one")
# head(db)
# plot_trial(db$data$treatment)
# 
# db=sim_designs(r1=20/70,r2=30/70,mu0=0,mu1=1,mu2=1,N=70,alloc="opt")
# head(db$data)
# plot_trial(db$data$treatment)
# 
# op <- fixmodel_cont(data=db$data,arm=2,alpha=0.025)# with using ncc
# summary(op$model)
# fixmodel_cont(data=db$data,arm=2,alpha=0.025,ncc=F)# without using ncc

##########################################
# Case study
##########################################

# original study - obtained means
mean_control = 17.3/3.5
mean_arm1 = 66.2/3.5
mean_arm2 = 72.3/3.5 

##########################################
# Simulations
##########################################

# nsim=100000
nsim=10

set.seed(4561)
df_res = data.frame(rt_a1=c(0),rt_a2=c(0),r1=c(0),r2=c(0),mu0=c(0),mu1=c(0),mu2=c(0),N=c(0),alloc=c("one"),trend=c(0),H0=F,var_e1=c(0), var_e2=c(0))
i=1


library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

##########################################
# design 3: three-period design (symmetric design)
##########################################
N = 92
N1 = round(N/3)
N3 = round(N/3)
N2 = N-N1-N3
c(N1,N2,N-N1-N2)



##########################################
# Power comparison
##########################################
# 1:1 allocation
y_sym = rdply(nsim,
          do.call(rbind.data.frame,
                  models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                               mu0=mean_control,mu1=6,mu2=6,
                                               N=N,alloc="one",sl=0)$data
                  )
          )
)  

# t1e / power
y1_t1e <- y_sym %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
e1_t1e <- sum(y1_t1e)/nsim

var_e1 <- mean((y_sym %>% filter(arm == "a1"))$var )

y2_t1e <- y_sym %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
e2_t1e <- sum(y2_t1e)/nsim

var_e2 <- mean((y_sym %>% filter(arm == "a2"))$var )

df_res[i,]=c(rt_a1=e1_t1e,rt_a2=e2_t1e, r1=N1/N,r2=N2/N,
             mu0=mean_control,mu1=6,mu2=6,
             N=N,alloc="one",trend=0,H0=F,
             var_e1=var_e1, var_e2=var_e2) 

i=i+1

##########################################
# optimal allocation
y_sym_opt = rdply(nsim,
              do.call(rbind.data.frame,
                      models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                                   mu0=mean_control,mu1=6,mu2=6,
                                                   N=N,alloc="opt",sl=0)$data
                      )
              )
) 

# t1e / power
y1_t1e <- y_sym_opt %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
e1_t1e <- sum(y1_t1e)/nsim

y2_t1e <- y_sym_opt %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
e2_t1e <- sum(y2_t1e)/nsim

var_e1 <- mean((y_sym_opt %>% filter(arm == "a1"))$var )
var_e2 <- mean((y_sym_opt %>% filter(arm == "a2"))$var )

df_res[i,]=c(rt_a1=e1_t1e,rt_a2=e2_t1e, r1=N1/N,r2=N2/N,
             mu0=mean_control,mu1=6,mu2=6,
             N=N,alloc="opt",trend=0,H0=F,
             var_e1=var_e1, var_e2=var_e2)

i=i+1 

##########################################
# sqrt k allocation

y_sym_sqrt = rdply(nsim,
               do.call(rbind.data.frame,
                       models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                                    mu0=mean_control,mu1=6,mu2=6,
                                                    N=N,alloc="sqrt",sl=0)$data
                       )
               )
) 

# t1e / power
y1_t1e <- y_sym_sqrt %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
e1_t1e <- sum(y1_t1e)/nsim

y2_t1e <- y_sym_sqrt %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
e2_t1e <- sum(y2_t1e)/nsim

var_e1 <- mean((y_sym_sqrt %>% filter(arm == "a1"))$var )
var_e2 <- mean((y_sym_sqrt %>% filter(arm == "a2"))$var )

df_res[i,]=c(rt_a1=e1_t1e,rt_a2=e2_t1e,r1=N1/N,r2=N2/N,
             mu0=mean_control,mu1=6,mu2=6,
             N=N,alloc="sqrt",trend=0,H0=F,
             var_e1=var_e1, var_e2=var_e2)

i=i+1

list_res_sym_H1 = list(y_sym,y_sym_opt,y_sym_sqrt)

##########################################
##########################################
# T1E comparison
##########################################
# 1:1 allocation
y_sym = rdply(nsim,
          do.call(rbind.data.frame,
                  models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                               mu0=mean_control,mu1=mean_control,mu2=mean_control,
                                               N=N,alloc="one",sl=0)$data
                  )
          )
) 

# t1e / power
y1_t1e <- y_sym %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
e1_t1e <- sum(y1_t1e)/nsim

y2_t1e <- y_sym %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
e2_t1e <- sum(y2_t1e)/nsim

var_e1 <- mean((y_sym %>% filter(arm == "a1"))$var )
var_e2 <- mean((y_sym %>% filter(arm == "a2"))$var )

df_res[i,]=c(rt_a1=e1_t1e,rt_a2=e2_t1e,r1=N1/N,r2=N2/N,
             mu0=mean_control,mu1=mean_control,mu2=mean_control,
             N=N,alloc="one",trend=0,H0=T,
             var_e1=var_e1, var_e2=var_e2)

i=i+1 

##########################################
# optimal allocation
y_sym_opt = rdply(nsim,
              do.call(rbind.data.frame,
                      models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                                   mu0=mean_control,mu1=mean_control,mu2=mean_control,
                                                   N=N,alloc="opt",sl=0)$data
                      )
              )
) 

# t1e / power
y1_t1e <- y_sym_opt %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
e1_t1e <- sum(y1_t1e)/nsim

y2_t1e <- y_sym_opt %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
e2_t1e <- sum(y2_t1e)/nsim

var_e1 <- mean((y_sym_opt %>% filter(arm == "a1"))$var )
var_e2 <- mean((y_sym_opt %>% filter(arm == "a2"))$var )

df_res[i,]=c(rt_a1=e1_t1e,rt_a2=e2_t1e,r1=N1/N,r2=N2/N,
             mu0=mean_control,mu1=mean_control,mu2=mean_control,
             N=N,alloc="opt",trend=0,H0=T,
             var_e1=var_e1, var_e2=var_e2)

i=i+1 

##########################################
# sqrt k allocation

y_sym_sqrt = rdply(nsim,
               do.call(rbind.data.frame,
                       models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                                    mu0=mean_control,mu1=mean_control,mu2=mean_control,
                                                    N=N,alloc="sqrt",sl=0)$data
                       )
               )
) 

# t1e / power
y1_t1e <- y_sym_sqrt %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
e1_t1e <- sum(y1_t1e)/nsim

y2_t1e <- y_sym_sqrt %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
e2_t1e <- sum(y2_t1e)/nsim

var_e1 <- mean((y_sym_sqrt %>% filter(arm == "a1"))$var )
var_e2 <- mean((y_sym_sqrt %>% filter(arm == "a2"))$var )

df_res[i,]=c(rt_a1=e1_t1e,rt_a2=e2_t1e,r1=N1/N,r2=N2/N,
             mu0=mean_control,mu1=mean_control,mu2=mean_control,
             N=N,alloc="sqrt",trend=0,H0=T,
             var_e1=var_e1, var_e2=var_e2)

i=i+1 

df_res
list_res_sym_H0 = list(y_sym,y_sym_opt,y_sym_sqrt)

##########################################
# design 3: three-period design (non-symmetric design)
##########################################
N = 92
N1 = round(N/3)
N2 = round(2*(N-N1)/3)
c(N1,N2,N-N1-N2)

 

##########################################
# Power comparison
##########################################
# 1:1 allocation
y_nsym = rdply(nsim,
          do.call(rbind.data.frame,
                  models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                               mu0=mean_control,mu1=6,mu2=6,
                                               N=N,alloc="one",sl=0)$data
                  )
          )
)  

# t1e / power
y1_t1e <- y_nsym %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
e1_t1e <- sum(y1_t1e)/nsim

y2_t1e <- y_nsym %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
e2_t1e <- sum(y2_t1e)/nsim

var_e1 <- mean((y_nsym %>% filter(arm == "a1"))$var )
var_e2 <- mean((y_nsym %>% filter(arm == "a2"))$var )

df_res[i,]=c(rt_a1=e1_t1e,rt_a2=e2_t1e, r1=N1/N,r2=N2/N,
             mu0=mean_control,mu1=6,mu2=6,
             N=N,alloc="one",trend=0,H0=F,
             var_e1=var_e1, var_e2=var_e2) 

i=i+1

##########################################
# optimal allocation
y_nsym_opt = rdply(nsim,
              do.call(rbind.data.frame,
                      models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                                   mu0=mean_control,mu1=6,mu2=6,
                                                   N=N,alloc="opt",sl=0)$data
                      )
              )
) 

# t1e / power
y1_t1e <- y_nsym_opt %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
e1_t1e <- sum(y1_t1e)/nsim

y2_t1e <- y_nsym_opt %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
e2_t1e <- sum(y2_t1e)/nsim

var_e1 <- mean((y_nsym_opt %>% filter(arm == "a1"))$var )
var_e2 <- mean((y_nsym_opt %>% filter(arm == "a2"))$var )

df_res[i,]=c(rt_a1=e1_t1e,rt_a2=e2_t1e, r1=N1/N,r2=N2/N,
             mu0=mean_control,mu1=6,mu2=6,
             N=N,alloc="opt",trend=0,H0=F,
             var_e1=var_e1, var_e2=var_e2)

i=i+1 

##########################################
# sqrt k allocation

y_nsym_sqrt = rdply(nsim,
              do.call(rbind.data.frame,
                      models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                                   mu0=mean_control,mu1=6,mu2=6,
                                                   N=N,alloc="sqrt",sl=0)$data
                      )
              )
) 

# t1e / power
y1_t1e <- y_nsym_sqrt %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
e1_t1e <- sum(y1_t1e)/nsim

y2_t1e <- y_nsym_sqrt %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
e2_t1e <- sum(y2_t1e)/nsim

var_e1 <- mean((y_nsym_sqrt %>% filter(arm == "a1"))$var )
var_e2 <- mean((y_nsym_sqrt %>% filter(arm == "a2"))$var )

df_res[i,]=c(rt_a1=e1_t1e,rt_a2=e2_t1e,r1=N1/N,r2=N2/N,
             mu0=mean_control,mu1=6,mu2=6,
             N=N,alloc="sqrt",trend=0,H0=F,
             var_e1=var_e1, var_e2=var_e2)

i=i+1

list_res_nsym_H1 = list(y_nsym,y_nsym_opt,y_nsym_sqrt)

##########################################
##########################################
# T1E comparison
##########################################
# 1:1 allocation
y_nsym = rdply(nsim,
          do.call(rbind.data.frame,
                  models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                               mu0=mean_control,mu1=mean_control,mu2=mean_control,
                                               N=N,alloc="one",sl=0)$data
                  )
          )
)  

# t1e / power
y1_t1e <- y_nsym %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
e1_t1e <- sum(y1_t1e)/nsim

y2_t1e <- y_nsym %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
e2_t1e <- sum(y2_t1e)/nsim

var_e1 <- mean((y_nsym %>% filter(arm == "a1"))$var )
var_e2 <- mean((y_nsym %>% filter(arm == "a2"))$var )

df_res[i,]=c(rt_a1=e1_t1e,rt_a2=e2_t1e,r1=N1/N,r2=N2/N,
             mu0=mean_control,mu1=mean_control,mu2=mean_control,
             N=N,alloc="one",trend=0,H0=T,
             var_e1=var_e1, var_e2=var_e2)

i=i+1 

##########################################
# optimal allocation
y_nsym_opt = rdply(nsim,
              do.call(rbind.data.frame,
                      models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                                   mu0=mean_control,mu1=mean_control,mu2=mean_control,
                                                   N=N,alloc="opt",sl=0)$data
                      )
              )
) 

# t1e / power
y1_t1e <- y_nsym_opt %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
e1_t1e <- sum(y1_t1e)/nsim

y2_t1e <- y_nsym_opt %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
e2_t1e <- sum(y2_t1e)/nsim

var_e1 <- mean((y_nsym_opt %>% filter(arm == "a1"))$var )
var_e2 <- mean((y_nsym_opt %>% filter(arm == "a2"))$var )



df_res[i,]=c(rt_a1=e1_t1e,rt_a2=e2_t1e,r1=N1/N,r2=N2/N,
             mu0=mean_control,mu1=mean_control,mu2=mean_control,
             N=N,alloc="opt",trend=0,H0=T,
             var_e1=var_e1, var_e2=var_e2)

i=i+1 

##########################################
# sqrt k allocation

y_nsym_sqrt = rdply(nsim,
               do.call(rbind.data.frame,
                       models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                                    mu0=mean_control,mu1=mean_control,mu2=mean_control,
                                                    N=N,alloc="sqrt",sl=0)$data
                       )
               )
) 

# t1e / power
y1_t1e <- y_nsym_sqrt %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
e1_t1e <- sum(y1_t1e)/nsim

y2_t1e <- y_nsym_sqrt %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
e2_t1e <- sum(y2_t1e)/nsim

var_e1 <- mean((y_nsym_sqrt %>% filter(arm == "a1"))$var )
var_e2 <- mean((y_nsym_sqrt %>% filter(arm == "a2"))$var )

df_res[i,]=c(rt_a1=e1_t1e,rt_a2=e2_t1e,r1=N1/N,r2=N2/N,
             mu0=mean_control,mu1=mean_control,mu2=mean_control,
             N=N,alloc="sqrt",trend=0,H0=T,
             var_e1=var_e1, var_e2=var_e2)

i=i+1 

df_res
list_res_nsym_H0 = list(y_nsym,y_nsym_opt,y_nsym_sqrt)

##########################################
# save.image("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/case-study/results/simstudy_results_3periods_withvar.RData") #local
# save.image("~/GitHub/Allocation/case-study/results/simstudy_results_3periods_withvar.RData") #server


##########################################
# design 2: two-period design  
########################################## 

N = 92
N1 = round(N/4)
N2 = round(N-N1)
c(N1,N2,N-N1-N2)

##########################################
# Power comparison
##########################################
# 1:1 allocation
y = rdply(nsim,
          do.call(rbind.data.frame,
                  models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                               mu0=mean_control,mu1=6,mu2=6,
                                               N=N,alloc="one",sl=0)$data
                  )
          )
)  

# t1e / power
y1_t1e <- y %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
e1_t1e <- sum(y1_t1e)/nsim

y2_t1e <- y %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
e2_t1e <- sum(y2_t1e)/nsim

var_e1 <- mean((y %>% filter(arm == "a1"))$var )
var_e2 <- mean((y %>% filter(arm == "a2"))$var )

df_res[i,]=c(rt_a1=e1_t1e,rt_a2=e2_t1e,r1=N1/N,r2=N2/N,
             mu0=mean_control,mu1=6,mu2=6,
             N=N,alloc="one",trend=0,H0=F,
             var_e1=var_e1, var_e2=var_e2)

i=i+1

##########################################
# optimal allocation
y_opt = rdply(nsim,
              do.call(rbind.data.frame,
                      models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                                   mu0=mean_control,mu1=6,mu2=6,
                                                   N=N,alloc="opt",sl=0)$data
                      )
              )
) 

# t1e / power
y1_t1e <- y_opt %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
e1_t1e <- sum(y1_t1e)/nsim

y2_t1e <- y_opt %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
e2_t1e <- sum(y2_t1e)/nsim

var_e1 <- mean((y_opt %>% filter(arm == "a1"))$var )
var_e2 <- mean((y_opt %>% filter(arm == "a2"))$var )

df_res[i,]=c(rt_a1=e1_t1e,rt_a2=e2_t1e,r1=N1/N,r2=N2/N,
             mu0=mean_control,mu1=6,mu2=6,
             N=N,alloc="opt",trend=0,H0=F,
             var_e1=var_e1, var_e2=var_e2)

i=i+1


##########################################
# sqrt k allocation

y_sqrt = rdply(nsim,
               do.call(rbind.data.frame,
                       models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                                    mu0=mean_control,mu1=6,mu2=6,
                                                    N=N,alloc="sqrt",sl=0)$data
                       )
               )
) 

# t1e / power
y1_t1e <- y_sqrt %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
e1_t1e <- sum(y1_t1e)/nsim

y2_t1e <- y_sqrt %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
e2_t1e <- sum(y2_t1e)/nsim

var_e1 <- mean((y_sqrt %>% filter(arm == "a1"))$var )
var_e2 <- mean((y_sqrt %>% filter(arm == "a2"))$var )

df_res[i,]=c(rt_a1=e1_t1e,rt_a2=e2_t1e,r1=N1/N,r2=N2/N,
             mu0=mean_control,mu1=6,mu2=6,
             N=N,alloc="sqrt",trend=0,H0=F, 
             var_e1=var_e1, var_e2=var_e2)

i=i+1


list_restwop_H1 = list(y,y_opt,y_sqrt)

##########################################
##########################################
# T1E comparison
##########################################
# 1:1 allocation
y = rdply(nsim,
          do.call(rbind.data.frame,
                  models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                               mu0=mean_control,mu1=mean_control,mu2=mean_control,
                                               N=N,alloc="one",sl=0)$data
                  )
          )
) 

head(y)

# t1e / power
y1_t1e <- y %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
e1_t1e <- sum(y1_t1e)/nsim

y2_t1e <- y %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
e2_t1e <- sum(y2_t1e)/nsim

var_e1 <- mean((y %>% filter(arm == "a1"))$var )
var_e2 <- mean((y %>% filter(arm == "a2"))$var )

df_res[i,]=c(rt_a1=e1_t1e,rt_a2=e2_t1e,r1=N1/N,r2=N2/N,
             mu0=mean_control,mu1=mean_control,mu2=mean_control,
             N=N,alloc="one",trend=0,H0=T,
             var_e1=var_e1, var_e2=var_e2)

i=i+1 

##########################################
# optimal allocation
y_opt = rdply(nsim,
              do.call(rbind.data.frame,
                      models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                                   mu0=mean_control,mu1=mean_control,mu2=mean_control,
                                                   N=N,alloc="opt",sl=0)$data
                      )
              )
) 

# t1e / power
y1_t1e <- y_opt %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
e1_t1e <- sum(y1_t1e)/nsim

y2_t1e <- y_opt %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
e2_t1e <- sum(y2_t1e)/nsim

var_e1 <- mean((y_opt %>% filter(arm == "a1"))$var )
var_e2 <- mean((y_opt %>% filter(arm == "a2"))$var )

df_res[i,]=c(rt_a1=e1_t1e,rt_a2=e2_t1e,r1=N1/N,r2=N2/N,
             mu0=mean_control,mu1=mean_control,mu2=mean_control,
             N=N,alloc="opt",trend=0,H0=T,
             var_e1=var_e1, var_e2=var_e2)

i=i+1 

##########################################
# sqrt k allocation

y_sqrt = rdply(nsim,
               do.call(rbind.data.frame,
                       models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                                    mu0=mean_control,mu1=mean_control,mu2=mean_control,
                                                    N=N,alloc="sqrt",sl=0)$data
                       )
               )
) 

# t1e / power
y1_t1e <- y_sqrt %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
e1_t1e <- sum(y1_t1e)/nsim

y2_t1e <- y_sqrt %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
e2_t1e <- sum(y2_t1e)/nsim

var_e1 <- mean((y_sqrt %>% filter(arm == "a1"))$var )
var_e2 <- mean((y_sqrt %>% filter(arm == "a2"))$var )


df_res[i,]=c(rt_a1=e1_t1e,rt_a2=e2_t1e,r1=N1/N,r2=N2/N,
             mu0=mean_control,mu1=mean_control,mu2=mean_control,
             N=N,alloc="sqrt",trend=0,H0=T,
             var_e1=var_e1, var_e2=var_e2)

i=i+1


##########################################

df_res$minrt = pmin(df_res$rt_a1,df_res$rt_a2)
df_res


list_restwop_H0 = list(y,y_opt,y_sqrt)

##########################################
rm(y, y_opt, y_sqrt,
   y_sym, y_sym_opt, y_sym_sqrt,
   y_nsym, y_nsym_opt, y_nsym_sqrt,
   y1_t1e, y2_t1e,
   e1_t1e, e2_t1e, i, mean_arm1, mean_arm2, mean_control, N, N1, N2, N3, 
   nsim)
##########################################

#stop cluster
stopCluster(cl)

# save.image("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/case-study/results/simstudy_results_withvar.RData") #local
# save.image("~/GitHub/Allocation/case-study/results/simstudy_completeresults_withvar.RData") #server


