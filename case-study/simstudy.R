
##########################################
# Optimal allocation
# Case study
# 2022-Oct
# Marta Bofill Roig
##########################################

rm(list = ls())

# install.packages("plyr")
library(plyr)
# devtools::install_github("pavlakrotka/NCC@v1.0")
library(NCC)

# devtools::install_github("ianmoran11/mmtable2")
library(mmtable2)
library(gt)
library(tidyverse)


##########################################
# Functions
source("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/case-study/aux_functions.R")
##########################################

# syntaxix 
# sim_designs(r1,r2,mu0,mu1,mu2,N,alloc="sqrt",sl=0.2) 

# Examples: 
# r1=20/70;r2=30/70;mu0=0;mu1=1;mu2=1;N=70;alloc="opt"
# db=sim_designs(r1=20/70,r2=30/70,mu0=0,mu1=1,mu2=1,N=70,alloc="one")
# head(db)
# plot_trial(db$data$treatment) 
# 
# db=sim_designs(r1=20/70,r2=30/70,mu0=0,mu1=1,mu2=1,N=70,alloc="opt")
# head(db$data)
# plot_trial(db$data$treatment)  
# 

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

library(dplyr)
nsim=100000 

N = 80
N1 = round(N/3)
N2 = round(2*(N-N1)/3)
c(N1,N2,N-N1-N2)

# design 3: three-period design (non-symmetric design)
db3_one=sim_designs(r1=N1/N,r2=N2/N,mu0=mean_control,mu1=mean_arm1,mu2=mean_arm2,N=N,alloc="one")
db3_sqrt=sim_designs(r1=N1/N,r2=N2/N,mu0=mean_control,mu1=mean_arm1,mu2=mean_arm2,N=N,alloc="sqrt")
db3_opt=sim_designs(r1=N1/N,r2=N2/N,mu0=mean_control,mu1=mean_arm1,mu2=mean_arm2,N=N,alloc="opt")

db3_one_ss <- data.frame(arms=c("A1","A2","C"),db3_one$ss, c(sum(db3_one$ss[1,]),sum(db3_one$ss[2,]),sum(db3_one$ss[3,])))
db3_sqrt_ss <- data.frame(arms=c("A1","A2","C"), db3_sqrt$ss, c(sum(db3_sqrt$ss[1,]),sum(db3_sqrt$ss[2,]),sum(db3_sqrt$ss[3,])))
db3_opt_ss <- data.frame(arms=c("A1","A2","C"), db3_opt$ss, c(sum(db3_opt$ss[1,]),sum(db3_opt$ss[2,]),sum(db3_opt$ss[3,])))

knitr::kable(db3_one_ss, format = "markdown", caption = c("Sample sizes per period and arm (1:1)"), col.names = c("Arms", "Period 1","Period 2","Period 3", "Total per arm"))
knitr::kable(db3_sqrt_ss, format = "markdown", caption = c("Sample sizes per period and arm (sqrt(k)-rule)"), col.names = c("Arms","Period 1","Period 2","Period 3", "Total per arm"))
knitr::kable(db3_opt_ss, format = "markdown", caption = c("Sample sizes per period and arm (optimal allocations)"), col.names = c("Arms","Period 1","Period 2","Period 3", "Total per arm"))

##########################################
# 1:1 allocation
y = rdply(nsim,
          do.call(rbind.data.frame,
                  models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                               mu0=mean_control,mu1=7,mu2=7,
                                               N=N,alloc="one")$data
                  )
          )
) 

head(y)

# t1e / power
y1_t1e <- y %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
sum(y1_t1e)/nsim

y2_t1e <- y %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
sum(y2_t1e)/nsim

##########################################
# optimal allocation
y_opt = rdply(nsim,
              do.call(rbind.data.frame,
                      models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                                   mu0=mean_control,mu1=7,mu2=7,
                                                   N=N,alloc="opt")$data
                      )
              )
) 

# t1e / power
y1_t1e <- y_opt %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
sum(y1_t1e)/nsim

y2_t1e <- y_opt %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
sum(y2_t1e)/nsim


##########################################
# sqrt k allocation

y_sqrt = rdply(nsim,
              do.call(rbind.data.frame,
                      models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                                   mu0=mean_control,mu1=7,mu2=7,
                                                   N=N,alloc="sqrt")$data
                      )
              )
) 

# t1e / power
y1_t1e <- y_sqrt %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
sum(y1_t1e)/nsim

y2_t1e <- y_sqrt %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
sum(y2_t1e)/nsim

##########################################
# save.image("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/case-study/simstudy_results.R.RData")

