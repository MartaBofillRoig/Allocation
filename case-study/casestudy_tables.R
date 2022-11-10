
##########################################
# Optimal allocation
# Simulation study
# 2022-Nov
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
# Case study
##########################################

# original study - obtained means
mean_control = 17.3/3.5
mean_arm1 = 66.2/3.5
mean_arm2 = 72.3/3.5 

##########################################
# design 3: three-period design (symmetric design)
##########################################
N = 92
N1 = round(N/3)
N3 = round(N/3)
N2 = N-N1-N3
c(N1,N2,N-N1-N2)

alloc_str <- c("one","opt","sqrt")
for(i in 1:3){
  m <- sim_designs(r1=N1/N,r2=N2/N,
                   mu0=mean_control,mu1=6,mu2=6,
                   N=N,alloc=alloc_str[i],sl=0)$ss
  
  rownames(m) <- c("Arm 2", "Arm 1", "Control")
  print(knitr::kable(m, 
                     format = "latex", caption = paste(alloc_str[i]), 
                     col.names = c("Period 1", "Period 2", "Period 3"), 
                     row.names=T, 
                     digits=3))
}

          
##########################################
# design 3: three-period design (non-symmetric design)
##########################################
N = 92
N1 = round(N/3)
N2 = round(2*(N-N1)/3)
c(N1,N2,N-N1-N2)

alloc_str <- c("one","opt","sqrt")
for(i in 1:3){
  m <- sim_designs(r1=N1/N,r2=N2/N,
                   mu0=mean_control,mu1=6,mu2=6,
                   N=N,alloc=alloc_str[i],sl=0)$ss
  
  rownames(m) <- c("Arm 2", "Arm 1", "Control")
  print(knitr::kable(m, 
                     format = "latex", caption = paste(alloc_str[i]), 
                     col.names = c("Period 1", "Period 2", "Period 3"), 
                     row.names=T, 
                     digits=3))
}



##########################################
# design 2: two-period design  
########################################## 

N = 92
N1 = round(N/4)
N2 = round(N-N1)
c(N1,N2,N-N1-N2)          


# sim_designs(r1=N1/N,r2=N2/N,
#             mu0=mean_control,mu1=6,mu2=6,
#             N=N,alloc="one",sl=0)$ss
# 
# 
# sim_designs(r1=N1/N,r2=N2/N,
#             mu0=mean_control,mu1=6,mu2=6,
#             N=N,alloc="opt",sl=0)$ss
# 
# 
# sim_designs(r1=N1/N,r2=N2/N,
#             mu0=mean_control,mu1=6,mu2=6,
#             N=N,alloc="sqrt",sl=0)$ss
# m <- sim_designs(r1=N1/N,r2=N2/N,
#                  mu0=mean_control,mu1=6,mu2=6,
#                  N=N,alloc="sqrt",sl=0)$ss
# rownames(m) <- c("Arm 2", "Arm 1", "Control")
# knitr::kable(m, 
#              format = "markdown", caption = c("Sample sizes"), 
#              col.names = c("Period 1", "Period 2", "Period 3"), 
#              row.names=T, 
#              digits=3)

alloc_str <- c("one","opt","sqrt")
for(i in 1:3){
  m <- sim_designs(r1=N1/N,r2=N2/N,
                   mu0=mean_control,mu1=6,mu2=6,
                   N=N,alloc=alloc_str[i],sl=0)$ss
  
  rownames(m) <- c("Arm 2", "Arm 1", "Control")
  print(knitr::kable(m, 
               format = "latex", caption = paste(alloc_str[i]), 
               col.names = c("Period 1", "Period 2", "Period 3"), 
               row.names=T, 
               digits=3))
}



          