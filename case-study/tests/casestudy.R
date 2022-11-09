
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
db=sim_designs(r1=20/70,r2=30/70,mu0=0,mu1=1,mu2=1,N=70,alloc="one")
head(db)
plot_trial(db$data$treatment) 

db=sim_designs(r1=20/70,r2=30/70,mu0=0,mu1=1,mu2=1,N=70,alloc="opt")
head(db$data)
plot_trial(db$data$treatment) 

##########################################
# Modeling
##########################################

# # NCC pkg functions 
# fixmodel_cont(data=db,arm=2,alpha=0.025) #model using ncc
# sepmodel_adj_cont(data=db,arm=1,alpha=0.025) #model using cc only

res_all = do.call(rbind.data.frame, models(data = db$data) )
head(res_all)
res_all$width_ci = res_all$upper_ci  - res_all$lower_ci  

res_cc = do.call(rbind.data.frame, models_cc(data = db$data) )
head(res)

res_ncc = do.call(rbind.data.frame, models_ncc(data = db$data) )
head(res_ncc)


##########################################
# Case study
##########################################

# original study
n_control = 31
n_arm1 = 31
n_arm2 = 30
N = n_control + n_arm1 + n_arm2

# means
mean_control = 17.3/3.5
mean_arm1 = 66.2/3.5
mean_arm2 = 72.3/3.5

# design 1: multi-arm design
db1_one = sim_designs(r1=1,r2=0,mu0=mean_control,mu1=mean_arm1,mu2=mean_arm2,N=N,alloc="one")
db1_sqrt = sim_designs(r1=1,r2=0,mu0=mean_control,mu1=mean_arm1,mu2=mean_arm2,N=N,alloc="sqrt")
db1_opt = sim_designs(r1=1,r2=0,mu0=mean_control,mu1=mean_arm1,mu2=mean_arm2,N=N,alloc="opt")

# head(db)
mod <- lm(response ~ as.factor(treatment), db1_sqrt$data)
summary(mod)

# sample sizes
db1_one$ss
db1_sqrt$ss
db1_opt$ss

plot_trial(db1_opt$data$treatment) 

(res1_one = do.call(rbind.data.frame, models_cc(data = db1_one$data) ))
(res1_sqrt = do.call(rbind.data.frame, models_cc(data = db1_sqrt$data) ))
(res1_opt = do.call(rbind.data.frame, models_cc(data = db1_opt$data) ))

# design 2: two-period design
db2_one=sim_designs(r1=n_arm1/N,r2=1-n_arm1/N,mu0=mean_control,mu1=mean_arm1,mu2=mean_arm2,N=N,alloc="one")
db2_sqrt=sim_designs(r1=n_arm1/N,r2=1-n_arm1/N,mu0=mean_control,mu1=mean_arm1,mu2=mean_arm2,N=N,alloc="opt")
db2_opt=sim_designs(r1=n_arm1/N,r2=1-n_arm1/N,mu0=mean_control,mu1=mean_arm1,mu2=mean_arm2,N=N,alloc="sqrt")

plot_trial(db2_opt$data$treatment) 

# sample sizes
db2_one$ss
db2_sqrt$ss
db2_opt$ss


db_ss <- data.frame(arms=c("A1","A2","C"),db2_one$ss, c(sum(db2_one$ss[1,]),sum(db2_one$ss[2,]),sum(db2_one$ss[3,])))
knitr::kable(db_ss, format = "markdown", col.names = c("Sample","Sample","Sample","Sample","T"))


(res2_one = do.call(rbind.data.frame, models_cc(data = db2_one$data) ))
(res2_sqrt = do.call(rbind.data.frame, models_cc(data = db2_sqrt$data) ))
(res2_opt = do.call(rbind.data.frame, models_cc(data = db2_opt$data) ))

(res2 = do.call(rbind.data.frame, models(data = db2_opt$data) ))

knitr::kable(res2_opt, format = "markdown")

# design 3: three-period design (symmetric design)
db3_one=sim_designs(r1=n_arm1/N,r2=1-2*n_arm1/N,mu0=mean_control,mu1=mean_arm1,mu2=mean_arm2,N=N,alloc="one")
db3_sqrt=sim_designs(r1=n_arm1/N,r2=1-2*n_arm1/N,mu0=mean_control,mu1=mean_arm1,mu2=mean_arm2,N=N,alloc="sqrt")
db3_opt=sim_designs(r1=n_arm1/N,r2=1-2*n_arm1/N,mu0=mean_control,mu1=mean_arm1,mu2=mean_arm2,N=N,alloc="opt")

plot_trial(db3_opt$data$treatment) 

# sample sizes
db3_one$ss
db3_sqrt$ss
db3_opt$ss

(res3_one = do.call(rbind.data.frame, models_cc(data = db3_one$data) ))
(res3_sqrt = do.call(rbind.data.frame, models_cc(data = db3_sqrt$data) ))
(res3_opt = do.call(rbind.data.frame, models_cc(data = db3_opt$data) ))


# design 3: three-period design (non-symmetric design)
db3b_one=sim_designs(r1=n_arm1/N,r2=n_arm1/N/2,mu0=mean_control,mu1=mean_arm1,mu2=mean_arm2,N=N,alloc="one")
db3b_sqrt=sim_designs(r1=n_arm1/N,r2=n_arm1/N/2,mu0=mean_control,mu1=mean_arm1,mu2=mean_arm2,N=N,alloc="sqrt")
db3b_opt=sim_designs(r1=n_arm1/N,r2=n_arm1/N/2,mu0=mean_control,mu1=mean_arm1,mu2=mean_arm2,N=N,alloc="opt")

plot_trial(db3b_opt$data$treatment) 

# r1=n_arm1/N;r2=n_arm1/N/2;alloc="sqrt"
# sample sizes
db3b_one$ss
db3b_sqrt$ss
db3b_opt$ss

(res3b_one = do.call(rbind.data.frame, models_cc(data = db3b_one$data) ))
(res3b_sqrt = do.call(rbind.data.frame, models_cc(data = db3b_sqrt$data) ))
(res3b_opt = do.call(rbind.data.frame, models_cc(data = db3b_opt$data) ))


##########################################
########################################## 

