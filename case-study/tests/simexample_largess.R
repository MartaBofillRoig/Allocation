
############################################
# Optimal allocation
# Numerical example with large sample sizes
# 2022-Oct
# Marta Bofill Roig
############################################

rm(list = ls())

library(plyr)
# devtools::install_github("pavlakrotka/NCC@v1.0")
library(NCC)
# devtools::install_github("ianmoran11/mmtable2")
library(mmtable2)
library(gt)
library(tidyverse)

set.seed(893)

##########################################
# Functions
source("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/case-study/aux_functions.R")
##########################################


res <- data.frame(N=c(0),r1=c(0),r2=c(0),
                  design=c(0),trend=c(0),
                  pow_a1=c(0),pow_a2=c(0),
                  width_ci1=c(0),width_ci2=c(0),
                  var_eff1=c(0),var_eff2=c(0))
i=1
knitr::kable(res, format = "markdown", caption = c("Comparison results a1 and a2 using optimal allocations"))

##########################################
N = 1000
N1 = round(N/3) 
N3 = round(N/3)
N2 = N-N1-N3
c(N1,N2,N-N1-N2)


db3_opt=sim_designs(r1=N1/N,r2=N2/N,mu0=0,mu1=0,mu2=0,N=N,alloc="opt")
db3_opt_ss <- data.frame(arms=c("A1","A2","C"), db3_opt$ss, c(sum(db3_opt$ss[1,]),sum(db3_opt$ss[2,]),sum(db3_opt$ss[3,])))
knitr::kable(db3_opt_ss, format = "markdown", caption = c("Sample sizes per period and arm (optimal allocations)"), col.names = c("Arms","Period 1","Period 2","Period 3", "Total per arm"))

res3_opt = do.call(rbind.data.frame, models_cc(data = db3_opt$data) )
res3_opt$width_ci = res3_opt$upper_ci  - res3_opt$lower_ci 
knitr::kable(res3_opt, format = "markdown")

# simulation - power
nsim=10000

y_opt = rdply(nsim,
              do.call(rbind.data.frame,
                      models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                                   mu0=0,mu1=0.4,mu2=0.4,
                                                   N=N,alloc="opt")$data
                      )
              )
) 

# t1e / power
y1_t1e <- y_opt %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
e1_t1e <- sum(y1_t1e)/nsim

width_ci_a1 <- y_opt  %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a1$width <- width_ci_a1$upper_ci - width_ci_a1$lower_ci


y2_t1e <- y_opt %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
e2_t1e <- sum(y2_t1e)/nsim

width_ci_a2 <- y_opt  %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a2$width <- width_ci_a2$upper_ci - width_ci_a2$lower_ci

# 
c(e1_t1e,e2_t1e)
c(mean(width_ci_a1$width),mean(width_ci_a2$width))
c(var(width_ci_a1$treat_effect),var(width_ci_a2$treat_effect))

# 
res[i,] <- c(N,N1/N,N2/N,
            "sym","sw",
            round(e1_t1e,3), round(e2_t1e,3),
            round(mean(width_ci_a1$width),3),round(mean(width_ci_a2$width),3),
            round(var(width_ci_a1$treat_effect),3),round(var(width_ci_a2$treat_effect),3))

##########################################
# Non-symmetrical case (ex 1)

N = 1000
N1 = round(N/3)
N2 = round(2*(N-N1)/3)
# N2= 500
c(N1,N2,N-N1-N2)


db3_opt_ns=sim_designs(r1=N1/N,r2=N2/N,mu0=0,mu1=0.4,mu2=0.4,N=N,alloc="opt")
db3_opt_ns_ss <- data.frame(arms=c("A1","A2","C"), db3_opt_ns$ss, c(sum(db3_opt_ns$ss[1,]),sum(db3_opt_ns$ss[2,]),sum(db3_opt_ns$ss[3,])))
knitr::kable(db3_opt_ns_ss, format = "markdown", caption = c("Sample sizes per period and arm (optimal allocations)"), col.names = c("Arms","Period 1","Period 2","Period 3", "Total per arm"))

res3_opt_ns = do.call(rbind.data.frame, models_cc(data = db3_opt_ns$data) )
res3_opt_ns$width_ci = res3_opt_ns$upper_ci  - res3_opt_ns$lower_ci 
knitr::kable(res3_opt_ns, format = "markdown")  

# simulation
nsim=10000

y_opt_ns = rdply(nsim,
              do.call(rbind.data.frame,
                      models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                                   mu0=0,mu1=0.4,mu2=0.4,
                                                   N=N,alloc="opt")$data
                      )
              )
) 

# t1e / power
y1_t1e_ns <- y_opt_ns %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
e1_t1e_ns <- sum(y1_t1e_ns)/nsim

width_ci_a1_ns <- y_opt_ns  %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a1_ns$width <- width_ci_a1_ns$upper_ci - width_ci_a1_ns$lower_ci


y2_t1e_ns <- y_opt_ns %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
e2_t1e_ns <- sum(y2_t1e_ns)/nsim

width_ci_a2_ns <- y_opt_ns  %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a2_ns$width <- width_ci_a2_ns$upper_ci - width_ci_a2_ns$lower_ci

# 
c(e1_t1e_ns,e2_t1e_ns)
c(mean(width_ci_a1_ns$width),mean(width_ci_a2_ns$width))
c(var(width_ci_a1_ns$treat_effect),var(width_ci_a2_ns$treat_effect))

# 
i=i+1
res[i,] <- c(N,N1/N,N2/N, 
             "nsym","sw",
             round(e1_t1e_ns,3), round(e2_t1e_ns,3),
             round(mean(width_ci_a1_ns$width),3),round(mean(width_ci_a2_ns$width),3),
             round(var(width_ci_a1_ns$treat_effect),3),round(var(width_ci_a2_ns$treat_effect),3))

##########################################
# Non-symmetrical case (ex 2)

N = 1000
N1 = round(N/3)
# N2 = round(2*(N-N1)/3)
N2= 500
c(N1,N2,N-N1-N2)


db3_opt_ns2=sim_designs(r1=N1/N,r2=N2/N,mu0=0,mu1=0.4,mu2=0.4,N=N,alloc="opt")
db3_opt_ns2_ss <- data.frame(arms=c("A1","A2","C"), db3_opt_ns2$ss, c(sum(db3_opt_ns2$ss[1,]),sum(db3_opt_ns2$ss[2,]),sum(db3_opt_ns2$ss[3,])))
knitr::kable(db3_opt_ns2_ss, format = "markdown", caption = c("Sample sizes per period and arm (optimal allocations)"), col.names = c("Arms","Period 1","Period 2","Period 3", "Total per arm"))

res3_opt_ns2 = do.call(rbind.data.frame, models_cc(data = db3_opt_ns2$data) )
res3_opt_ns2$width_ci = res3_opt_ns2$upper_ci  - res3_opt_ns2$lower_ci 
knitr::kable(res3_opt_ns2, format = "markdown")  

# simulation
nsim=10000

y_opt_ns2 = rdply(nsim,
                 do.call(rbind.data.frame,
                         models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                                      mu0=0,mu1=0.4,mu2=0.4,
                                                      N=N,alloc="opt")$data
                         )
                 )
) 

# t1e / power
y1_t1e_ns2 <- y_opt_ns2 %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
e1_t1e_ns2 <- sum(y1_t1e_ns2)/nsim

width_ci_a1_ns2 <- y_opt_ns2  %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a1_ns2$width <- width_ci_a1_ns2$upper_ci - width_ci_a1_ns2$lower_ci

# mean(width_ci_a1_ns2$width)
# var(width_ci_a1_ns2$treat_effect)

y2_t1e_ns2 <- y_opt_ns2 %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
e2_t1e_ns2 <- sum(y2_t1e_ns2)/nsim

width_ci_a2_ns2 <- y_opt_ns2  %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a2_ns2$width <- width_ci_a2_ns2$upper_ci - width_ci_a2_ns2$lower_ci

#  comparison
c(e1_t1e_ns2,e2_t1e_ns2)
c(mean(width_ci_a1_ns2$width),mean(width_ci_a2_ns2$width))
c(var(width_ci_a1_ns2$treat_effect),var(width_ci_a2_ns2$treat_effect))
c(sd(width_ci_a1_ns2$width),sd(width_ci_a2_ns2$width))

# 
i=i+1
res[i,] <- c(N,N1/N,N2/N,
             "nsym","sw", 
             round(e1_t1e_ns2,3), round(e2_t1e_ns2,3),
             round(mean(width_ci_a1_ns2$width),3),round(mean(width_ci_a2_ns2$width),3),
             round(var(width_ci_a1_ns2$treat_effect),3),round(var(width_ci_a2_ns2$treat_effect),3))


##########################################
knitr::kable(res, format = "markdown", caption = c("Comparison results a1 and a2 using optimal allocations"))
# save.image("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/case-study/results/simstudy_results_largess.RData") #local

##########################################
# simulation without time trends
# set.seed(356)
##########################################
# Non-symmetrical case and no time trend

N = 1000
N1 = round(N/3)
N2 = round(2*(N-N1)/3)
# N2= 500
c(N1,N2,N-N1-N2)


db3_opt_nsnt = sim_designs(r1=N1/N,r2=N2/N,mu0=0,mu1=0.4,mu2=0.4,N=N,alloc="opt",sl=0)
db3_opt_nsnt_ss <- data.frame(arms=c("A1","A2","C"), db3_opt_nsnt$ss, c(sum(db3_opt_nsnt$ss[1,]),sum(db3_opt_nsnt$ss[2,]),sum(db3_opt_nsnt$ss[3,])))
knitr::kable(db3_opt_nsnt_ss, format = "markdown", caption = c("Sample sizes per period and arm (optimal allocations)"), col.names = c("Arms","Period 1","Period 2","Period 3", "Total per arm"))

res3_opt_nsnt = do.call(rbind.data.frame, models_cc(data = db3_opt_nsnt$data) )
res3_opt_nsnt$width_ci = res3_opt_nsnt$upper_ci  - res3_opt_nsnt$lower_ci 
knitr::kable(res3_opt_nsnt, format = "markdown")  

# simulation
nsim=10000

y_opt_nsnt = rdply(nsim,
                 do.call(rbind.data.frame,
                         models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                                      mu0=0,mu1=0.4,mu2=0.4,
                                                      N=N,alloc="opt",sl=0)$data
                         )
                 )
) 

# t1e / power
y1_t1e_nsnt <- y_opt_nsnt %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
e1_t1e_nsnt <- sum(y1_t1e_nsnt)/nsim

width_ci_a1_nsnt <- y_opt_nsnt  %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a1_nsnt$width <- width_ci_a1_nsnt$upper_ci - width_ci_a1_nsnt$lower_ci


y2_t1e_nsnt <- y_opt_nsnt %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
e2_t1e_nsnt <- sum(y2_t1e_nsnt)/nsim

width_ci_a2_nsnt <- y_opt_nsnt  %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a2_nsnt$width <- width_ci_a2_nsnt$upper_ci - width_ci_a2_nsnt$lower_ci

# 
c(e1_t1e_nsnt,e2_t1e_nsnt)
c(mean(width_ci_a1_nsnt$width),mean(width_ci_a2_nsnt$width))
c(var(width_ci_a1_nsnt$treat_effect),var(width_ci_a2_nsnt$treat_effect))

# 
i=i+1
res[i,] <- c(N,N1/N,N2/N, 
             "nsym","no-trend",
             round(e1_t1e_nsnt,3), round(e2_t1e_nsnt,3),
             round(mean(width_ci_a1_nsnt$width),3),round(mean(width_ci_a2_nsnt$width),3),
             round(var(width_ci_a1_nsnt$treat_effect),3),round(var(width_ci_a2_nsnt$treat_effect),3))

##########################################
# simulation with linear time trends 
##########################################
# Non-symmetrical case 

N = 1000
N1 = round(N/3)
N2 = round(2*(N-N1)/3)
# N2= 500
c(N1,N2,N-N1-N2)


db3_opt_nsl = sim_designs(r1=N1/N,r2=N2/N,mu0=0,mu1=0.4,mu2=0.4,N=N,alloc="opt",sl=0)
db3_opt_nsl_ss <- data.frame(arms=c("A1","A2","C"), db3_opt_nsl$ss, c(sum(db3_opt_nsl$ss[1,]),sum(db3_opt_nsl$ss[2,]),sum(db3_opt_nsl$ss[3,])))
knitr::kable(db3_opt_nsl_ss, format = "markdown", caption = c("Sample sizes per period and arm (optimal allocations)"), col.names = c("Arms","Period 1","Period 2","Period 3", "Total per arm"))

res3_opt_nsl = do.call(rbind.data.frame, models_cc(data = db3_opt_nsl$data) )
res3_opt_nsl$width_ci = res3_opt_nsl$upper_ci  - res3_opt_nsl$lower_ci 
knitr::kable(res3_opt_nsl, format = "markdown")  

# simulation
nsim=10000

y_opt_nsl = rdply(nsim,
                   do.call(rbind.data.frame,
                           models_cc(data = sim_designs(r1=N1/N,r2=N2/N,
                                                        mu0=0,mu1=0.4,mu2=0.4,
                                                        N=N,alloc="opt",trend="linear",sl=0.5)$data
                           )
                   )
) 

# t1e / power
y1_t1e_nsl <- y_opt_nsl %>% 
  filter(arm == "a1") %>%
  select(reject_h0)  
e1_t1e_nsl <- sum(y1_t1e_nsl)/nsim

width_ci_a1_nsl <- y_opt_nsl  %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a1_nsl$width <- width_ci_a1_nsnt$upper_ci - width_ci_a1_nsnt$lower_ci


y2_t1e_nsl <- y_opt_nsl %>% 
  filter(arm == "a2") %>%
  select(reject_h0) 
e2_t1e_nsl <- sum(y2_t1e_nsl)/nsim

width_ci_a2_nsl <- y_opt_nsl  %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a2_nsl$width <- width_ci_a2_nsl$upper_ci - width_ci_a2_nsl$lower_ci

# 
c(e1_t1e_nsl,e2_t1e_nsl)
c(mean(width_ci_a1_nsl$width),mean(width_ci_a2_nsl$width))
c(var(width_ci_a1_nsl$treat_effect),var(width_ci_a2_nsl$treat_effect))

# 
i=i+1
res[i,] <- c(N,N1/N,N2/N, 
             "nsym","linear",
             round(e1_t1e_nsl,3), round(e2_t1e_nsl,3),
             round(mean(width_ci_a1_nsl$width),3),round(mean(width_ci_a2_nsl$width),3),
             round(var(width_ci_a1_nsl$treat_effect),3),round(var(width_ci_a2_nsl$treat_effect),3))


##########################################
##########################################
knitr::kable(res, format = "markdown", caption = c("Comparison results a1 and a2 using optimal allocations"))
# save.image("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/case-study/results/simstudy_results_largess.RData") #local


