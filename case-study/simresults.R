##########################################
# Optimal allocation
# Simulation study - results
# 2022-Oct
# Marta Bofill Roig
##########################################

rm(list = ls())

library(tidyverse)

load("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/case-study/results/simstudy_completeresults_clean.RData")
# load("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/case-study/results/simstudy_results.RData")
df_res$design = ifelse(as.numeric(df_res$r1)+as.numeric(df_res$r2)==1,"2-period", "3-period")
# df_res$r1+df_res$r2

df_res$minrt = pmin(df_res$rt_a1,df_res$rt_a2)

res_report_H1 <- df_res %>% filter(H0=="FALSE") %>% select(rt_a1,rt_a2,r1,r2,alloc,minrt,design)
knitr::kable(res_report_H1, format = "markdown", caption = c("Power comparisons"))

res_report_H0 <- df_res %>% filter(H0=="TRUE") %>% select(rt_a1,rt_a2,r1,r2,alloc,minrt,design)
knitr::kable(res_report_H0, format = "markdown", caption = c("Type 1 error rate"))

res_report_H1$width_ci_a1 <- c(rep(NA,dim(res_report_H1)[1]))
res_report_H1$width_ci_a2 <- c(rep(NA,dim(res_report_H1)[1]))
res_report_H0$width_ci_a1 <- c(rep(NA,dim(res_report_H0)[1]))
res_report_H0$width_ci_a2 <- c(rep(NA,dim(res_report_H0)[1]))

res_report_H1$var_eff_a1 <- c(rep(NA,dim(res_report_H1)[1]))
res_report_H1$var_eff_a2 <- c(rep(NA,dim(res_report_H1)[1]))
res_report_H0$var_eff_a1 <- c(rep(NA,dim(res_report_H0)[1]))
res_report_H0$var_eff_a2 <- c(rep(NA,dim(res_report_H0)[1]))

##########################################
# 3-periods symmetric
##########################################
# list_res_H1 = list(y,y_opt,y_sqrt)

# one to one allocation
# H1
width_ci_a1 <- list_res_sym_H1[[1]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a1$width <- width_ci_a1$upper_ci - width_ci_a1$lower_ci
res_report_H1$width_ci_a1[1] <- mean(width_ci_a1$width)
res_report_H1$var_eff_a1[1] <- var(width_ci_a1$treat_effect)

width_ci_a2 <- list_res_sym_H1[[1]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a2$width <- width_ci_a2$upper_ci - width_ci_a2$lower_ci
res_report_H1$width_ci_a2[1] <-mean(width_ci_a2$width) 
res_report_H1$var_eff_a2[1] <- var(width_ci_a2$treat_effect)

# boxplot(width_ci_a1$width-width_ci_a2$width)


# H0
width_ci_a1 <- list_res_sym_H0[[1]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a1$width <- width_ci_a1$upper_ci - width_ci_a1$lower_ci
res_report_H0$width_ci_a1[1] <-mean(width_ci_a1$width)
res_report_H0$var_eff_a1[1] <- var(width_ci_a1$treat_effect)

width_ci_a2 <- list_res_sym_H0[[1]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a2$width <- width_ci_a2$upper_ci - width_ci_a2$lower_ci
res_report_H0$width_ci_a2[1] <-mean(width_ci_a2$width) 
res_report_H0$var_eff_a2[1] <- var(width_ci_a2$treat_effect)

# opt allocation
# H1
width_ciopt_a1 <- list_res_sym_H1[[2]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a1$width <- width_ciopt_a1$upper_ci - width_ciopt_a1$lower_ci
res_report_H1$width_ci_a1[2] <- mean(width_ciopt_a1$width)
res_report_H1$var_eff_a1[2] <- var(width_ci_a1$treat_effect)

width_ciopt_a2 <- list_res_sym_H1[[2]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a2$width <- width_ciopt_a2$upper_ci - width_ciopt_a2$lower_ci
res_report_H1$width_ci_a2[2] <- mean(width_ciopt_a2$width)
res_report_H1$var_eff_a2[2] <- var(width_ciopt_a2$treat_effect)

# boxplot(width_ciopt_a1$width-width_ciopt_a2$width)

# H0
width_ciopt_a1 <- list_res_sym_H0[[2]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a1$width <- width_ciopt_a1$upper_ci - width_ciopt_a1$lower_ci
res_report_H0$width_ci_a1[2] <- mean(width_ciopt_a1$width)
res_report_H0$var_eff_a1[2] <- var(width_ciopt_a1$treat_effect)

width_ciopt_a2 <- list_res_sym_H0[[2]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a2$width <- width_ciopt_a2$upper_ci - width_ciopt_a2$lower_ci
res_report_H0$width_ci_a2[2] <- mean(width_ciopt_a2$width)
res_report_H0$var_eff_a2[2] <- var(width_ciopt_a2$treat_effect)

# sqrt allocation
# H1
width_cisqrt_a1 <- list_res_sym_H1[[3]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a1$width <- width_cisqrt_a1$upper_ci - width_cisqrt_a1$lower_ci
res_report_H1$width_ci_a1[3] <- mean(width_cisqrt_a1$width)
res_report_H1$var_eff_a1[3] <- var(width_cisqrt_a1$treat_effect)

width_cisqrt_a2 <- list_res_sym_H1[[3]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a2$width <- width_cisqrt_a2$upper_ci - width_cisqrt_a2$lower_ci
res_report_H1$width_ci_a2[3] <- mean(width_cisqrt_a2$width)
res_report_H1$var_eff_a2[3] <- var(width_cisqrt_a2$treat_effect)

# H0
width_cisqrt_a1 <- list_res_sym_H0[[3]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a1$width <- width_cisqrt_a1$upper_ci - width_cisqrt_a1$lower_ci
res_report_H0$width_ci_a1[3] <- mean(width_cisqrt_a1$width)
res_report_H0$var_eff_a1[3] <- var(width_cisqrt_a1$treat_effect) 

width_cisqrt_a2 <- list_res_sym_H0[[3]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a2$width <- width_cisqrt_a2$upper_ci - width_cisqrt_a2$lower_ci
res_report_H0$width_ci_a2[3] <- mean(width_cisqrt_a2$width)
res_report_H0$var_eff_a2[3] <- var(width_cisqrt_a2$treat_effect)


##########################################
# 3-periods nonsymmetric
##########################################
# list_res_H1 = list(y,y_opt,y_sqrt)

# one to one allocation
# H1
width_ci_a1 <- list_res_nsym_H1[[1]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a1$width <- width_ci_a1$upper_ci - width_ci_a1$lower_ci
res_report_H1$width_ci_a1[4] <- mean(width_ci_a1$width)
res_report_H1$var_eff_a1[4] <- var(width_ci_a1$treat_effect)

width_ci_a2 <- list_res_nsym_H1[[1]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a2$width <- width_ci_a2$upper_ci - width_ci_a2$lower_ci
res_report_H1$width_ci_a2[4] <-mean(width_ci_a2$width) 
res_report_H1$var_eff_a2[4] <- var(width_ci_a2$treat_effect)

# boxplot(width_ci_a1$width-width_ci_a2$width)


# H0
width_ci_a1 <- list_res_nsym_H0[[1]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a1$width <- width_ci_a1$upper_ci - width_ci_a1$lower_ci
res_report_H0$width_ci_a1[4] <-mean(width_ci_a1$width)
res_report_H0$var_eff_a1[4] <- var(width_ci_a1$treat_effect)

width_ci_a2 <- list_res_nsym_H0[[1]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a2$width <- width_ci_a2$upper_ci - width_ci_a2$lower_ci
res_report_H0$width_ci_a2[4] <-mean(width_ci_a2$width) 
res_report_H0$var_eff_a2[4] <- var(width_ci_a2$treat_effect)

# opt allocation
# H1
width_ciopt_a1 <- list_res_nsym_H1[[2]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a1$width <- width_ciopt_a1$upper_ci - width_ciopt_a1$lower_ci
res_report_H1$width_ci_a1[5] <- mean(width_ciopt_a1$width)
res_report_H1$var_eff_a1[5] <- var(width_ci_a1$treat_effect)

width_ciopt_a2 <- list_res_nsym_H1[[2]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a2$width <- width_ciopt_a2$upper_ci - width_ciopt_a2$lower_ci
res_report_H1$width_ci_a2[5] <- mean(width_ciopt_a2$width)
res_report_H1$var_eff_a2[5] <- var(width_ciopt_a2$treat_effect)

# boxplot(width_ciopt_a1$width-width_ciopt_a2$width)

# H0
width_ciopt_a1 <- list_res_nsym_H0[[2]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a1$width <- width_ciopt_a1$upper_ci - width_ciopt_a1$lower_ci
res_report_H0$width_ci_a1[5] <- mean(width_ciopt_a1$width)
res_report_H0$var_eff_a1[5] <- var(width_ciopt_a1$treat_effect)

width_ciopt_a2 <- list_res_nsym_H0[[2]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a2$width <- width_ciopt_a2$upper_ci - width_ciopt_a2$lower_ci
res_report_H0$width_ci_a2[5] <- mean(width_ciopt_a2$width)
res_report_H0$var_eff_a2[5] <- var(width_ciopt_a2$treat_effect)

# sqrt allocation
# H1
width_cisqrt_a1 <- list_res_nsym_H1[[3]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a1$width <- width_cisqrt_a1$upper_ci - width_cisqrt_a1$lower_ci
res_report_H1$width_ci_a1[6] <- mean(width_cisqrt_a1$width)
res_report_H1$var_eff_a1[6] <- var(width_cisqrt_a1$treat_effect)

width_cisqrt_a2 <- list_res_nsym_H1[[3]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a2$width <- width_cisqrt_a2$upper_ci - width_cisqrt_a2$lower_ci
res_report_H1$width_ci_a2[6] <- mean(width_cisqrt_a2$width)
res_report_H1$var_eff_a2[6] <- var(width_cisqrt_a2$treat_effect)

# H0
width_cisqrt_a1 <- list_res_nsym_H0[[3]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a1$width <- width_cisqrt_a1$upper_ci - width_cisqrt_a1$lower_ci
res_report_H0$width_ci_a1[6] <- mean(width_cisqrt_a1$width)
res_report_H0$var_eff_a1[6] <- var(width_cisqrt_a1$treat_effect) 

width_cisqrt_a2 <- list_res_nsym_H0[[3]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a2$width <- width_cisqrt_a2$upper_ci - width_cisqrt_a2$lower_ci
res_report_H0$width_ci_a2[6] <- mean(width_cisqrt_a2$width)
res_report_H0$var_eff_a2[6] <- var(width_cisqrt_a2$treat_effect)


##########################################
# Two periods
##########################################
# list_restwop_H0
# list_restwop_H1

# one to one allocation
# H1
width_ci_a1 <- list_restwop_H1[[1]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a1$width <- width_ci_a1$upper_ci - width_ci_a1$lower_ci
res_report_H1$width_ci_a1[7] <- mean(width_ci_a1$width)
res_report_H1$var_eff_a1[7] <- var(width_ci_a1$treat_effect)

width_ci_a2 <- list_restwop_H1[[1]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a2$width <- width_ci_a2$upper_ci - width_ci_a2$lower_ci 
res_report_H1$width_ci_a2[7] <- mean(width_ci_a2$width)
res_report_H1$var_eff_a2[7] <- var(width_ci_a2$treat_effect)

# H0
width_ci_a1 <- list_restwop_H0[[1]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a1$width <- width_ci_a1$upper_ci - width_ci_a1$lower_ci
res_report_H0$width_ci_a1[7] <-mean(width_ci_a1$width)
res_report_H0$var_eff_a1[7] <- var(width_ci_a1$treat_effect)

width_ci_a2 <- list_restwop_H0[[1]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a2$width <- width_ci_a2$upper_ci - width_ci_a2$lower_ci
res_report_H0$width_ci_a2[7] <-mean(width_ci_a2$width) 
res_report_H0$var_eff_a2[7] <- var(width_ci_a2$treat_effect)

# opt allocation
# H1
width_ciopt_a1 <- list_restwop_H1[[2]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a1$width <- width_ciopt_a1$upper_ci - width_ciopt_a1$lower_ci
res_report_H1$width_ci_a1[8] <- mean(width_ciopt_a1$width)
res_report_H1$var_eff_a1[8] <- var(width_ciopt_a1$treat_effect)

width_ciopt_a2 <- list_restwop_H1[[2]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a2$width <- width_ciopt_a2$upper_ci - width_ciopt_a2$lower_ci
res_report_H1$width_ci_a2[8] <- mean(width_ciopt_a2$width)
res_report_H1$var_eff_a2[8] <- var(width_ci_a2$treat_effect)

# H0
width_ciopt_a1 <- list_restwop_H0[[2]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a1$width <- width_ciopt_a1$upper_ci - width_ciopt_a1$lower_ci
res_report_H0$width_ci_a1[8] <- mean(width_ciopt_a1$width)
res_report_H0$var_eff_a1[8] <- var(width_ciopt_a1$treat_effect)

width_ciopt_a2 <- list_restwop_H0[[2]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a2$width <- width_ciopt_a2$upper_ci - width_ciopt_a2$lower_ci
res_report_H0$width_ci_a2[8] <- mean(width_ciopt_a2$width)
res_report_H0$var_eff_a2[8] <- var(width_ci_a2$treat_effect)


# sqrt allocation
# H1
width_cisqrt_a1 <- list_restwop_H1[[3]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a1$width <- width_cisqrt_a1$upper_ci - width_cisqrt_a1$lower_ci
res_report_H1$width_ci_a1[9] <- mean(width_cisqrt_a1$width)
res_report_H1$var_eff_a1[9] <- var(width_cisqrt_a1$treat_effect)

width_cisqrt_a2 <- list_restwop_H1[[3]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a2$width <- width_cisqrt_a2$upper_ci - width_cisqrt_a2$lower_ci
res_report_H1$width_ci_a2[9] <- mean(width_cisqrt_a2$width)
res_report_H1$var_eff_a2[9] <- var(width_cisqrt_a2$treat_effect)

# H0
width_cisqrt_a1 <- list_restwop_H0[[3]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a1$width <- width_cisqrt_a1$upper_ci - width_cisqrt_a1$lower_ci
res_report_H0$width_ci_a1[9] <- mean(width_cisqrt_a1$width)
res_report_H0$var_eff_a1[9] <- var(width_cisqrt_a1$treat_effect)

width_cisqrt_a2 <- list_restwop_H0[[3]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a2$width <- width_cisqrt_a2$upper_ci - width_cisqrt_a2$lower_ci
res_report_H0$width_ci_a2[9] <- mean(width_cisqrt_a2$width)
res_report_H0$var_eff_a2[9] <- var(width_cisqrt_a2$treat_effect)

##########################################
# Results
res_report_H0 

res_report_H0 <- res_report_H0[,c(3:5, 7, 1:2, 8:11)]
res_report_H0$r1 <- as.numeric(res_report_H0$r1)
res_report_H0$r2 <- as.numeric(res_report_H0$r2)
res_report_H0$rt_a1 <- as.numeric(res_report_H0$rt_a1)
res_report_H0$rt_a2 <- as.numeric(res_report_H0$rt_a2)
# res_report_H0$minrt <- as.numeric(res_report_H0$minrt)

knitr::kable(res_report_H0, format = "latex", 
             caption = c("Type 1 error (T1E) rates for each design according to the optimal allocation strategy"), 
             col.names = c("r1","r2", "Allocation", "Design", "T1E A1","T1E A2", "CI Width A1", "CI Width A2", "Variance A1", "Variance A2"),
             digits=3)

# 

res_report_H1

res_report_H1 <- res_report_H1[,c(3:5, 7, 1:2, 8:11)]
res_report_H1$r1 <- as.numeric(res_report_H1$r1)
res_report_H1$r2 <- as.numeric(res_report_H1$r2)
res_report_H1$rt_a1 <- as.numeric(res_report_H1$rt_a1)
res_report_H1$rt_a2 <- as.numeric(res_report_H1$rt_a2)
# res_report_H1$minrt <- as.numeric(res_report_H1$minrt)

knitr::kable(res_report_H1, format = "latex", 
             caption = c("Power for each design according to the optimal allocation strategy"), 
             col.names = c("r1","r2", "Allocation", "Design", "Power A1","Power A2", "CI Width A1", "CI Width A2", "Variance A1", "Variance A2"),
             digits=3) 

