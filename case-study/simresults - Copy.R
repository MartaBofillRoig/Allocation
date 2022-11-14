##########################################
# Optimal allocation
# Simulation study - results
# 2022-Nov
# Marta Bofill Roig
##########################################

rm(list = ls())

library(tidyverse)

load("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/case-study/results/simstudy_completeresults_withvar_nov_pwadj.RData")
# load("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/case-study/results/simstudy_completeresults_withvar_nov.RData")

df_res<-df_res %>%
  mutate(design = case_when(
    as.numeric(df_res$r1)==1 ~ "1-period",
    as.numeric(df_res$r1)+as.numeric(df_res$r2)==1 & as.numeric(df_res$r2)>0 ~ "2-period",
    as.numeric(df_res$r1)+as.numeric(df_res$r2)<1 ~ "3-period"
  ))


df_res$r1 <- as.numeric(df_res$r1)
df_res$r2 <- as.numeric(df_res$r2)
df_res$rt_a1 <- as.numeric(df_res$rt_a1)
df_res$rt_a2 <- as.numeric(df_res$rt_a2)
df_res$var_e1 <- as.numeric(df_res$var_e1)
df_res$var_e2 <- as.numeric(df_res$var_e2)

# summary tables

res_report_H1 <- df_res %>% filter(H0=="FALSE") %>% select(rt_a1,rt_a2,var_e1,var_e2,r1,r2,alloc,minrt,design)
knitr::kable(res_report_H1, format = "markdown", caption = c("Power comparisons"), digits=3)

res_report_H0 <- df_res %>% filter(H0=="TRUE") %>% select(rt_a1,rt_a2,var_e1,var_e2,r1,r2,alloc,minrt,design)
knitr::kable(res_report_H0, format = "markdown", caption = c("Type 1 error rate"), digits=3)


res_report_H1$width_ci_a1 <- c(rep(NA,dim(res_report_H1)[1]))
res_report_H1$width_ci_a2 <- c(rep(NA,dim(res_report_H1)[1]))
res_report_H0$width_ci_a1 <- c(rep(NA,dim(res_report_H0)[1]))
res_report_H0$width_ci_a2 <- c(rep(NA,dim(res_report_H0)[1])) 




##########################################
# 3-periods symmetric
##########################################
# list_res_H1 = list(y,y_opt,y_sqrt)

# one to one allocation
# H1
width_ci_a1 <- list_res_sym_H1[[1]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a1$width <- width_ci_a1$upper_ci - width_ci_a1$lower_ci 
res_report_H1$width_ci_a1[1] <- mean(width_ci_a1$width) 

width_ci_a2 <- list_res_sym_H1[[1]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a2$width <- width_ci_a2$upper_ci - width_ci_a2$lower_ci 
res_report_H1$width_ci_a2[1] <- mean(width_ci_a2$width)  

# boxplot(width_ci_a1$width-width_ci_a2$width)


# H0
width_ci_a1 <- list_res_sym_H0[[1]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a1$width <- width_ci_a1$upper_ci - width_ci_a1$lower_ci 
res_report_H0$width_ci_a1[1] <-mean(width_ci_a1$width) 


width_ci_a2 <- list_res_sym_H0[[1]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a2$width <- width_ci_a2$upper_ci - width_ci_a2$lower_ci 
res_report_H0$width_ci_a2[1] <-mean(width_ci_a2$width)  

# opt allocation
# H1
width_ciopt_a1 <- list_res_sym_H1[[2]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a1$width <- width_ciopt_a1$upper_ci - width_ciopt_a1$lower_ci 
res_report_H1$width_ci_a1[2] <- mean(width_ciopt_a1$width) 


width_ciopt_a2 <- list_res_sym_H1[[2]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a2$width <- width_ciopt_a2$upper_ci - width_ciopt_a2$lower_ci 
res_report_H1$width_ci_a2[2] <- mean(width_ciopt_a2$width) 

# boxplot(width_ciopt_a1$width-width_ciopt_a2$width)

# H0
width_ciopt_a1 <- list_res_sym_H0[[2]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a1$width <- width_ciopt_a1$upper_ci - width_ciopt_a1$lower_ci 
res_report_H0$width_ci_a1[2] <- mean(width_ciopt_a1$width) 


width_ciopt_a2 <- list_res_sym_H0[[2]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a2$width <- width_ciopt_a2$upper_ci - width_ciopt_a2$lower_ci 
res_report_H0$width_ci_a2[2] <- mean(width_ciopt_a2$width) 

# sqrt allocation
# H1
width_cisqrt_a1 <- list_res_sym_H1[[3]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a1$width <- width_cisqrt_a1$upper_ci - width_cisqrt_a1$lower_ci 
res_report_H1$width_ci_a1[3] <- mean(width_cisqrt_a1$width) 

width_cisqrt_a2 <- list_res_sym_H1[[3]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a2$width <- width_cisqrt_a2$upper_ci - width_cisqrt_a2$lower_ci 
res_report_H1$width_ci_a2[3] <- mean(width_cisqrt_a2$width) 

# H0
width_cisqrt_a1 <- list_res_sym_H0[[3]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a1$width <- width_cisqrt_a1$upper_ci - width_cisqrt_a1$lower_ci 
res_report_H0$width_ci_a1[3] <- mean(width_cisqrt_a1$width) 


width_cisqrt_a2 <- list_res_sym_H0[[3]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a2$width <- width_cisqrt_a2$upper_ci - width_cisqrt_a2$lower_ci 
res_report_H0$width_ci_a2[3] <- mean(width_cisqrt_a2$width) 


##########################################
# 3-periods nonsymmetric
##########################################
# list_res_H1 = list(y,y_opt,y_sqrt)

# one to one allocation
# H1
width_ci_a1 <- list_res_nsym_H1[[1]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a1$width <- width_ci_a1$upper_ci - width_ci_a1$lower_ci 
res_report_H1$width_ci_a1[4] <- mean(width_ci_a1$width) 


width_ci_a2 <- list_res_nsym_H1[[1]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a2$width <- width_ci_a2$upper_ci - width_ci_a2$lower_ci 
res_report_H1$width_ci_a2[4] <-mean(width_ci_a2$width)  

# H0
width_ci_a1 <- list_res_nsym_H0[[1]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a1$width <- width_ci_a1$upper_ci - width_ci_a1$lower_ci 
res_report_H0$width_ci_a1[4] <-mean(width_ci_a1$width) 

width_ci_a2 <- list_res_nsym_H0[[1]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a2$width <- width_ci_a2$upper_ci - width_ci_a2$lower_ci 
res_report_H0$width_ci_a2[4] <-mean(width_ci_a2$width)  

# opt allocation
# H1
width_ciopt_a1 <- list_res_nsym_H1[[2]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a1$width <- width_ciopt_a1$upper_ci - width_ciopt_a1$lower_ci 
res_report_H1$width_ci_a1[5] <- mean(width_ciopt_a1$width) 


width_ciopt_a2 <- list_res_nsym_H1[[2]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a2$width <- width_ciopt_a2$upper_ci - width_ciopt_a2$lower_ci 
res_report_H1$width_ci_a2[5] <- mean(width_ciopt_a2$width) 

# boxplot(width_ciopt_a1$width-width_ciopt_a2$width)

# H0
width_ciopt_a1 <- list_res_nsym_H0[[2]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a1$width <- width_ciopt_a1$upper_ci - width_ciopt_a1$lower_ci 
res_report_H0$width_ci_a1[5] <- mean(width_ciopt_a1$width) 

width_ciopt_a2 <- list_res_nsym_H0[[2]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a2$width <- width_ciopt_a2$upper_ci - width_ciopt_a2$lower_ci 
res_report_H0$width_ci_a2[5] <- mean(width_ciopt_a2$width) 

# sqrt allocation
# H1
width_cisqrt_a1 <- list_res_nsym_H1[[3]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a1$width <- width_cisqrt_a1$upper_ci - width_cisqrt_a1$lower_ci 
res_report_H1$width_ci_a1[6] <- mean(width_cisqrt_a1$width) 


width_cisqrt_a2 <- list_res_nsym_H1[[3]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a2$width <- width_cisqrt_a2$upper_ci - width_cisqrt_a2$lower_ci 
res_report_H1$width_ci_a2[6] <- mean(width_cisqrt_a2$width) 

# H0
width_cisqrt_a1 <- list_res_nsym_H0[[3]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a1$width <- width_cisqrt_a1$upper_ci - width_cisqrt_a1$lower_ci 
res_report_H0$width_ci_a1[6] <- mean(width_cisqrt_a1$width)

width_cisqrt_a2 <- list_res_nsym_H0[[3]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a2$width <- width_cisqrt_a2$upper_ci - width_cisqrt_a2$lower_ci 
res_report_H0$width_ci_a2[6] <- mean(width_cisqrt_a2$width) 


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


width_ci_a2 <- list_restwop_H1[[1]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a2$width <- width_ci_a2$upper_ci - width_ci_a2$lower_ci  
res_report_H1$width_ci_a2[7] <- mean(width_ci_a2$width) 

# H0
width_ci_a1 <- list_restwop_H0[[1]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a1$width <- width_ci_a1$upper_ci - width_ci_a1$lower_ci 
res_report_H0$width_ci_a1[7] <-mean(width_ci_a1$width) 


width_ci_a2 <- list_restwop_H0[[1]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a2$width <- width_ci_a2$upper_ci - width_ci_a2$lower_ci 
res_report_H0$width_ci_a2[7] <-mean(width_ci_a2$width)  

# opt allocation
# H1
width_ciopt_a1 <- list_restwop_H1[[2]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a1$width <- width_ciopt_a1$upper_ci - width_ciopt_a1$lower_ci 
res_report_H1$width_ci_a1[8] <- mean(width_ciopt_a1$width) 

width_ciopt_a2 <- list_restwop_H1[[2]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a2$width <- width_ciopt_a2$upper_ci - width_ciopt_a2$lower_ci 
res_report_H1$width_ci_a2[8] <- mean(width_ciopt_a2$width) 

# H0
width_ciopt_a1 <- list_restwop_H0[[2]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a1$width <- width_ciopt_a1$upper_ci - width_ciopt_a1$lower_ci 
res_report_H0$width_ci_a1[8] <- mean(width_ciopt_a1$width) 

width_ciopt_a2 <- list_restwop_H0[[2]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a2$width <- width_ciopt_a2$upper_ci - width_ciopt_a2$lower_ci 
res_report_H0$width_ci_a2[8] <- mean(width_ciopt_a2$width) 


# sqrt allocation
# H1
width_cisqrt_a1 <- list_restwop_H1[[3]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a1$width <- width_cisqrt_a1$upper_ci - width_cisqrt_a1$lower_ci 
res_report_H1$width_ci_a1[9] <- mean(width_cisqrt_a1$width) 

width_cisqrt_a2 <- list_restwop_H1[[3]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a2$width <- width_cisqrt_a2$upper_ci - width_cisqrt_a2$lower_ci 
res_report_H1$width_ci_a2[9] <- mean(width_cisqrt_a2$width) 

# H0
width_cisqrt_a1 <- list_restwop_H0[[3]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a1$width <- width_cisqrt_a1$upper_ci - width_cisqrt_a1$lower_ci 
res_report_H0$width_ci_a1[9] <- mean(width_cisqrt_a1$width) 

width_cisqrt_a2 <- list_restwop_H0[[3]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a2$width <- width_cisqrt_a2$upper_ci - width_cisqrt_a2$lower_ci 
res_report_H0$width_ci_a2[9] <- mean(width_cisqrt_a2$width) 



##########################################
# 1-periods 
########################################## 

# one to one allocation
# H1
width_ci_a1 <- list_res_mams_H1[[1]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a1$width <- width_ci_a1$upper_ci - width_ci_a1$lower_ci 
res_report_H1$width_ci_a1[10] <- mean(width_ci_a1$width) 

width_ci_a2 <- list_res_mams_H1[[1]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a2$width <- width_ci_a2$upper_ci - width_ci_a2$lower_ci 
res_report_H1$width_ci_a2[10] <- mean(width_ci_a2$width)  

# boxplot(width_ci_a1$width-width_ci_a2$width)


# H0
width_ci_a1 <- list_res_mams_H0[[1]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a1$width <- width_ci_a1$upper_ci - width_ci_a1$lower_ci 
res_report_H0$width_ci_a1[10] <-mean(width_ci_a1$width) 


width_ci_a2 <- list_res_mams_H0[[1]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ci_a2$width <- width_ci_a2$upper_ci - width_ci_a2$lower_ci 
res_report_H0$width_ci_a2[10] <-mean(width_ci_a2$width)  

# opt allocation
# H1
width_ciopt_a1 <- list_res_mams_H1[[2]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a1$width <- width_ciopt_a1$upper_ci - width_ciopt_a1$lower_ci 
res_report_H1$width_ci_a1[11] <- mean(width_ciopt_a1$width) 


width_ciopt_a2 <- list_res_mams_H1[[2]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a2$width <- width_ciopt_a2$upper_ci - width_ciopt_a2$lower_ci 
res_report_H1$width_ci_a2[11] <- mean(width_ciopt_a2$width) 

# boxplot(width_ciopt_a1$width-width_ciopt_a2$width)

# H0
width_ciopt_a1 <- list_res_mams_H0[[2]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a1$width <- width_ciopt_a1$upper_ci - width_ciopt_a1$lower_ci 
res_report_H0$width_ci_a1[11] <- mean(width_ciopt_a1$width) 


width_ciopt_a2 <- list_res_mams_H0[[2]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_ciopt_a2$width <- width_ciopt_a2$upper_ci - width_ciopt_a2$lower_ci 
res_report_H0$width_ci_a2[11] <- mean(width_ciopt_a2$width) 

# sqrt allocation
# H1
width_cisqrt_a1 <- list_res_mams_H1[[3]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a1$width <- width_cisqrt_a1$upper_ci - width_cisqrt_a1$lower_ci 
res_report_H1$width_ci_a1[12] <- mean(width_cisqrt_a1$width) 

width_cisqrt_a2 <- list_res_mams_H1[[3]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a2$width <- width_cisqrt_a2$upper_ci - width_cisqrt_a2$lower_ci 
res_report_H1$width_ci_a2[12] <- mean(width_cisqrt_a2$width) 

# H0
width_cisqrt_a1 <- list_res_mams_H0[[3]] %>% filter(arm == "a1") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a1$width <- width_cisqrt_a1$upper_ci - width_cisqrt_a1$lower_ci 
res_report_H0$width_ci_a1[12] <- mean(width_cisqrt_a1$width) 


width_cisqrt_a2 <- list_res_mams_H0[[3]] %>% filter(arm == "a2") %>% select(lower_ci, upper_ci, treat_effect)
width_cisqrt_a2$width <- width_cisqrt_a2$upper_ci - width_cisqrt_a2$lower_ci 
res_report_H0$width_ci_a2[12] <- mean(width_cisqrt_a2$width) 




##########################################
# summary tables

# res_report_H1table <- df_res %>% filter(H0=="FALSE") #%>% select(rt_a1,rt_a2,var_e1,var_e2,r1,r2,alloc,minrt,design)

table_H1 <- res_report_H1[c(9, 5:7, 1:2, 10:11, 3:4)]
knitr::kable(table_H1, format = "latex", caption = c("Power comparisons"), col.names = c("Design", "r1","r2", "Allocation", "Power A1","Power A2", "CI Width A1", "CI Width A2", "Variance A1", "Variance A2"), digits=3)

# res_report_H0table <- df_res %>% filter(H0=="TRUE") %>% select(rt_a1,rt_a2,var_e1,var_e2,r1,r2,alloc,minrt,design)
table_H0 <- res_report_H0[c(9, 5:7, 1:2, 10:11, 3:4)]
knitr::kable(table_H0, format = "latex", caption = c("Type 1 error rate comparisons"), col.names = c("Design",  "r1","r2", "Allocation", "T1E A1","T1E A2","CI Width A1", "CI Width A2", "Variance A1", "Variance A2"), digits=3)

##########################################
# Large sample sizes

load("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/case-study/results/simstudy_completeresults_withvar_nov_lss.RData")
df_res$design = ifelse(as.numeric(df_res$r1)+as.numeric(df_res$r2)==1,"2-period", "3-period")

df_res$r1 <- as.numeric(df_res$r1)
df_res$r2 <- as.numeric(df_res$r2)
df_res$rt_a1 <- as.numeric(df_res$rt_a1)
df_res$rt_a2 <- as.numeric(df_res$rt_a2)
df_res$var_e1 <- as.numeric(df_res$var_e1)
df_res$var_e2 <- as.numeric(df_res$var_e2)

# summary tables

res_report_H1 <- df_res %>% filter(H0=="FALSE") %>% select(rt_a1,rt_a2,var_e1,var_e2,r1,r2,alloc,minrt,design)
knitr::kable(res_report_H1, format = "markdown", caption = c("Power comparisons"), digits=3)

res_report_H0 <- df_res %>% filter(H0=="TRUE") %>% select(rt_a1,rt_a2,var_e1,var_e2,r1,r2,alloc,minrt,design)
knitr::kable(res_report_H0, format = "markdown", caption = c("Type 1 error rate"), digits=3)
