##########################################
# Optimal allocation
# Simulation study - results
# 2022-Oct
# Marta Bofill Roig
##########################################

library(tidyverse)

load("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/case-study/results/simstudy_results.RData")
df_res$design = ifelse(as.numeric(df_res$r1)+as.numeric(df_res$r2)==1,"2-period", "3-period")
# df_res$r1+df_res$r2

res_report_H1 <- df_res %>% filter(H0=="FALSE") %>% select(rt_a1,rt_a2,r1,r2,alloc,minrt,design)
knitr::kable(res_report_H1, format = "markdown", caption = c("Power comparisons"))

res_report_H0 <- df_res %>% filter(H0=="TRUE") %>% select(rt_a1,rt_a2,r1,r2,alloc,minrt,design)
knitr::kable(res_report_H0, format = "markdown", caption = c("Type 1 error rate"))



