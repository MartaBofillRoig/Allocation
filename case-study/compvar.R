
##########################################
# Optimal allocation
# Case study
# 2022-Oct
# Marta Bofill Roig
##########################################

library(tidyverse)

##########################################
# Optimal allocations
# for case 3
##########################################

Power=function(x,y)x^y

eq=function(r22,r1=0.1,r2=0.8) (r1 + ((Power(r2,2) - 4*r2*r22 + 2*Power(r22,2))*Power(Power(r2,2) - 2*r2*r22 + 2*Power(r22,2),2))/(Power(r2,2)*Power(r2 - r22,3)) - (1-r1-r2))/4.

fr12=function(r22,r1=0.1,r2=0.8) (r2*(r2 - 2*r22))/(2*(r2 - r22))

f=Vectorize(function(r1,r2) { 
  r22=uniroot(eq,c(0,r2),r1=r1,r2=r2)$root
  r12=fr12(r22,r1,r2)
  r02=r2-r12-r22
  sol=c(r1,r2,r02,r12,r22)
  sol
})
x<-f(r1=20/70,r2=30/70)

##########################################
# Variance comparison
##########################################

# lower=0, upper=1, extendInt = "yes"

Power=function(x,y)x^y

cond_eq <- function(r22,r12,r1,r2){
  r1/4 + r12 + Power(r12,2)/(-r2 + r22) + r22*(-1 - r22/(r12 - r2)) - (1-r1-r2)/4
}

inv_var <- function(r12,r1,r2,eps=0.0001){
  
  r3 <- 1 - r1 - r2 
  r11 <- r01 <- r1/2
  r23 <- r03 <- r3/2
  r22 <- tryCatch(uniroot(cond_eq,c(0,r2-eps),r12=r12,r1=r1,r2=r2)$root, error=NA)  
  
  if(is.na(r22)){
    r02<-NA
    term1<-NA
  }else{
    r02 <- r2 - r22 - r12
    term1 <- r1/4 + r12 + Power(r12,2)/(-r2 + r22)
  }

  return(c(r1,r2,r02,r12,r22,1/term1))
  # return(c(r1=r1,r2=r2,r02=r02,r12=r12,r22=r22,inv_var=1/term1))
}

##########################################
# test
##########################################

# inv_var(r12=x[4,],r1=20/70,r2=30/70)
r12=x[4,];r1=20/70;r2=30/70
# c(r12=x[4,],r1=20/70,r2=30/70)
# seq(x[4,],r2,0.1)
# seq(x[4,],r2,0.1)
# matrix(unlist(lapply(seq(0.01,r2,0.001), inv_var, r1=20/70,r2=30/70)),ncol=6, byrow = T)

res <- lapply(seq(0.01,r2,0.001), inv_var, r1=r1,r2=r2)
matrix_res <- matrix(unlist(res),ncol=6, byrow = T)
data_res <- as.data.frame(matrix_res)

data_res <- data_res %>% 
  rename(
    r1 = V1, 
    r2 = V2,
    r02 = V3,
    r12 = V4,
    r22 = V5,
    var = V6
  ) 

# sum(data_res$r02>0)
data_rescl <- data_res %>% filter(r02>0)
head(data_rescl)

plot(data_rescl$r12,data_rescl$var,pch=19, cex=0.1)
abline(v=x[4,],col="grey")

##########################################
# 2-period design
##########################################
N = 80
N1 = round(N/4)
N2 = round(N-N1)
c(N1,N2,N-N1-N2)

r1=N1/N
r2=N2/N

res <- lapply(seq(0.01,r2,0.01), inv_var, r1=r1,r2=r2)
matrix_res <- matrix(unlist(res),ncol=6, byrow = T)
data_res <- as.data.frame(matrix_res)

data_res <- data_res %>% 
  rename(
    r1 = V1, 
    r2 = V2,
    r02 = V3,
    r12 = V4,
    r22 = V5,
    var = V6
  ) 

# sum(data_res$r02>0)
data_rescl <- data_res %>% filter(r02>0)
head(data_rescl)

plot(data_rescl$r12,data_rescl$var,pch=19, cex=0.1)
# abline(v=x[4,],col="grey")

##########################################
# 3-period design
##########################################
N1 = round(N/3)
N2 = round(2*(N-N1)/3)
c(N1,N2,N-N1-N2) 

r1=N1/N
r2=N2/N


res <- lapply(seq(0.01,r2,0.01), inv_var, r1=r1,r2=r2)
# is.list(res)
# unlist(res)

matrix_res <- matrix(unlist(res),ncol=6, byrow = T)
data_res <- as.data.frame(matrix_res)

data_res <- data_res %>% 
  rename(
    r1 = V1, 
    r2 = V2,
    r02 = V3,
    r12 = V4,
    r22 = V5,
    var = V6
  ) 

head(data_res,10)

# sum(data_res$r02>0)
data_rescl <- data_res %>% filter(r02>0)
head(data_rescl)

plot(data_rescl$r12,data_rescl$var,pch=19, cex=0.1)
# abline(v=x[4,],col="grey")
