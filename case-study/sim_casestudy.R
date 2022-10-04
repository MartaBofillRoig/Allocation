
##########################################
# Optimal allocation
# Case study
# 2022-Oct
# Marta Bofill Roig
##########################################

rm(list = ls())
# devtools::install_github("pavlakrotka/NCC@v1.0")
library(NCC)

##########################################
# Optimal allocations
##########################################

# Case 3
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
# f(r1=20/70,r2=30/70)


##########################################
# Simulation function
##########################################

sim_designs <- function(r1,r2,mu0,mu1,mu2,N,alloc="sqrt",sl=0.2){
  
  r3 = 1-r1-r2
  
  if(r1 == 1){
    
    if(alloc == "one"){
      r22 <- r11 <- r01 <- r1/3
      r02 <- r12 <- r23 <- r03 <- 0
    }
    if(alloc != "one"){ 
      r01 <- sqrt(2)/(2+sqrt(2))
      r22 <- r11 <- (1-r01)/2 
      r02 <- r12 <- r23 <- r03 <- 0
    }
    
  }else{
    
    if(alloc == "opt"){ 
      
      r11 <- r01 <- r1/2
      
      r2_opt <- f(r1=r1,r2=r2)
      r02 <- r2_opt[3]
      r12 <- r2_opt[4]
      r22 <- r2_opt[5]
      
      r23 <- r03 <- r3/2
      
    }
    if(alloc == "one"){
      r11 <- r01 <- r1/2
      r22 <- r12 <- r02 <- r2/3
      r23 <- r03 <- r3/2
    }
    if(alloc == "sqrt"){
      r11 <- r01 <- r1/2
      r02 <- r2*sqrt(2)/(2+sqrt(2))
      r22 <- r12 <- (r2-r02)/2
      r23 <- r03 <- r3/2
    }
    
  }
  
  
  # c(r11,r01)
  
  n11 = round(r11*N)
  n01 = round(r01*N)
  
  n22 = round(r22*N)
  n12 = round(r12*N)
  n02 = round(r02*N)
  
  n23 = round(r23*N)
  n03 = round(r03*N)
  
  # c(n11,n01,n22,n12,n02,n23,n03)
  
  means = c(mu0,mu1,mu2)
  
  treatment = c(
    sample(c(rep(1,n11),rep(0,n01))),
    sample(c(rep(2,n22),rep(1,n12),rep(0,n02))),
    sample(c(rep(2,n23),rep(0,n03)))
  )
  
  Nsim = length(treatment) 
  
  if(r1==1){
    treatment = sample(treatment)
    period = rep(1,Nsim)
  }else{
    period = c(
      rep(1,n11+n01),
      rep(2,n22+n12+n02),
      rep(3,n23+n03)
    )
  }
  
  response = rnorm(Nsim,
                   mean=means[treatment[1:Nsim]+1]+sw_trend(cj=1:Nsim, lambda=sl),
                   sd=1) 
  
  data = data.frame(response,treatment,period)
  return(data)
}


# r1=20/70;r2=30/70;mu0=0;mu1=1;mu2=1;N=70;alloc="opt"
db=sim_designs(r1=20/70,r2=30/70,mu0=0,mu1=1,mu2=1,N=70,alloc="one")
head(db)
plot_trial(db$treatment) 

db=sim_designs(r1=20/70,r2=30/70,mu0=0,mu1=1,mu2=1,N=70,alloc="opt")
head(db)
plot_trial(db$treatment) 

##########################################
# Modeling
##########################################

# # NCC pkg functions 
# fixmodel_cont(data=db,arm=2,alpha=0.025) #model using ncc
# sepmodel_adj_cont(data=db,arm=1,alpha=0.025) #model using cc only

models <- function(data,alpha=0.025){

  # using concurrent controls only
    
  ls_a1=sepmodel_adj_cont(data,arm=1,alpha=alpha)
  ls_a1[[6]] <- "cc"
  names(ls_a1)[6] <- "model" 
  ls_a1[[7]]<- "a1"
  names(ls_a1)[[7]] <- "arm"
  
  ls_a2=sepmodel_adj_cont(data,arm=2,alpha=alpha)
  ls_a2[[6]] <- "cc"
  names(ls_a2)[6] <- "model" 
  ls_a2[[7]] <- "a2"
  names(ls_a2)[[7]] <- "arm"
  
  # using concurrent and non-concurrent controls
  
  lf_a1=fixmodel_cont(data,arm=1,alpha=alpha)
  lf_a1[[6]] <- "ncc"
  names(lf_a1)[6] <- "model" 
  lf_a1[[7]] <- "a1"
  names(lf_a1)[[7]] <- "arm"
  
  lf_a2=fixmodel_cont(data,arm=2,alpha=alpha)
  lf_a2[[6]] <- "ncc"
  names(lf_a2)[6] <- "model" 
  lf_a2[[7]] <- "a2"
  names(lf_a2)[[7]] <- "arm"
  
  return(list(lf_a1,lf_a2,ls_a1,ls_a2))
}


res = do.call(rbind.data.frame, models(data = db) )
head(res)



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
plot_trial(db1_opt$treatment) 

(res_one = do.call(rbind.data.frame, models(data = db1_one) ))
(res_sqrt = do.call(rbind.data.frame, models(data = db1_sqrt) ))
(res_opt = do.call(rbind.data.frame, models(data = db1_opt) ))

# design 2: two-period design
db2_one=sim_designs(r1=n_arm1/N,r2=1-n_arm1/N,mu0=mean_control,mu1=mean_arm1,mu2=mean_arm2,N=N,alloc="one")
db2_sqrt=sim_designs(r1=n_arm1/N,r2=1-n_arm1/N,mu0=mean_control,mu1=mean_arm1,mu2=mean_arm2,N=N,alloc="opt")
db2_opt=sim_designs(r1=n_arm1/N,r2=1-n_arm1/N,mu0=mean_control,mu1=mean_arm1,mu2=mean_arm2,N=N,alloc="sqrt")

plot_trial(db2_opt$treatment) 

(res_one = do.call(rbind.data.frame, models(data = db2_one) ))
(res_sqrt = do.call(rbind.data.frame, models(data = db2_sqrt) ))
(res_opt = do.call(rbind.data.frame, models(data = db2_opt) ))

# design 3: three-period design
db3_one=sim_designs(r1=n_arm1/N,r2=1-2*n_arm1/N,mu0=mean_control,mu1=mean_arm1,mu2=mean_arm2,N=N,alloc="one")
db3_sqrt=sim_designs(r1=n_arm1/N,r2=1-2*n_arm1/N,mu0=mean_control,mu1=mean_arm1,mu2=mean_arm2,N=N,alloc="opt")
db3_opt=sim_designs(r1=n_arm1/N,r2=1-2*n_arm1/N,mu0=mean_control,mu1=mean_arm1,mu2=mean_arm2,N=N,alloc="sqrt")

plot_trial(db3_opt$treatment) 

(res_one = do.call(rbind.data.frame, models(data = db3_one) ))
(res_sqrt = do.call(rbind.data.frame, models(data = db3_sqrt) ))
(res_opt = do.call(rbind.data.frame, models(data = db3_opt) ))




