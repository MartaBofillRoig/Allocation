##########################################
# Optimal allocation
# Functions
# 2022-Oct
# Marta Bofill Roig
##########################################


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
# f(r1=20/70,r2=30/70)


##########################################
# Simulation function
# sim_designs()
##########################################

sim_designs <- function(r1,r2,mu0,mu1,mu2,N,alloc="sqrt",trend="stepwise",sl=0.2){
  
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
  
  # c(r11,r01,r22,r12,r02,r23,r03)
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
  
  if(trend=="stepwise"){
    
    response = rnorm(Nsim,
                     mean=means[treatment[1:Nsim]+1]+sw_trend(cj=period[1:Nsim], lambda=sl),
                     sd=1) 
    
  }
  if(trend=="linear"){
    
    response = rnorm(Nsim,
                     mean=means[treatment[1:Nsim]+1]+linear_trend(j=1:Nsim, lambda=sl, sample_size=c(0,Nsim)),
                     sd=1) 
  }
  
  data = data.frame(response,treatment,period)
  if(r1==1){
    ss = matrix(c(n22,n11,n01,0,n12,n02,n23,0,n03), nrow=3)
  }else{
    ss = matrix(c(0,n11,n01,n22,n12,n02,n23,0,n03), nrow=3)
  }
  return(list(data=data,ss=ss))
}


##########################################
# Modeling
# models()
# models_cc()
# models_ncc()
##########################################

# # NCC pkg functions 
# fixmodel_cont(data=db,arm=2,alpha=0.025) #model using ncc
# sepmodel_adj_cont(data=db,arm=1,alpha=0.025) #model using cc only

models <- function(data,alpha=0.025){
  
  # using concurrent controls only
  
  ls_a1=fixmodel_cont(data,arm=1,alpha=alpha,ncc=F)
  ls_a1[[6]]<- vcov(summary(ls_a1$model))[2,2]
  names(ls_a1)[[6]] <- "var"
  
  ls_a1[[7]]<- "a1"
  names(ls_a1)[[7]] <- "arm"
  ls_a1[[8]] <- "cc"
  names(ls_a1)[8] <- "model"
  
  ls_a2=fixmodel_cont(data,arm=2,alpha=alpha,ncc=F)
  ls_a2[[6]]<- vcov(summary(ls_a2$model))[3,3]
  
  ls_a2[[7]] <- "a2"
  names(ls_a2)[[7]] <- "arm"
  ls_a2[[8]] <- "cc"
  names(ls_a2)[8] <- "model"
  
  # using concurrent and non-concurrent controls
  
  lf_a1=fixmodel_cont(data,arm=1,alpha=alpha)
  lf_a1[[6]]<- vcov(summary(lf_a1$model))[2,2]
  names(lf_a1)[[6]] <- "var"
  
  lf_a1[[7]] <- "a1"
  names(lf_a1)[[7]] <- "arm"
  lf_a1[[8]] <- "ncc"
  names(lf_a1)[8] <- "model"
  
  lf_a2=fixmodel_cont(data,arm=2,alpha=alpha)
  lf_a2[[6]]<- vcov(summary(lf_a2$model))[3,3]
  names(lf_a2)[[6]] <- "var"
  
  lf_a2[[7]] <- "a2"
  names(lf_a2)[[7]] <- "arm"
  lf_a2[[8]] <- "ncc"
  names(lf_a2)[8] <- "model"
  
  return(list(ls_a1,ls_a2,lf_a1,lf_a2))
}

models_cc <- function(data,alpha=0.025){
  
  # using concurrent controls only
  
  ls_a1=fixmodel_cont(data,arm=1,alpha=alpha,ncc=F)
  ls_a1[[6]]<- vcov(summary(ls_a1$model))[2,2]
  names(ls_a1)[[6]] <- "var"
  
  ls_a1[[7]]<- "a1"
  names(ls_a1)[[7]] <- "arm"   
  
  ls_a2=fixmodel_cont(data,arm=2,alpha=alpha,ncc=F)
  ls_a2[[6]]<- vcov(summary(ls_a2$model))[3,3]
  names(ls_a2)[[6]] <- "var"
  
  ls_a2[[7]] <- "a2"
  names(ls_a2)[[7]] <- "arm" 
  
  return(list(ls_a1,ls_a2))
}


models_ncc <- function(data,alpha=0.025){ 
  
  # using concurrent and non-concurrent controls
  
  lf_a1=fixmodel_cont(data,arm=1,alpha=alpha)
  lf_a1[[6]] <- "a1"
  names(lf_a1)[[6]] <- "arm"
  # lf_a1[[7]] <- "ncc"
  # names(lf_a1)[7] <- "model"
  
  lf_a2=fixmodel_cont(data,arm=2,alpha=alpha)
  lf_a2[[6]] <- "a2"
  names(lf_a2)[[6]] <- "arm"
  # lf_a2[[7]] <- "ncc"
  # names(lf_a2)[7] <- "model" 
  
  return(list(lf_a1,lf_a2))
}

