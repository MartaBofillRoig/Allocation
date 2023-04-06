# aux_functions - same functions as in the shiny app
# NCC 


library(nloptr)
library(parallel)
library(latex2exp) 
################Functions NCC #################################
# Objective Function
#x=c(r12,r22)
eval_f <- function(x,r1,r2)
{
  return(-(r1/4 + x[1] + x[1]^2/(-r2 + x[2]) ))
}

### Gradient
eval_grad_f <- function(x,r1,r2) {
  return( c(-1 + (2 *x[1])/(r2 - x[2]), x[1]^2/(r2 - x[2])^2) )
}
##
eval_jac_g_ineq=function(x,r1,r2) {
  return( c(1,1) )
}

eval_jac_g_eq=function(x,r1,r2) {
  r12=x[1]
  r22=x[2]
  return( c(1 + (8* r12 *(r1 + 2* r12)* r22^2)/(r1 *r2 + 4 *r12 *(-r12 + r2))^2 + (
    2* r12)/(-r2 + r22),-1 - r12^2/(r2 - r22)^2 + (2 *(r1 + 4 *r12) *r22)/(
      r1* r2 + 4 *r12* (-r12 + r2))) )
}

# Inequality constraints (need to be of the form g(x)<=0)
eval_g_ineq <- function(x,r1,r2)
{
  return (sum(x)-r2)
}

eval_g_eq <- function(x,r1,r2)
{ r3=1-r1-r2
r12=x[1]
r22=x[2]
return ( r1/4 + r12 + r12^2/(-r2 + r22) - (r22 - ((r1 + 4* r12)* r22^2)/(r1 *r2 + 4* r12 *(-r12 + r2)) + r3/4) )
}
# Lower and upper bounds
lb <- c(0,0)
ub <- c(1,1)#initial values
x0 <- c(0.1,0.1)

# Set optimization options.#NLOPT_LD_MMA
local_opts <- list( "algorithm" = "NLOPT_LD_SLSQP", "xtol_rel" = 1.0e-15 )
opts <- list( "algorithm"= "NLOPT_LD_SLSQP",#NLOPT_GN_ISRES
              "xtol_rel"= 1.0e-15,
              "maxeval"= 260000,
              "local_opts" = local_opts,
              "print_level" = 0 )

optncc <- Vectorize( function(r1,r2) {
  res=nloptr( x0 = c(r2/3,r2/3),
              eval_f = eval_f,
              eval_grad_f=eval_grad_f,
              eval_jac_g_ineq=eval_jac_g_ineq,
              eval_jac_g_eq=eval_jac_g_eq,
              lb = lb,
              ub = ub,
              eval_g_ineq = eval_g_ineq,
              eval_g_eq = eval_g_eq,
              opts = opts,r1=r1,r2=r2
  )$solution
  c(r1,r2,res/r2)
},vectorize.args = "r2")

res=t(optncc(r1=0.2,r2=c(0.5)))
print(res)

optr1=function(r1)
{ r2v=seq(0.5-r1,1-r1,ifelse(r1>0.45,0.002,0.01))
#  res=t(optncc(r1,r2=r2v))
if(r1<0.5) res=suppressWarnings(matrix(pvec(r2v,optncc,r1=r1,mc.cores=1),byrow=T,ncol=4))
else res=cbind(c(r1,r1),c(0,.5),c(0,0),c(.5,.5))
return(res)
}
# mc.cores=max(1,detectCores()-2)




