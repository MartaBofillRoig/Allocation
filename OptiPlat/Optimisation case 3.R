library(nloptr)
library(parallel)
library(latex2exp)
###########Functions CC ###########################


Power=function(x,y)x^y

eq=function(r22,r1=0.1,r2=0.8) (r1 + ((Power(r2,2) - 4*r2*r22 + 2*Power(r22,2))*Power(Power(r2,2) - 2*r2*r22 + 2*Power(r22,2),2))/(Power(r2,2)*Power(r2 - r22,3)) - (1-r1-r2))/4.

fr12=function(r22,r1=0.1,r2=0.8) (r2*(r2 - 2*r22))/(2*(r2 - r22))

optcc=Vectorize(function(r1,r2) { 
  r22=uniroot(eq,c(0,r2),r1=r1,r2=r2)$root
  r12=fr12(r22,r1,r2)
  sol=c(r1,r2,r12/r2,r22/r2)
  sol
})

optccr1=function(r1)
{ r2v=seq(0.5-r1,1-r1,ifelse(r1>0.45,0.002,0.01))
  r2v[1]=r2v[1]+10^(-10) # to avoid error
#  res=t(optncc(r1,r2=r2v))
if(r1<0.5) res=suppressWarnings(matrix(pvec(r2v,optcc,r1=r1,mc.cores=1),byrow=T,ncol=4))
  else res=cbind(c(r1,r1),c(0,.5),c(0,0),c(.5,.5))
return(res)
}
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

########################Plot###

plalloc=function(r1,CC=T,NCC=T,savepdf=F)
{
results <-optr1(r1)
colnames(results)=c("r1","r2","r12","r22")
results=as.data.frame(results)
results$r02 <- 1- results$r12-results$r22

resultscc <-optccr1(r1)
colnames(resultscc)=c("r1","r2","r12","r22")
resultscc=as.data.frame(resultscc)
resultscc$r02 <- 1- resultscc$r12-resultscc$r22

###compute se
sef=function(x)- 1/eval_f(c(x[3]*x[2],x[4]*x[2]),r1=x[1],r2=x[2])
resultscc$se=apply(resultscc,1,sef)
results$se=apply(results,1,sef)
if(r1==0.5) {
  results$se=c(8,8)
  resultscc$se=c(8,8)
  }
optse=sef(c(0,1,1/(2+sqrt(2)),1/(2+sqrt(2))))

if(savepdf) pdf(paste("/Users/poschm/Desktop/Optimal Allocation Programs/case3ncc_r1_R_",r1,".pdf",sep=""))
if(!savepdf)

par(mar=c(8.1, 4.1, 4.1, 5.1), xpd=F,mfrow=c(2,1),pty="m")
lwd=2
plot(results$r2,results$r02,type="l",
     ylim=c(0,.5),xlim=c(0,1),
     col="white",
     xlab=TeX("$r_{2}$"),
     ylab="",
     main="Optimal Allocation Ratios in Period 2",lty=2
     )#main=TeX(paste("$r_{1}=$",r1,sep=""))
abline(h=0,col="grey")
abline(h=sqrt(2)/(2+sqrt(2)),col="grey")
abline(v=1-2*c(r1),col="grey")
if(CC)
  {lines(resultscc$r2,resultscc$r12,col="red",lwd=lwd)
lines(resultscc$r2,resultscc$r22,col="blue",lwd=lwd)
lines(resultscc$r2,resultscc$r02,col="black",lwd=lwd)}
if(NCC){
lines(results$r2,results$r12,col="red",lty=2,lwd=lwd)
lines(results$r2,results$r22,col="blue",lty=2,lwd=lwd)
lines(results$r2,results$r02,col="black",lty=2,lwd=lwd)
}
par( xpd=T)
legend("bottom", inset=c(0,-0.45), 
       legend=c("Control","Treatment 1","Treatment 2"), 
       col=c("black","red","blue"),lty=1,bty="n",ncol=3,lwd=lwd)
legend("bottom", inset=c(0,-0.54), 
       legend=c("Concurrent","Non-Concurrent"), 
       col=c("black","black"),lty=c(1,2),bty="n",ncol=2,lwd=lwd)
par( xpd=F)
plot(results$r2,results$se,type="l",
     ylim=c(-30,0),xlim=c(0,1),
     col="white",
     xlab=TeX("$r_{2}$"),ylab="Percent",
     main="Decrease in Variance Compared to Separate Trials",lty=2)
abline(h=-(1-optse/8)*100,col="grey")
abline(h=0,col="grey")
if (CC) lines(resultscc$r2,-(1-resultscc$se/8)*100,col="black",lty=1,lwd=lwd)
if (NCC) lines(results$r2,-(1-results$se/8)*100,col="black",lty=2,lwd=lwd)
par(pty="m")


if(savepdf) dev.off()
}
#plalloc(0.3,CC=F,NCC=T)
#plncc(0.1,savepdf=T)
#plncc(0.3,savepdf=T)
#plncc(0.4,savepdf=T)
#plncc(0.49,savepdf=T)


