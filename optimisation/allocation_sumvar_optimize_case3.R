#################################
# Optimisation sum of variances
# December 2023
#################################

rm(list = ls())
library(latex2exp)
library(tidyverse)
setwd("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/optimisation")
#################################
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

fr1=function(r1,steps=100) f(r1,seq(.5-r1,1-r1,length.out=steps)) 
#################################


#################################
resultssum <- read_csv("resultssum.csv", col_names = FALSE)
head(resultssum) 

#################################
results1 <- subset(resultssum, resultssum$X1==0.1)
results1 <- rename(results1, r1=X1, r2=X2, r02=X3, r22=X4)

# summary((subset(resultssum, resultssum$X1==0.1)))
results1$r12 <- 1- results1$r02-results1$r22
head(results1) 

pdf(paste("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/optimisation/sumvar_case3_r1_",results1$r1,".pdf",sep=""))
plot(results1$r2,results1$r02,type="l",ylim=c(0,.5),xlim=c(0,1),xlab=TeX("$r_{2}$"),ylab="Allocation Ratios Period 2",lty=1, lwd=2, main=TeX(paste("$r_{1}=$",results1$r1,sep="")))
abline(h=0,col="grey")
abline(h=sqrt(2)/(2+sqrt(2)),col="grey")
abline(v=1-2*c(results1$r1),col="grey")
lines(results1$r2,results1$r12,col="red",lty=1, lwd=2)
lines(results1$r2,results1$r22,col="blue",lty=1, lwd=2)
lines(results1$r2,results1$r02,col="black",lty=1, lwd=2)

res=fr1(0.1)  
lines(res[2,],res[4,]/res[2,],col="red",lty=2)
lines(res[2,],res[5,]/res[2,],col="blue",lty=2)
lines(res[2,],res[3,]/res[2,],col="black",lty=2)
dev.off()
# in dotted lines - optimisation using max variances
# lty=3, lwd=2


#################################
results1 <- subset(resultssum, resultssum$X1==0.3)
results1 <- rename(results1, r1=X1, r2=X2, r02=X3, r22=X4)

# summary((subset(resultssum, resultssum$X1==0.1)))
results1$r12 <- 1- results1$r02-results1$r22
head(results1) 

pdf(paste("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/optimisation/sumvar_case3_r1_",results1$r1,".pdf",sep=""))
plot(results1$r2,results1$r02,type="l",ylim=c(0,.5),xlim=c(0,1),xlab=TeX("$r_{2}$"),ylab="Allocation Ratios Period 2",lty=1, lwd=2, main=TeX(paste("$r_{1}=$",results1$r1,sep="")))
abline(h=0,col="grey")
abline(h=sqrt(2)/(2+sqrt(2)),col="grey")
abline(v=1-2*c(results1$r1),col="grey")
lines(results1$r2,results1$r12,col="red",lty=1, lwd=2)
lines(results1$r2,results1$r22,col="blue",lty=1, lwd=2)
lines(results1$r2,results1$r02,col="black",lty=1, lwd=2)

res=fr1(0.3)  
lines(res[2,],res[4,]/res[2,],col="red",lty=2)
lines(res[2,],res[5,]/res[2,],col="blue",lty=2)
lines(res[2,],res[3,]/res[2,],col="black",lty=2)
dev.off()
# in dotted lines - optimisation using max variances
# lty=3, lwd=2


#################################
results1 <- subset(resultssum, resultssum$X1==0.4)
results1 <- rename(results1, r1=X1, r2=X2, r02=X3, r22=X4)

# summary((subset(resultssum, resultssum$X1==0.1)))
results1$r12 <- 1- results1$r02-results1$r22
head(results1) 

pdf(paste("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/optimisation/sumvar_case3_r1_",results1$r1,".pdf",sep=""))
plot(results1$r2,results1$r02,type="l",ylim=c(0,.5),xlim=c(0,1),xlab=TeX("$r_{2}$"),ylab="Allocation Ratios Period 2",lty=1, lwd=2, main=TeX(paste("$r_{1}=$",results1$r1,sep="")))
abline(h=0,col="grey")
abline(h=sqrt(2)/(2+sqrt(2)),col="grey")
abline(v=1-2*c(results1$r1),col="grey")
lines(results1$r2,results1$r12,col="red",lty=1, lwd=2)
lines(results1$r2,results1$r22,col="blue",lty=1, lwd=2)
lines(results1$r2,results1$r02,col="black",lty=1, lwd=2)

res=fr1(0.4)  
lines(res[2,],res[4,]/res[2,],col="red",lty=2)
lines(res[2,],res[5,]/res[2,],col="blue",lty=2)
lines(res[2,],res[3,]/res[2,],col="black",lty=2)
dev.off()
# in dotted lines - optimisation using max variances
# lty=3, lwd=2


#################################
results1 <- subset(resultssum, resultssum$X1==0.49)
results1 <- rename(results1, r1=X1, r2=X2, r02=X3, r22=X4)

# summary((subset(resultssum, resultssum$X1==0.1)))
results1$r12 <- 1- results1$r02-results1$r22
head(results1) 

pdf(paste("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/optimisation/sumvar_case3_r1_",results1$r1,".pdf",sep=""))
plot(results1$r2,results1$r02,type="l",ylim=c(0,.5),xlim=c(0,1),xlab=TeX("$r_{2}$"),ylab="Allocation Ratios Period 2",lty=1, lwd=2, main=TeX(paste("$r_{1}=$",results1$r1,sep="")))
abline(h=0,col="grey")
abline(h=sqrt(2)/(2+sqrt(2)),col="grey")
abline(v=1-2*c(results1$r1),col="grey")
lines(results1$r2,results1$r12,col="red",lty=1, lwd=2)
lines(results1$r2,results1$r22,col="blue",lty=1, lwd=2)
lines(results1$r2,results1$r02,col="black",lty=1, lwd=2)

res=fr1(0.49)  
lines(res[2,],res[4,]/res[2,],col="red",lty=2)
lines(res[2,],res[5,]/res[2,],col="blue",lty=2)
lines(res[2,],res[3,]/res[2,],col="black",lty=2)
dev.off()
# in dotted lines - optimisation using max variances
# lty=3, lwd=2