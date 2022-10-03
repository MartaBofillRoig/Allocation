
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

results1 <- read_csv("results1.csv", col_names = FALSE)
results1=rename(results1,r12=X1,r22=X2)  
results1$r2 <- (40:90)/100   

results1$r02 <- 1- results1$r12-results1$r22
head(results1)  
r1=0.1

pdf(paste("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/optimisation/case3_r1_",r1,".pdf",sep=""))
plot(results1$r2,results1$r02,type="l",ylim=c(0,.5),xlim=c(0,1),xlab=TeX("$r_{2}$"),ylab="Allocation Ratios Period 2",lty=2, main=TeX(paste("$r_{1}=$",r1,sep="")))
abline(h=0,col="grey")
abline(h=sqrt(2)/(2+sqrt(2)),col="grey")
abline(v=1-2*c(r1),col="grey")
lines(results1$r2,results1$r12,col="red",lty=2)
lines(results1$r2,results1$r22,col="blue",lty=2)
lines(results1$r2,results1$r02,col="black",lty=2)

# nonconcurrent controls
res=fr1(r1)  
lines(res[2,],res[4,]/res[2,],col="red")
lines(res[2,],res[5,]/res[2,],col="blue")
lines(res[2,],res[3,]/res[2,],col="black")

dev.off()

# 

results2 <- read_csv("results2.csv", col_names = FALSE)
results2=rename(results2,r12=X1,r22=X2)  
results2$r2 <- (20:70)/100  
results2$r02 <- 1- results2$r12-results2$r22
head(results2)  
r1=0.3

pdf(paste("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/optimisation/case3_r1_",r1,".pdf",sep=""))
plot(results2$r2,results2$r02,type="l",ylim=c(0,.5),xlim=c(0,1),xlab=TeX("$r_{2}$"),ylab="Allocation Ratios Period 2",lty=2, main=TeX(paste("$r_{1}=$",r1,sep="")))
abline(h=0,col="grey")
abline(h=sqrt(2)/(2+sqrt(2)),col="grey")
abline(v=1-2*c(r1),col="grey")
lines(results2$r2,results2$r12,col="red",lty=2)
lines(results2$r2,results2$r22,col="blue",lty=2)
lines(results2$r2,results2$r02,col="black",lty=2)

# nonconcurrent controls
res=fr1(r1)  
lines(res[2,],res[4,]/res[2,],col="red")
lines(res[2,],res[5,]/res[2,],col="blue")
lines(res[2,],res[3,]/res[2,],col="black")

dev.off()

# 

results3 <- read_csv("results3.csv", col_names = FALSE)
results3=rename(results3,r12=X1,r22=X2)  
results3$r2 <- (10:60)/100  
results3$r02 <- 1- results3$r12-results3$r22
head(results3)  
r1=0.4

pdf(paste("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/optimisation/case3_r1_",r1,".pdf",sep=""))
plot(results3$r2,results3$r02,type="l",ylim=c(0,.5),xlim=c(0,1),xlab=TeX("$r_{2}$"),ylab="Allocation Ratios Period 2",lty=2, main=TeX(paste("$r_{1}=$",r1,sep="")))
abline(h=0,col="grey")
abline(h=sqrt(2)/(2+sqrt(2)),col="grey")
abline(v=1-2*c(r1),col="grey")
lines(results3$r2,results3$r12,col="red",lty=2)
lines(results3$r2,results3$r22,col="blue",lty=2)
lines(results3$r2,results3$r02,col="black",lty=2)

# nonconcurrent controls
res=fr1(r1)  
lines(res[2,],res[4,]/res[2,],col="red")
lines(res[2,],res[5,]/res[2,],col="blue")
lines(res[2,],res[3,]/res[2,],col="black")

dev.off()

# 
results4 <- read_csv("results4.csv", col_names = FALSE)
results4=rename(results4,r12=X1,r22=X2)  
results4$r2 <- c(0.01, 0.012, 0.014, 0.016, 0.018, 0.02, 0.022, 0.024, 0.026, 0.028, 
                 0.03, 0.032, 0.034, 0.036, 0.038, 0.04, 0.042, 0.044, 0.046, 0.048, 
                 0.05, 0.052, 0.054, 0.056, 0.058, 0.06, 0.062, 0.064, 0.066, 0.068, 
                 0.07, 0.072, 0.074, 0.076, 0.078, 0.08, 0.082, 0.084, 0.086, 0.088, 
                 0.09, 0.092, 0.094, 0.096, 0.098, 0.1, 0.102, 0.104, 0.106, 0.108, 
                 0.11, 0.112, 0.114, 0.116, 0.118, 0.12, 0.122, 0.124, 0.126, 0.128, 
                 0.13, 0.132, 0.134, 0.136, 0.138, 0.14, 0.142, 0.144, 0.146, 0.148, 
                 0.15, 0.152, 0.154, 0.156, 0.158, 0.16, 0.162, 0.164, 0.166, 0.168, 
                 0.17, 0.172, 0.174, 0.176, 0.178, 0.18, 0.182, 0.184, 0.186, 0.188, 
                 0.19, 0.192, 0.194, 0.196, 0.198, 0.2, 0.202, 0.204, 0.206, 0.208, 
                 0.21, 0.212, 0.214, 0.216, 0.218, 0.22, 0.222, 0.224, 0.226, 0.228, 
                 0.23, 0.232, 0.234, 0.236, 0.238, 0.24, 0.242, 0.244, 0.246, 0.248, 
                 0.25, 0.252, 0.254, 0.256, 0.258, 0.26, 0.262, 0.264, 0.266, 0.268, 
                 0.27, 0.272, 0.274, 0.276, 0.278, 0.28, 0.282, 0.284, 0.286, 0.288, 
                 0.29, 0.292, 0.294, 0.296, 0.298, 0.3, 0.302, 0.304, 0.306, 0.308, 
                 0.31, 0.312, 0.314, 0.316, 0.318, 0.32, 0.322, 0.324, 0.326, 0.328, 
                 0.33, 0.332, 0.334, 0.336, 0.338, 0.34, 0.342, 0.344, 0.346, 0.348, 
                 0.35, 0.352, 0.354, 0.356, 0.358, 0.36, 0.362, 0.364, 0.366, 0.368, 
                 0.37, 0.372, 0.374, 0.376, 0.378, 0.38, 0.382, 0.384, 0.386, 0.388, 
                 0.39, 0.392, 0.394, 0.396, 0.398, 0.4, 0.402, 0.404, 0.406, 0.408, 
                 0.41, 0.412, 0.414, 0.416, 0.418, 0.42, 0.422, 0.424, 0.426, 0.428, 
                 0.43, 0.432, 0.434, 0.436, 0.438, 0.44, 0.442, 0.444, 0.446, 0.448, 
                 0.45, 0.452, 0.454, 0.456, 0.458, 0.46, 0.462, 0.464, 0.466, 0.468, 
                 0.47, 0.472, 0.474, 0.476, 0.478, 0.48, 0.482, 0.484, 0.486, 0.488, 
                 0.49)
# (1:49)/100   
results4$r02 <- 1- results4$r12-results4$r22
head(results4)  
r1=0.49

pdf(paste("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/optimisation/case3_r1_",r1,".pdf",sep=""))
plot(results4$r2,results4$r02,type="l",ylim=c(0,.5),xlim=c(0,1),xlab=TeX("$r_{2}$"),ylab="Allocation Ratios Period 2",lty=2, main=TeX(paste("$r_{1}=$",r1,sep="")))
abline(h=0,col="grey")
abline(h=sqrt(2)/(2+sqrt(2)),col="grey")
abline(v=1-2*c(r1),col="grey")
lines(results4$r2,results4$r12,col="red",lty=2)
lines(results4$r2,results4$r22,col="blue",lty=2)
lines(results4$r2,results4$r02,col="black",lty=2)

# nonconcurrent controls
res=fr1(r1)  
lines(res[2,],res[4,]/res[2,],col="red")
lines(res[2,],res[5,]/res[2,],col="blue")
lines(res[2,],res[3,]/res[2,],col="black")

dev.off()


