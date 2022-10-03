library(latex2exp)
library(tidyverse)
setwd("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/optimisation")

results1 <- read_csv("results1.csv", col_names = FALSE)
results1=rename(results1,r12=X1,r22=X2)  
results1$r2 <- (40:90)/100   

results1$r02 <- 1- results1$r12-results1$r22
head(results1)  
r1=0.1

pdf(paste("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/optimisation/case3ncc_r1_",r1,".pdf",sep=""))
plot(results1$r2,results1$r02,type="l",ylim=c(0,.5),xlim=c(0,1),xlab=TeX("$r_{2}$"),ylab="Allocation Ratios Period 2",lty=1, main=TeX(paste("$r_{1}=$",r1,sep="")))
abline(h=0,col="grey")
abline(h=sqrt(2)/(2+sqrt(2)),col="grey")
abline(v=1-2*c(r1),col="grey")
lines(results1$r2,results1$r12,col="red")
lines(results1$r2,results1$r22,col="blue")
lines(results1$r2,results1$r02,col="black")
dev.off()

# 

results2 <- read_csv("results2.csv", col_names = FALSE)
results2=rename(results2,r12=X1,r22=X2)  
results2$r2 <- (20:70)/100  
results2$r02 <- 1- results2$r12-results2$r22
head(results2)  
r1=0.3

pdf(paste("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/optimisation/case3ncc_r1_",r1,".pdf",sep=""))
plot(results2$r2,results2$r02,type="l",ylim=c(0,.5),xlim=c(0,1),xlab=TeX("$r_{2}$"),ylab="Allocation Ratios Period 2",lty=1, main=TeX(paste("$r_{1}=$",r1,sep="")))
abline(h=0,col="grey")
abline(h=sqrt(2)/(2+sqrt(2)),col="grey")
abline(v=1-2*c(r1),col="grey")
lines(results2$r2,results2$r12,col="red")
lines(results2$r2,results2$r22,col="blue")
lines(results2$r2,results2$r02,col="black")
dev.off()

# 

results3 <- read_csv("results3.csv", col_names = FALSE)
results3=rename(results3,r12=X1,r22=X2)  
results3$r2 <- (10:60)/100  
results3$r02 <- 1- results3$r12-results3$r22
head(results3)  
r1=0.4

pdf(paste("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/optimisation/case3ncc_r1_",r1,".pdf",sep=""))
plot(results3$r2,results3$r02,type="l",ylim=c(0,.5),xlim=c(0,1),xlab=TeX("$r_{2}$"),ylab="Allocation Ratios Period 2",lty=1, main=TeX(paste("$r_{1}=$",r1,sep="")))
abline(h=0,col="grey")
abline(h=sqrt(2)/(2+sqrt(2)),col="grey")
abline(v=1-2*c(r1),col="grey")
lines(results3$r2,results3$r12,col="red")
lines(results3$r2,results3$r22,col="blue")
lines(results3$r2,results3$r02,col="black")
dev.off()

# 
results4 <- read_csv("results4.csv", col_names = FALSE)
results4=rename(results4,r12=X1,r22=X2)  
results4$r2 <- (1:49)/100  
results4$r02 <- 1- results4$r12-results4$r22
head(results4)  
r1=0.49

pdf(paste("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/optimisation/case3ncc_r1_",r1,".pdf",sep=""))
plot(results4$r2,results4$r02,type="l",ylim=c(0,.5),xlim=c(0,1),xlab=TeX("$r_{2}$"),ylab="Allocation Ratios Period 2",lty=1, main=TeX(paste("$r_{1}=$",r1,sep="")))
abline(h=0,col="grey")
abline(h=sqrt(2)/(2+sqrt(2)),col="grey")
abline(v=1-2*c(r1),col="grey")
lines(results4$r2,results4$r12,col="red")
lines(results4$r2,results4$r22,col="blue")
lines(results4$r2,results4$r02,col="black")
dev.off()


