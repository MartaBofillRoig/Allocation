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
resultsuneq <- read_csv("resultsuneqvar.csv", col_names = FALSE)
resultsuneq <- rename(resultsuneq, r1=X1, r2=X2, rs10=X3, rs20=X4, r20=X5, r22=X6)
resultsuneq$r12 <- 1- resultsuneq$r20-resultsuneq$r22
head(resultsuneq) 

dim(resultsuneq)[1]- length(which(resultsuneq$r22<0))
dim(resultsuneq[-which(resultsuneq$r22<0),])

results <- resultsuneq[-which(resultsuneq$r22<0),]
summary(results)

#################################
results1 <- subset(results, results$rs10==2.0001 &  results$r1==0.3)
head(results1)
summary(results1) 

pdf(paste("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/optimisation/uneq_case3_rs102_r1_",results1$r1,".pdf",sep=""))
# in dotted lines - optimisation using max variances
# lty=3, lwd=2 

plot(0,type="l",ylim=c(0,.6),xlim=c(0,1),xlab=TeX("$r_{2}$"),ylab="Allocation Ratios Period 2",lty=1, lwd=2, main=TeX(paste("$r_{1}=$",results1$r1, sep=" ", ", $sigma_1 / sigma_0=$", round(results1$rs10))))
abline(h=0,col="grey")
abline(h=sqrt(2)/(2+sqrt(2)),col="grey")
abline(v=1-2*c(results1$r1),col="grey") 
l=length(levels(as.factor(results1$rs20))) 
line_types <- 1:l  # Customize line types as needed
for (i in 1:l) {
  subset_data <- subset(results1, results1$rs20== levels(as.factor(results1$rs20))[i])
  lines(subset_data$r2, subset_data$r20, col="black", lty = line_types[i], lwd=2)
  lines(subset_data$r2, subset_data$r12, col="red", lty = line_types[i], lwd=2)
  lines(subset_data$r2, subset_data$r22, col="blue", lty = line_types[i], lwd=2) 
}
# Add a legend
legend("bottomleft", legend = levels(as.factor(results1$rs20)), lty = line_types, title = TeX("$sigma_2 / sigma_0$"))
dev.off()

#################################

results1 <- subset(results, results$rs10==1.0001 &  results$r1==0.3)
# results1 <- subset(results, results$rs10==1.0001)
head(results1)
summary(results1) 

pdf(paste("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/optimisation/uneq_case3_r1_",results1$r1,".pdf",sep=""))


# Plotting in base R with different types of lines
plot(0,type="l",ylim=c(0,.5),xlim=c(0,1),xlab=TeX("$r_{2}$"),ylab="Allocation Ratios Period 2",lty=1, lwd=2, main=TeX(paste("$r_{1}=$",results1$r1, sep=" ", ", $sigma_1 / sigma_0=$", round(results1$rs10))))
abline(h=0,col="grey")
abline(h=sqrt(2)/(2+sqrt(2)),col="grey")
abline(v=1-2*c(results1$r1),col="grey") 
l=length(levels(as.factor(results1$rs20))) 
line_types <- 1:l   
for (i in 1:l) {
  subset_data <- subset(results1, results1$rs20== levels(as.factor(results1$rs20))[i])
  lines(subset_data$r2, subset_data$r20, col="black", lty = line_types[i], lwd=2)
  lines(subset_data$r2, subset_data$r12, col="red", lty = line_types[i], lwd=2)
  lines(subset_data$r2, subset_data$r22, col="blue", lty = line_types[i], lwd=2) 
}
# Add a legend
legend("bottomleft", legend = levels(as.factor(results1$rs20)), lty = line_types, title = TeX("$sigma_2 / sigma_0$")) 
dev.off()

#################################

results1 <- subset(results, results$rs10==1.0001 &  results$r1==0.1)
# results1 <- subset(results, results$rs10==1.0001)
head(results1)
summary(results1) 

pdf(paste("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/optimisation/uneq_case3_r1_",results1$r1,".pdf",sep=""))


# Plotting in base R with different types of lines
plot(0,type="l",ylim=c(0,.5),xlim=c(0,1),xlab=TeX("$r_{2}$"),ylab="Allocation Ratios Period 2",lty=1, lwd=2, main=TeX(paste("$r_{1}=$",results1$r1, sep=" ", ", $sigma_1 / sigma_0=$", round(results1$rs10))))
abline(h=0,col="grey")
abline(h=sqrt(2)/(2+sqrt(2)),col="grey")
abline(v=1-2*c(results1$r1),col="grey") 
l=length(levels(as.factor(results1$rs20))) 
line_types <- 1:l   
for (i in 1:l) {
  subset_data <- subset(results1, results1$rs20== levels(as.factor(results1$rs20))[i])
  lines(subset_data$r2, subset_data$r20, col="black", lty = line_types[i], lwd=2)
  lines(subset_data$r2, subset_data$r12, col="red", lty = line_types[i], lwd=2)
  lines(subset_data$r2, subset_data$r22, col="blue", lty = line_types[i], lwd=2) 
}
# Add a legend
legend("bottomleft", legend = levels(as.factor(results1$rs20)), lty = line_types, title = TeX("$sigma_2 / sigma_0$")) 
dev.off()
