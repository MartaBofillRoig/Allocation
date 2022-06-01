#################################
# Case 3 - optimal allocation paper
Power=function(x,y)x^y

eq=function(r22,r1=0.1,r2=0.8) (r1 + ((Power(r2,2) - 4*r2*r22 + 2*Power(r22,2))*Power(Power(r2,2) - 2*r2*r22 + 2*Power(r22,2),2))/(Power(r2,2)*Power(r2 - r22,3)) - (1-r1-r2))/4.

fr12=function(r22,r1=0.1,r2=0.8) (r2*(r2 - 2*r22))/(2*(r2 - r22))

f=Vectorize(function(r1,r2) { 
  r22=(r1 - Power(r1,2) + 4*r12 - 4*r1*r12 - 4*Power(r12,2))/(r1 + 4*r12)
  # r22=(r1*r2 + 2*r2*(r2 + sqrt(r1*r2 + Power(r2,2))) - Power(r2 + sqrt(r1*r2 + Power(r2,2)),2))/
    # (r1 + 2*(r2 + sqrt(r1*r2 + Power(r2,2))))
  # r12=(r2 + sqrt(r1*r2 + Power(r2,2)))/2
  r02=r2-r12-r22
  sol=c(r1,r2,r02,r12,r22)
  sol
})




fr1=function(r1,steps=100) f(r1,seq(.5-r1,1-r1,length.out=steps)) 
res=fr1(0.49)
plot(res[2,],res[3,]/res[2,],type="l",ylim=c(0,.5),xlim=c(0,1),xlab=expression(r[2]),ylab="Allocation Ratios Period 2 as fraction of N2",main="Case 3",sub="(r1=0,0.2,0.4,0.49)",lty=1)
lines(res[2,],res[4,]/res[2,],col="red")
lines(res[2,],res[5,]/res[2,],col="blue")


res=fr1(0.4)
lines(res[2,],res[3,]/res[2,],type="l",lty=2)
lines(res[2,],res[4,]/res[2,],col="red",lty=2)
lines(res[2,],res[5,]/res[2,],col="blue",lty=2)
abline(h=0,col="grey")


res=fr1(0.2)
lines(res[2,],res[3,]/res[2,],type="l",lty=3)
lines(res[2,],res[4,]/res[2,],col="red",lty=3)
lines(res[2,],res[5,]/res[2,],col="blue",lty=3)
abline(h=0,col="grey")


res=fr1(0)
lines(res[2,],res[3,]/res[2,],type="l",lty=4)
lines(res[2,],res[4,]/res[2,],col="red",lty=4)
lines(res[2,],res[5,]/res[2,],col="blue",lty=4)
abline(h=0,col="grey")
abline(h=sqrt(2)/(2+sqrt(2)),col="grey")
abline(v=1-2*c(0,0.2,0.4,0.49),col="grey")



setwd("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/CEB-allocation/ncc")
# pdf file
plotf=function(r1)
{
  res=fr1(r1)
  pdf(paste("C:/Users/mbofi/Dropbox/CeMSIIS/GitHub/Allocation/CEB-allocation/ncc/case3_r1_",r1,".pdf",sep=""))
  plot(res[2,],res[3,]/res[2,],type="l",ylim=c(0,.5),xlim=c(0,1),xlab=TeX("$r_{2}$"),ylab="Allocation Ratios Period 2",lty=1,
       main=TeX(paste("$r_{1}=$",r1,sep="")))
  abline(h=0,col="grey")
  abline(h=sqrt(2)/(2+sqrt(2)),col="grey")
  abline(v=1-2*c(r1),col="grey")
  lines(res[2,],res[4,]/res[2,],col="red")
  lines(res[2,],res[5,]/res[2,],col="blue")
  lines(res[2,],res[3,]/res[2,],col="black")
  dev.off()
}

plotf(0)
plotf(0.1)
plotf(0.4)
plotf(0.49)
plotf(0.3)

##########################################################################################################################################
library(tidyverse)
library(ggeasy)
library(latex2exp)


plot_area <- function(r1){
  res <- fr1(r1)
  data_area <- data.frame(r2 = res[2,],
                          rC_2 = res[3,]/res[2,],
                          rA_2 = res[4,]/res[2,],
                          rB_2 = res[5,]/res[2,])
  
  data_area <- data_area %>%
    pivot_longer(cols = c(2:4), names_to = "ratio")
  
  data_area$ratio <- factor(data_area$ratio, levels = c("rB_2", "rA_2", "rC_2"))
  
  ggplot(data_area) +
    geom_area(aes(r2, value, fill=ratio), stat="identity", color = "black", alpha = 0.7) +
    scale_fill_manual(values = c("darkolivegreen3", "darkred", "grey"), labels = c(TeX("$r_{B,2}$"), TeX("$r_{A,2}$"), TeX("$r_{C,2}$")), name="") +
    labs(x = TeX("$N_2/N$"), y = "Allocation Ratios Period 2", title = TeX(paste0("$N_{1}/N=$", r1))) +
    geom_vline(xintercept = 1-2*r1) +
    geom_hline(yintercept = sqrt(2)/(2+sqrt(2))) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    theme_classic() +
    easy_center_title()
  
  ggsave(paste0("case3_area_r1_", r1, ".pdf"), width = 8, height = 7)
}

plot_area(0)
plot_area(0.1)
plot_area(0.3)  
plot_area(0.4)
plot_area(0.49)
  
  
  
  
  
  













