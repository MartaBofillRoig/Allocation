#################################
# assume the samples sizes in the periods n1, n2, n3 are given. balanced sample sizes in period 1 and 3.


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




fr1=function(r1,steps=100) f(r1,seq(.5-r1,1-r1,length.out=steps)) 
res=fr1(0.49)
plot(res[2,],res[3,],type="l",ylim=c(0,.5),xlim=c(0,1),xlab=expression(r[2]),ylab="Allocation Ratios Period 2 as fraction of N",main="Case 3",sub="(r1=0,0.2,0.4,0.49)",lty=1)
lines(res[2,],res[4,],col="red")
lines(res[2,],res[5,],col="blue")

f(1/3,1/3)

res=fr1(0.4)
lines(res[2,],res[3,],type="l",lty=2)
lines(res[2,],res[4,],col="red",lty=2)
lines(res[2,],res[5,],col="blue",lty=2)
abline(h=0,col="grey")


res=fr1(0.2)
lines(res[2,],res[3,],type="l",lty=3)
lines(res[2,],res[4,],col="red",lty=3)
lines(res[2,],res[5,],col="blue",lty=3)
abline(h=0,col="grey")


res=fr1(0)
lines(res[2,],res[3,],type="l",lty=4)
lines(res[2,],res[4,],col="red",lty=4)
lines(res[2,],res[5,],col="blue",lty=4)
abline(h=0,col="grey")

f(0.1,0.8)

library(latex2exp)

plotf=function(r1)
  {
  res=fr1(r1)
  pdf(paste("~/temp/case3_r1_",r1,".pdf",sep=""))
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
