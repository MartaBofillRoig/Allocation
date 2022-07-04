#################################
# assume the samples sizes in the periods n1, n2, n3 are given. balanced sample sizes in period 1 and 3.


# Case 2
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



windows(width = 5, height = 5)
res=f(seq(0,0.5,0.01),1-seq(0,0.5,0.01)) 
# res=fr1(0.49)
plot(res[1,],res[3,]/res[2,],type="l",ylim=c(0,.5),xlim=c(0,.5),xlab=expression(r[1]),ylab="Allocation Ratios Period 2 as fraction of N2",main="Case 2 without NCC",lty=1)
lines(res[1,],res[4,]/res[2,],col="red")
lines(res[1,],res[5,]/res[2,],col="blue")

