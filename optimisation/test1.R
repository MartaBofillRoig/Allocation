
Power=function(x,y)x^y 

r1=0.1;r2=0.8;r3=0.1;r22=0.23

eq=function(r12,r22,r1,r2,r3){
  r1/4 + r12 - r22 + ((r1 + 4*r12)*Power(r22,2))/(r1*r2 + 4*r12*(-r12 + r2)) + Power(r12,2)/(-r2 + r22) - r3/4
}

# mroot(eq,lower)
# mroot(eq, lower = 0, upper = 1)

optimize(eq,c(0,r2),r1=r1,r2=r2,r3=r3,r22=r22)



f=Vectorize(function(r1,r2) { 
  r22=uniroot(eq,c(0,r2),r1=r1,r2=r2)$root
  r12=fr12(r22,r1,r2)
  r02=r2-r12-r22
  sol=c(r1,r2,r02,r12,r22)
  sol
})