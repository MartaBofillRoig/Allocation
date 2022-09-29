fr12=function(r1,r2,r22){
  (32*(3*Power(r2,2) - 6*r2*r22 + 2*Power(r22,2)) + (16i*Power(2,1/3)*(1i + sqrt(3))* (3*Power(r2,4) - 12*Power(r2,3)*r22 + 18*Power(r2,2)*Power(r22,2) - 12*r2*Power(r22,3) + 4*Power(r22,4) + 3*r1*r2*(Power(r2,2) - 3*r2*r22 + 2*Power(r22,2))))/Power(-9*r1*Power(r2,3)*Power(r22,2) + 18*Power(r2,4)*Power(r22,2) + 27*r1*Power(r2,2)*Power(r22,3) - 72*Power(r2,3)*Power(r22,3) - 18*r1*r2*Power(r22,4) + 108*Power(r2,2)*Power(r22,4) - 72*r2*Power(r22,5) + 16*Power(r22,6) + sqrt(-4*Power(3*Power(r2,4) - 12*Power(r2,3)*r22 + 18*Power(r2,2)*Power(r22,2) - 12*r2*Power(r22,3) + 4*Power(r22,4) + 3*r1*r2*(Power(r2,2) - 3*r2*r22 + 2*Power(r22,2)),3) +  Power(r22,4)*Power(9*r1*r2*(Power(r2,2) - 3*r2*r22 + 2*Power(r22,2)) - 2*(9*Power(r2,4) - 36*Power(r2,3)*r22 + 54*Power(r2,2)*Power(r22,2) - 36*r2*Power(r22,3) + 8*Power(r22,4)),2)),1/3) - 8*Power(2,2/3)*(1 + 1i*sqrt(3))*Power(-9*r1*Power(r2,3)*Power(r22,2) + 18*Power(r2,4)*Power(r22,2) + 27*r1*Power(r2,2)*Power(r22,3) -  72*Power(r2,3)*Power(r22,3) - 18*r1*r2*Power(r22,4) + 108*Power(r2,2)*Power(r22,4) - 72*r2*Power(r22,5) + 16*Power(r22,6) +  sqrt(-4*Power(3*Power(r2,4) - 12*Power(r2,3)*r22 + 18*Power(r2,2)*Power(r22,2) - 12*r2*Power(r22,3) + 4*Power(r22,4) + 3*r1*r2*(Power(r2,2) - 3*r2*r22 + 2*Power(r22,2)),3) + Power(r22,4)*Power(9*r1*r2*(Power(r2,2) - 3*r2*r22 + 2*Power(r22,2)) - 2*(9*Power(r2,4) - 36*Power(r2,3)*r22 + 54*Power(r2,2)*Power(r22,2) - 36*r2*Power(r22,3) + 8*Power(r22,4)),2)),1/3))/(192.*(r2 - r22))
}

eq=function(r22,r1,r2,r12){
  (r1/4 + r12 + Power(r12,2)/(-r2 + r22))-(r22 - ((r1 + 4*r12)*Power(r22,2))/(r1*r2 + 4*r12*(-r12 + r2)) + r3/4)
  # term1=r1/4 + r12 + Power(r12,2)/(-r2 + r22)
  # term2=r22 - ((r1 + 4*r12)*Power(r22,2))/(r1*r2 + 4*r12*(-r12 + r2)) + r3/4
}

