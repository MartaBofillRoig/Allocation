
# Case 2 with non-concurrent controls


# Preliminar functions

Power=function(x,y)x^y

fr11 = function(r1){r1/2}

fr22 = function(r1){
  1 - r1 +  sqrt(4*Power(-1 + r1,2) + (-22 + 43*r1 - 21*Power(r1,2))/4. + (-22 + 65*r1 - 64*Power(r1,2) + 21*Power(r1,3))/(12*(-1 + r1)) + (Power(2,0.3333333333333333)*(4 - 40*r1 + 129*Power(r1,2) - 196*Power(r1,3) + 154*Power(r1,4) - 60*Power(r1,5) + 9*Power(r1,6)))/ (3*(-1 + r1)*Power(1024 - 1536*r1 - 26112*Power(r1,2) + 135040*Power(r1,3) - 304896*Power(r1,4) + 391296*Power(r1,5) - 303104*Power(r1,6) + 139392*Power(r1,7) - 34560*Power(r1,8) + 3456*Power(r1,9) +  sqrt(28311552*r1 - 467140608*Power(r1,2) + 3588489216*Power(r1,3) - 16978083840*Power(r1,4) + 55228760064*Power(r1,5) - 130682585088*Power(r1,6) + 232168882176*Power(r1,7) - 315144732672*Power(r1,8) + 329334128640*Power(r1,9) - 264790867968*Power(r1,10) + 162331361280*Power(r1,11) - 74439917568*Power(r1,12) + 24680595456*Power(r1,13) - 5573836800*Power(r1,14) + 764411904*Power(r1,15) - 47775744*Power(r1,16)),0.3333333333333333)) + Power(1024 - 1536*r1 - 26112*Power(r1,2) + 135040*Power(r1,3) - 304896*Power(r1,4) + 391296*Power(r1,5) - 303104*Power(r1,6) + 139392*Power(r1,7) - 34560*Power(r1,8) +    3456*Power(r1,9) +  sqrt(28311552*r1 - 467140608*Power(r1,2) + 3588489216*Power(r1,3) - 16978083840*Power(r1,4) + 55228760064*Power(r1,5) - 130682585088*Power(r1,6) + 232168882176*Power(r1,7) - 315144732672*Power(r1,8) + 329334128640*Power(r1,9) - 264790867968*Power(r1,10) + 162331361280*Power(r1,11) - 74439917568*Power(r1,12) + 24680595456*Power(r1,13) - 5573836800*Power(r1,14) + 764411904*Power(r1,15) - 47775744*Power(r1,16)),0.3333333333333333)/(48*Power(2,0.3333333333333333)*(-1 + r1)))/2. - 
     sqrt(8*Power(-1 + r1,2) + (-22 + 43*r1 - 21*Power(r1,2))/4. - (-22 + 65*r1 - 64*Power(r1,2) + 21*Power(r1,3))/(12*(-1 + r1)) - (Power(2,0.3333333333333333)*(4 - 40*r1 + 129*Power(r1,2) - 196*Power(r1,3) + 154*Power(r1,4) - 60*Power(r1,5) + 9*Power(r1,6)))/(3*(-1 + r1)*Power(1024 - 1536*r1 - 26112*Power(r1,2) + 135040*Power(r1,3) - 304896*Power(r1,4) + 391296*Power(r1,5) - 303104*Power(r1,6) + 139392*Power(r1,7) - 34560*Power(r1,8) + 3456*Power(r1,9) +  sqrt(28311552*r1 - 467140608*Power(r1,2) + 3588489216*Power(r1,3) - 16978083840*Power(r1,4) + 55228760064*Power(r1,5) -    130682585088*Power(r1,6) + 232168882176*Power(r1,7) - 315144732672*Power(r1,8) + 329334128640*Power(r1,9) - 264790867968*Power(r1,10) + 162331361280*Power(r1,11) -  74439917568*Power(r1,12) + 24680595456*Power(r1,13) - 5573836800*Power(r1,14) + 764411904*Power(r1,15) - 47775744*Power(r1,16)),0.3333333333333333)) -  Power(1024 - 1536*r1 - 26112*Power(r1,2) + 135040*Power(r1,3) - 304896*Power(r1,4) + 391296*Power(r1,5) - 303104*Power(r1,6) + 139392*Power(r1,7) - 34560*Power(r1,8) +     3456*Power(r1,9) +  sqrt(28311552*r1 - 467140608*Power(r1,2) + 3588489216*Power(r1,3) - 16978083840*Power(r1,4) + 55228760064*Power(r1,5) - 130682585088*Power(r1,6) +  232168882176*Power(r1,7) - 315144732672*Power(r1,8) + 329334128640*Power(r1,9) - 264790867968*Power(r1,10) + 162331361280*Power(r1,11) - 74439917568*Power(r1,12) +  24680595456*Power(r1,13) - 5573836800*Power(r1,14) + 764411904*Power(r1,15) - 47775744*Power(r1,16)),0.3333333333333333)/(48*Power(2,0.3333333333333333)*(-1 + r1)) +   (-64*Power(-1 + r1,3) + 4*(-1 + r1)*(22 - 43*r1 + 21*Power(r1,2)) - 6*(-4 + 11*r1 - 10*Power(r1,2) + 3*Power(r1,3)))/  (4* sqrt(4*Power(-1 + r1,2) + (-22 + 43*r1 - 21*Power(r1,2))/4. + (-22 + 65*r1 - 64*Power(r1,2) + 21*Power(r1,3))/(12*(-1 + r1)) +  (Power(2,0.3333333333333333)*(4 - 40*r1 + 129*Power(r1,2) - 196*Power(r1,3) + 154*Power(r1,4) - 60*Power(r1,5) + 9*Power(r1,6)))/  (3*(-1 + r1)*Power(1024 - 1536*r1 - 26112*Power(r1,2) + 135040*Power(r1,3) - 304896*Power(r1,4) + 391296*Power(r1,5) - 303104*Power(r1,6) + 139392*Power(r1,7) -   34560*Power(r1,8) + 3456*Power(r1,9) +  sqrt(28311552*r1 - 467140608*Power(r1,2) + 3588489216*Power(r1,3) - 16978083840*Power(r1,4) + 55228760064*Power(r1,5) -      130682585088*Power(r1,6) + 232168882176*Power(r1,7) - 315144732672*Power(r1,8) + 329334128640*Power(r1,9) - 264790867968*Power(r1,10) + 162331361280*Power(r1,11) -  74439917568*Power(r1,12) + 24680595456*Power(r1,13) - 5573836800*Power(r1,14) + 764411904*Power(r1,15) - 47775744*Power(r1,16)),0.3333333333333333)) +  Power(1024 - 1536*r1 - 26112*Power(r1,2) + 135040*Power(r1,3) - 304896*Power(r1,4) + 391296*Power(r1,5) - 303104*Power(r1,6) + 139392*Power(r1,7) - 34560*Power(r1,8) +  3456*Power(r1,9) +  sqrt(28311552*r1 - 467140608*Power(r1,2) + 3588489216*Power(r1,3) - 16978083840*Power(r1,4) + 55228760064*Power(r1,5) - 130682585088*Power(r1,6) +   232168882176*Power(r1,7) - 315144732672*Power(r1,8) + 329334128640*Power(r1,9) - 264790867968*Power(r1,10) + 162331361280*Power(r1,11) - 74439917568*Power(r1,12) +  24680595456*Power(r1,13) - 5573836800*Power(r1,14) + 764411904*Power(r1,15) - 47775744*Power(r1,16)),0.3333333333333333)/(48*Power(2,0.3333333333333333)*(-1 + r1)))))/ 2
}

# 

fr12 = function(r1,r11,r22){
  (r1 - Power(r1,2) -  sqrt(r1)* sqrt(r1 - 2*Power(r1,2) + Power(r1,3) + 4*r1*r11 - 4*Power(r1,2)*r11 - 4*Power(r11,2) + 4*r1*Power(r11,2) - 4*r1*r22 + 4*Power(r1,2)*r22 + 
                                      4*r1*Power(r22,2)))/(2.*r1)
}


# Solutions

f=Vectorize(function(r1,r2) { 
  r22=fr22(r1)
  r12=fr12(r1,r1/2,r22)
  r02=r2-r12-r22
  sol=c(r1,r2,r02,r12,r22)
  sol
})

# 

windows(width = 5, height = 5)
res=f(seq(0,0.5,0.01),1-seq(0,0.5,0.01)) 
# res=fr1(0.49)
plot(res[1,],res[3,]/res[2,],type="l",ylim=c(0,.5),xlim=c(0,.5),xlab=expression(r[1]),ylab="Allocation Ratios Period 2 as fraction of N2",main="Case 2 with NCC",lty=1)
lines(res[1,],res[4,]/res[2,],col="red")
lines(res[1,],res[5,]/res[2,],col="blue")




