

# Numerical examples

n1=50
n2=50

N=150
N1=50

r22= N1/3/N
r12= N1/3/N
r03= N1/2/N

(r22*(-N + N1 - N*r03 + N*r12 + N*r22))/(-N*r03 + N*r22)


# 

NT=150
N1=50
r02=N1/3/N
r22=N1/3/N


(1/2)*(1-N1/NT-r02-r22+(sqrt(N1+NT*(-1+r02+r22))*sqrt(N1^2*r02*(r02+r22)+N1*NT*(r02^3+r02^2*(-1-2*r22)+r02*(-r22+r22^2))))/(sqrt(N1)*NT*sqrt(r02)*sqrt(r02 + r22) ))
