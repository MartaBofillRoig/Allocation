# Optimal allocation ratios in platform trials
# Case 3-B

Power = function(x,y){x^y}

#  Eq solutions for period 1

# Inputs v and v11

v12_eq = function(v,v11){
  
  v12 = v*v11/(v11-v) 
  
  return(v12)

}

C_eq = function(v12,N,sigma=1){
  
  C=v12*N/(sigma^2)
  
  return(C) 
  
}

#  Eq solutions for period 2

solr12_eq = function(C,r2){
 
  solr12 = (Power(2*Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 6*sqrt(3)*sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))),1/3)/C + r2*(2 - (Power(2,2/3)*C*r2)/Power(Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 3*sqrt(3)*sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))),1/3)))/6.
 
 return(solr12)  

}

# 

solr02_eq = function(C,r2){
 
  solr02 = (Power(2*Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 6*sqrt(3)*sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))),1/3)/C + r2*(2 - (Power(2,2/3)*C*r2)/ Power(Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 3*sqrt(3)*sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))),1/3)))/(6.*(-1 + Power(2*Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 6*sqrt(3)*sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))), 1/3)/6. + (C*r2*(2 - (Power(2,2/3)*C*r2)/ Power(Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 3*sqrt(3)*sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))),1/3)))/6.))
 
 return(solr02)  

}

# 

solr22_eq = function(C,r2){
 solr22 = r2 - (Power(2*Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 6*sqrt(3)*sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))),1/3)/C + r2*(2 - (Power(2,2/3)*C*r2)/ Power(Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 3*sqrt(3)*sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))), 1/3)))/ (6.*(-1 + Power(2*Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 6*sqrt(3)*sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))), 1/3)/6. + (C*r2*(2 - (Power(2,2/3)*C*r2)/Power(Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 3*sqrt(3)*sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))), 1/3)))/6.)) + (-(Power(2*Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 6*sqrt(3)*sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))), 1/3)/C) + r2*(-2 + (Power(2,2/3)*C*r2)/ Power(Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 3*sqrt(3)*sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))), 1/3)))/6.
 
 solr22
}


# Wrapper function
f=Vectorize(function(C,r2){ 
  
  # v12=v12_eq(v,v11)
  # C= C_eq(v12,N,sigma=1)
  # r22=solr22_eq(C,r2)
  
  # r02=solr02_eq(C,r2)
  r12=solr12_eq(C,r2)
  
  r02 = r12/(-1+C*r12)
  
  r22=r2 - r02 - r12
  
  sol=c(r02,r12,r22)
  
  return(sol)
})

v12_eq(v=1,v11=11)
solr12_eq(C=0.5,r2=0.4)
f(C=0.5,r2=0.4)
f(C=c(1:10),r2=0.4) 



plot(solr12_eq(C=1:100/10,r2=0.4),C=1:10/10)


# Wrapper function
# f2=Vectorize(function(C,r2,N=1){
# 
#   r12=solr12_eq(C,r2)
#   r02=solr02_eq(C,r2)
#   # r22=solr22_eq(C,r2)
#   r22=r2 - r02 - r12
# 
#   sol=c(r2,r02,r12,r22)
# 
#   return(sol)
# })
# f2(C=c(1:10),r2=0.5)

