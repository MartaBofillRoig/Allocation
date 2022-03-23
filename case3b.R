# Optimal allocation ratios in platform trials
# Case 3-B



Power = function(x,y){x^y}

# 

solr12_eq = function(C,r2){
 solr12 = (Power(2*Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 6*Sqrt(3)*Sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))),1/3)/C + r2*(2 - (Power(2,2/3)*C*r2)/Power(Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 3*Sqrt(3)*Sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))),1/3)))/6.
}

# 

solr02_eq = function(C,r2){
 solr02 = (Power(2*Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 6*Sqrt(3)*Sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))),1/3)/C + r2*(2 - (Power(2,2/3)*C*r2)/ Power(Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 3*Sqrt(3)*Sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))),1/3)))/(6.*(-1 + Power(2*Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 6*Sqrt(3)*Sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))), 1/3)/6. + (C*r2*(2 - (Power(2,2/3)*C*r2)/ Power(Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 3*Sqrt(3)*Sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))),1/3)))/6.))
}

# 

solr22_eq = function(C,r2){
 solr22 = r2 - (Power(2*Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 6*Sqrt(3)*Sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))),1/3)/C + r2*(2 - (Power(2,2/3)*C*r2)/ Power(Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 3*Sqrt(3)*Sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))), 1/3)))/ (6.*(-1 + Power(2*Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 6*Sqrt(3)*Sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))), 1/3)/6. + (C*r2*(2 - (Power(2,2/3)*C*r2)/Power(Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 3*Sqrt(3)*Sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))), 1/3)))/6.)) + (-(Power(2*Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 6*Sqrt(3)*Sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))), 1/3)/C) + r2*(-2 + (Power(2,2/3)*C*r2)/ Power(Power(C,2)*Power(r2,2)*(27 - 5*C*r2) + 3*Sqrt(3)*Sqrt(Power(C,4)*Power(r2,4)*(27 + C*r2*(-10 + C*r2))), 1/3)))/6.
}