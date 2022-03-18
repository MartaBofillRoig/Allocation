

fun_r01 <- function(NT,N1,r12){
  r01 = (N1*(1 + sqrt(2 - (NT*(N2 - 5*N2*r12 + NT*r12*(1 + 3*r12)))/(N1*(N2 + NT*r12)))))/(2*NT)
}

fun_r11 <- function(NT,N1,r12){
  r11 = (N1 - N1*sqrt(2 - (NT*(N2 - 5*N2*r12 + NT*r12*(1 + 3*r12)))/(N1*(N2 + NT*r12))))/(2*NT)
}

fun_r02 <- function(NT,N2,r12){
  r02 = (N2 - NT*r12)/(2*NT)  
}

fun_r22 <- function(NT,N2,r12){
  r22 = (N2 - NT*r12)/(2*NT)
}

fun_r03 <- function(N2,r12){
  N1=NT-N2
  r03 = -0.5*(N1 + N2 - NT)/NT
}

fun_r23 <- function(N2,r12){
  N1=NT-N2
  r23 = -0.5*(N1 + N2 - NT)/NT
}