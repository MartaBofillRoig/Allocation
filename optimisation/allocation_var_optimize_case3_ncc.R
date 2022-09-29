
results1 <- read_csv("results1.csv", col_names = FALSE)

head(results1)
results1$X0 <- c(0.2, 0.21, 0.22, 0.23, 0.24, 0.25, 0.26, 0.27, 0.28, 0.29, 0.3, 0.31, 
0.32, 0.33, 0.34, 0.35, 0.36, 0.37, 0.38, 0.39, 0.4, 0.41, 0.42, 
0.43, 0.44, 0.45, 0.46, 0.47, 0.48, 0.49, 0.5, 0.51, 0.52, 0.53, 
0.54, 0.55, 0.56, 0.57, 0.58, 0.59, 0.6, 0.61, 0.62, 0.63, 0.64, 
0.65, 0.66, 0.67, 0.68, 0.69, 0.7)
  
  

fr1=function(r1,steps=100) f(r1,seq(.5-r1,1-r1,length.out=steps)) 
res=fr1(0.49)

plot(res[2,],res[3,]/res[2,],type="l",ylim=c(0,.5),xlim=c(0,1),xlab=expression(r[2]),ylab="Allocation Ratios Period 2 as fraction of N2",main="Case 3",sub="(r1=0,0.2,0.4,0.49)",lty=1)
lines(res[2,],res[4,]/res[2,],col="red")
lines(res[2,],res[5,]/res[2,],col="blue")



plot(results1$X0,1-results1$X2-results1$X1,type="l",ylim=c(0,.5),xlim=c(0,1),xlab=expression(r[2]),ylab="Allocation Ratios Period 2 as fraction of N2",main="Case 3",sub="(r1=0,0.2,0.4,0.49)",lty=1)
lines(results1$X0,results1$X1,col="red")
lines(results1$X0,results1$X2,col="blue")
