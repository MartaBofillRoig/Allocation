
db=sim_designs(r1=30/70,r2=40/70,mu0=0,mu1=1,mu2=1,N=70,alloc="opt", sl=0)
head(db$data)
plot_trial(db$data$treatment) 

mod=lm(response~as.factor(treatment),data=subset(db$data, db$data$period==2)) #model using cc only
summary(mod)
sepmodel_adj_cont(data=db$data,arm=2,alpha=0.025) #model using cc only

vcov(mod)[3,3]
vcov(mod)[2,2]

se = (confint(mod)[3,1]-mod$coefficients[3])/qnorm(0.025)
se^2

((confint(mod)[3,1]-mod$coefficients[3])/qt(0.025,mod$df))^2
((confint(mod)[3,1]-mod$coefficients[3])/qnorm(0.025))^2

