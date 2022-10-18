##########################################
N = 1000
N1 = round(N/3)
N2 = round(2*(N-N1)/3)
c(N1,N2,N-N1-N2)

# design 3: three-period design (non-symmetric design)
db3_one=sim_designs(r1=N1/N,r2=N2/N,mu0=0,mu1=0,mu2=0,N=N,alloc="one")
db3_sqrt=sim_designs(r1=N1/N,r2=N2/N,mu0=0,mu1=0,mu2=0,N=N,alloc="sqrt")
db3_opt=sim_designs(r1=N1/N,r2=N2/N,mu0=0,mu1=0,mu2=0,N=N,alloc="opt")

db3_one_ss <- data.frame(arms=c("A1","A2","C"),db3_one$ss, c(sum(db3_one$ss[1,]),sum(db3_one$ss[2,]),sum(db3_one$ss[3,])))
db3_sqrt_ss <- data.frame(arms=c("A1","A2","C"), db3_sqrt$ss, c(sum(db3_sqrt$ss[1,]),sum(db3_sqrt$ss[2,]),sum(db3_sqrt$ss[3,])))
db3_opt_ss <- data.frame(arms=c("A1","A2","C"), db3_opt$ss, c(sum(db3_opt$ss[1,]),sum(db3_opt$ss[2,]),sum(db3_opt$ss[3,])))

knitr::kable(db3_one_ss, format = "markdown", caption = c("Sample sizes per period and arm (1:1)"), col.names = c("Arms", "Period 1","Period 2","Period 3", "Total per arm"))
knitr::kable(db3_sqrt_ss, format = "markdown", caption = c("Sample sizes per period and arm (sqrt(k)-rule)"), col.names = c("Arms","Period 1","Period 2","Period 3", "Total per arm"))
knitr::kable(db3_opt_ss, format = "markdown", caption = c("Sample sizes per period and arm (optimal allocations)"), col.names = c("Arms","Period 1","Period 2","Period 3", "Total per arm"))


res3_one = do.call(rbind.data.frame, models_cc(data = db3_one$data) )
res3_one$width_ci = res3_one$upper_ci  - res3_one$lower_ci 
knitr::kable(res3_one, format = "markdown")  

res3_sqrt = do.call(rbind.data.frame, models_cc(data = db3_sqrt$data) )
res3_sqrt$width_ci = res3_sqrt$upper_ci  - res3_sqrt$lower_ci 
knitr::kable(res3_sqrt, format = "markdown") 

res3_opt = do.call(rbind.data.frame, models_cc(data = db3_opt$data) )
res3_opt$width_ci = res3_opt$upper_ci  - res3_opt$lower_ci 
knitr::kable(res3_opt, format = "markdown")
