rm(list=ls())

#Simulation from a standard Normal r.v.: setting
n <- 1
alpha <- 0.05
N_MC <- 10000

#C index formulation
C.index <- function(p, alpha) {
  if (p < alpha) {
    C <- 1-(qnorm(1-alpha)/qnorm(1-p))^2
  } else if (p >= alpha && p < 0.5) {
    C <- 1-(qnorm(1-p)/qnorm(1-alpha))^2
  } else {
    C <- 1
  }
  return(C)
}

C.index(0.49,0.05)

#Simulations
set.seed(1234)
res.1 = NULL
for(i in 1: N_MC){
  res.1$t[i]=rnorm(n, mean=4, sd=1)
  res.1$pv[i]=pnorm(-(res.1$t[i]), mean=0, sd=1)
  res.1$C[i]=C.index(res.1$pv[i],alpha)
}
hist(res.1$pv)

#Results extraction
res.1$signif <- ifelse((res.1$pv <= 0.05), 1, 0)
res.1$C_big <- ifelse(res.1$C > 0.50, 1, 0)

sum(ifelse((res.1$signif ==0)  & (res.1$C_big==1), 1, 0))
sum(ifelse((res.1$signif ==0)  & (res.1$C_big==0), 1, 0))
sum(ifelse((res.1$signif ==1)  & (res.1$C_big==1), 1, 0))
sum(ifelse((res.1$signif ==1)  & (res.1$C_big==0), 1, 0))

res.1.data<-as.data.frame(res.1)
View(res.1.data)
