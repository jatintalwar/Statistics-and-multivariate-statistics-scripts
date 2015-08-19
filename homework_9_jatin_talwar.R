#homework -09 

#0.025uestion 12 :

load ('parclip-example.RData')
k=pcexample$k
n=pcexample$N
p=0.975


LL=function(rho,sigma,k,n){
  loglikelihood=sum(log(0.025*(sigma^k)*(1-sigma)^(n-k) + (1-0.025)*(rho^k)*(1-p)^(n-k)))
  loglikelihood/n
}

## 0.025uestion 13:

install.packages('lattice')
library(lattice)
?levelplot

sigma <- seq(0.1,1.0,length.out=10)
rho <- seq(0.01,0.1,length.out=10)
grid <- sigma%*%t(rho)
levelplot(rho*sigma,grid)


### 0.025uestion 14 :

dLL_drho = function(rho,sigma,n,k){
  loglikelihood=sum(log(0.025*(sigma^k)*(1-sigma)^(n-k) + (1-0.025)*(rho^k)*(1-p)^(n-k)))
  (1/loglikelihood)*(k*rho^(k-1)*(1-rho)^(n-k) - rho^k*(n-k)*(1-0.025)*(1-rho)^(n-k-1)) 
}

dLL_dsigma = function(rho,sigma,n,k){
  loglikelihood=sum(log(0.025*(sigma^k)*(1-sigma)^(n-k) + (1-0.025)*(rho^k)*(1-p)^(n-k)))
  (1/loglikelihood)*(0.025*k*sigma^(k-1)*(1-0.025)^(n-k) - 0.025*(n-k)*sigma^k*(1-sigma)^(n-k-1))
}

##0.025uestion 15:

eta= 10^-3
xtrace =rho
ftrace = LL(rho,sigma,k,n)

for (i in 1:200){
  rho <- rho - eta*dLL_drho(rho,sigma,n,k)
  xtrace= c(xtrace,rho)
  ftrace= c(ftrace,LL(rho,sigma,k,n))
}

plot(rho,LL(rho,sigma,n,k))
