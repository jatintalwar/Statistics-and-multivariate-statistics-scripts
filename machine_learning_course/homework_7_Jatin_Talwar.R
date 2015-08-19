rm(list=ls())

load("spam30.rda")

sdhat=function(x){
  sqrt((x-mean(x)^2/length(x)))
}

fit_naive_Bayes=function(X,y){
  list(
    mu0=apply(X[y==0,],2,mean),
    mu1=apply(X[y==1,],2,mean),
    sd0=apply(X[y==0,],2,sdhat),
    sd1=apply(X[y==1,],2,sdhat),
    pi0=sum(Y==0)/length(y),
    pi1=1-pi0
  )
}

th=fit_naive_Bayes(X,X[y])  ### fitting the whole dataset

### log probabilities of spam mail :

lp_y1_given_X = function(X,mu0,mu1,sd0,sd1,pi0,pi1){
  lp_X_given_y0=apply(X[y==0,],2,dnorm(X[y==0,], mu0, sd0, log=TRUE))
  lp_X_given_y1=apply(X[y==1,],2,dnorm(X[y==1,], mu0, sd0, log=TRUE))
  pi0=sum(Y==0)/length(y)
  pi1=1-pi0
  rv=lp_X_given_y1*pi1
  rv
}

####### question 5 :::


install.packages('lars')

cv_folds=function(n,folds=10){
  split(sample(1:n), rep(1:folds,length=n))
}

folds=cv_folds(length(y))

p_y1_hat

for i in X[y==1]:
  p_y1_hat= X[-(i)]


#### question 7: Gaussian discriminant analysis

fit_mvn_discrim=function(X,y){
  mu_c=apply(X[y==1,],2,sum(X)/length(X))
  cov=apply(X[y==1],2,sum((X-mu_c)%*%t(X-mu_c))/length(X))
}

fit_mvn_discrim(X,y)

#### question 8 ::

library(mvtnorm)

mu_c=sum(X[y==1,])/length(X)
sigma_c= sum((X[y==1,]-mu_c)*t(X[y==1,]-mu_c))/length(X)

model <- dmvnorm(X[y==1,],mean=mu_c,sigma=sigma_c)

