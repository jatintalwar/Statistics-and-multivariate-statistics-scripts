load("df2.rda")

x1=df2$x1
x2=df2$x2
t1=df2$t1
t2=df2$t2
par(mfcol=c(1,2))
plot(x1,x2,col=t1+1,main="t1")
plot(x1,x2,col=t2+1,main="t2")

logreg <- function(t,X,w0, eps=10^-5, maxiter= 200,print.m = TRUE){
  i=0
  dw=Inf
  while(i < maxiter & max(abs(dw)) >= eps){
    w=iterStep(t,X,w0)
    dw=w-w0
    w0=w
    i=i+1
  }
  if(print.m) print(paste("Algorithm converged in",i,"steps"))
  w
}

sig <- function(x){
  1/(1+exp(-x))
}

iterStep <- function(t,X,w){
  a=c(X%*%w)
  y=(sig(a))
  R=diag(y*(1-y))
  invR=solve(R)
  z=a - invR %*% (y-t)
  c(solve( t(X)%*%R%*%X , t(X)%*%R%*%z))
}

###question 8 

X=cbind(1,x1,x2, x1*x2, x1^2, x2^2)
w0 = rep(0, ncol(X))
w1=logreg(t1,X,w0)
## [1] "Algorithm converged in 7 steps"
w1
## [1] -0.8414 -0.1007 0.0734 -0.0743 1.4121 0.9975
w2=logreg(t2,X,w0)
## [1] "Algorithm converged in 6 steps"
w2
## [1] -0.1723 -0.0840 -0.1236 1.8263 0.0815 -0.0314

Xdf=data.frame(X)
names(Xdf)=c("1", "x1","x2", "x1*x2", "x1^2", "x2^2")
plot(Xdf,col=t1+1,cex=0.5)
plot(Xdf,col=t2+1, cex=0.5)
plot(x1^2,x2^2,col=t1+1,main="t1")
plot(x1*x2,x2,col=t2+1,main="t2")
View(Xdf)

######################    cross validation #######################

cv_folds <- function(n,folds=10){
  split(sample(1:n),rep(1:folds,length=n))
}

folds=cv_folds(length(X))

W_hat1= rep(NA,length(X))

for(i in 1:length(folds)){
  fo=folds[[i]]
  params= logreg(t1[-fo],X[-fo],w0)
  W_hat(logreg(t1[-fo],X[-fo],w0))
}

W_hat1 =ifelse(W_hat1>0.5,1,0)
table(W_hat1,w1)








