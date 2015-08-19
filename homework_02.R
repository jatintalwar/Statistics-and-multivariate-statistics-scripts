### lecture 14th october 2014 

####    

ols <- read.delim('02_ols.txt')
View(ols)
plot(ols)
x=ols[,1]
y=ols[,2]
X=cbind(1,x)

###    question 10 ::::

w= solve(t(X) %*% X,t(X) %*% y)

yhat=X%*%w
plot(x,y)
points(x,yhat,col='red',pch="x")
abline(w[1],w[2],col='black')


#######    question 11: ###############

tab <- read.delim('02_nonlin_ols.txt')

View(tab)
x=tab[,1]
y=tab[,2]

###design matrix :

sapply(0:9,function(k) x^k)  ##test sapply


################ question 12,13    #####################


dmat <- function(x,k){
  X_0=sapply(0:k,function(i) x^i)
  return(X_0)
}

polyfit <- function(x,y,k){
  X=dmat(x,k)                                  ###fits the data here 
  w=solve(t(X) %*% X,t(X) %*% y)
  return(w)
}

#################    question 14 :    ############

polypredict <- function(x,y,k,xnew){
  Xnew=dmat(xnew,k)
  w=polyfit(x,y,k)
  yhat=Xnew %*% w
  return(yhat)
}


####### question 15,16 :   ################


polyplot <- function(x,y,k){
  x0=seq(min(x),max(x),0.01)
  yhat=polypredict(x,y,k,x0)
  plot(x,y,main=sprintf("k=%d",k),pch='*')
  lines(x0,yhat,col='red',)
}

polyplot(x,y,3)
polyplot(x,y,7)
polyplot(x,y,9)   ### full rank not satisfied here as number of columns are more than the number of rows. 
                  ####  hence we do not have as many data points as variables so we overfit the data !!


### so simulate new data and try again using the k and see what happens 

## solve for overfitting : use cross validation ! cut data in 4 parts ad let the data be train for a bit and 


