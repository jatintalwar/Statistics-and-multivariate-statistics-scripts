########  SMMML :::: assignment 4 ##############

######### submitted by : jatin talwar   ########0


#### question 5:

draw <- function(N){
  y=runif(N,-1,1)
  x=runif(N,-1,1)
  X=ifelse(x^2+y^2 <=1,1,0)
  return (X)
}

muMap <- function(N){
  vec=draw(N)
  sum(vec)/length(vec)
}

Nvec=seq(1,1000,20)
nMap=sapply(Nvec,muMap)
plot(Nvec,nMap)
abline(pi/4,0,col='red')

plotPosterior <- function(N){
  m=sum(draw(N))
  a=m+1
  b=N-m+1
  plot(function(x) dbeta(x,a,b),main='Posterior plot for N=1000')
  abline(v=pi/4, col='red')
}
plotPosterior(100)

errbar(Nvec,nMap,0.85,0.75)


####   question 11    ######


Nvec=10^seq(0,7,0.5)
uNlog=sapply(Nvec,muMap)
plot(abs(uNlog*4-pi)~Nvec, log='xy')

y=log10(abs(uNlog*4-pi))
x=log10(Nvec)
fit=lm(y~x)
plot(x,y)
abline(fit)

print(fit$coef)

###

######    question 18 #####

#### N = 1

N_1=rnorm(1,mean=0,sd=1)
Mean_values_1 <- c()
for (i in 1:5){
  M <- sample(N_1,replace=T)
  Mean_values_1[i] <- mean(M)
}

#### N = 10

N_10 <- rnorm(10,mean=0,sd=1)
Mean_values_10 <- c()
for (i in 1:7){
  M <- sample(N_10,replace=T)
  Mean_values_10[i] <- mean(M)
}

#### N = 100

N_100=rnorm(100,mean=0,sd=1)
Mean_values_100 <- c()
for (i in 1:35){
  M <- sample(N_100,replace=T)
  Mean_values_100[i] <- mean(M)
}

#### N = 1000

N_1000=rnorm(1000,mean=0,sd=1)
Mean_values_1000 <- c()
for (i in 1:350){
  M <- sample(N_1000,replace=T)
  Mean_values_1000[i] <- mean(M)
}

par(mfrow=(c(2,2)))
hist(Mean_values_1,col='red',main='N=1',xlab='Mean')
hist(Mean_values_10,col='red',main='N=10',xlab='Mean')
hist(Mean_values_100,col='red',main='N=100',xlab='Mean')
hist(Mean_values_1000,col='red',main='N=1000',xlab='Mean')

par(mfrow=(c(2,2)))
qqplot(N_1,rnorm(1,mean=0,sd=1),main='QQ plot, N=1',ylab='Theoretical Distribution')
qqplot(N_10,rnorm(10,mean=0,sd=1),main='QQ plot, N=10',ylab='Theoretical Distribution')
qqplot(N_100,rnorm(100,mean=0,sd=1),main='QQ plot, N=100',ylab='Theoretical Distribution')
qqplot(N_1000,rnorm(1000,mean=0,sd=1),main='QQ plot, N=1000',ylab='Theoretical Distribution')

