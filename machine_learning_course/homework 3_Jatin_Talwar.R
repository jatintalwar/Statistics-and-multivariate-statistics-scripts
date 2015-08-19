####homework assignment 03 -Jatin Talwar

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

###### complete figures :

par(mfrow=(c(2,2)))
hist(Mean_values_1,col='red',main='N=1',xlab='Mean')
hist(Mean_values_10,col='red',main='N=10',xlab='Mean')
hist(Mean_values_100,col='red',main='N=100',xlab='Mean')
hist(Mean_values_1000,col='red',main='N=1000',xlab='Mean')

par(mfrow=(c(2,2)))
qqnorm(N_1)
qqline(N_1,col='blue')
abline(0,1,col='red')
qqnorm(N_10)
qqline(N_10,col='blue')
abline(0,1,col='red')
qqnorm(N_100)
qqline(N_100,col='blue')
abline(0,1,col='red')
qqnorm(N_1000)
qqline(N_1000,col='blue')
abline(0,1,col='red')

par(mfrow=(c(2,2)))
qqplot(N_1,rnorm(1,mean=0,sd=1),main='QQ plot, N=1',ylab='Theoretical Distribution')
qqplot(N_10,rnorm(10,mean=0,sd=1),main='QQ plot, N=10',ylab='Theoretical Distribution')
qqplot(N_100,rnorm(100,mean=0,sd=1),main='QQ plot, N=100',ylab='Theoretical Distribution')
qqplot(N_1000,rnorm(1000,mean=0,sd=1),main='QQ plot, N=1000',ylab='Theoretical Distribution')


x=seq(from =-2, to=3,by=0.01)
x=rnorm(x,mean=mean(x),sd=sd(x))
y=x^2
plot(y)
qqnorm(y)

#### the graph shows it is a exponential curve
### hence the PDF of y is exponential.
