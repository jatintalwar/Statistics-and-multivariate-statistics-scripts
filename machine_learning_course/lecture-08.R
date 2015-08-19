### lecture 8

rm(list=ls())

load('08-linear_reg_residuals.rda')

par(mfrow=c(1,1))
plot(y~x1)
plot(y~x2)
plot(y~x3)

new_model <- lm(y~x1+x2+x3)
new_model

coef(new_model)
residuals(new_model)

yhat <-predict(new_model)
plot(y~yhat)

summary(new_model)
qqnorm(residuals(new_model))

plot(residuals(new_model)~x2)

###new model ! :
new_model_2 <- lm(y~x1+sin(x2)+x3)
plot(residuals(new_model_2)~x2)

##new model :

new_model_3 <- lm(y~x1+sin(x2)+x3+x2)
plot(residuals(new_model_3)~x2)

qqnorm(residuals(new_model_3))

### question 2.2

load('08-gene_reg_network.rda')
plot(X[,1]~X[,2])
View(X)

X1 <- X[,1]
X2 <- X[,2]
X3 <- X[,3]

plot(y~X1)
plot(Y~X2)
plot(y~X3)

first_model <- lm(y~X1)
plot(residuals(first_model)~X1)
qqnorm(residuals(first_model))

second_model <- lm(y~X2)

qqnorm(residuals(second_model))
plot(residuals(second_model)~X2)

third_model <- lm(y~X3)
qqnorm(residuals(third_model))
plot(residuals(third_model)~X3)


combined_model <- lm(y~X1+X2+X3)

plot(residuals(combined_model)~X1)
plot(residuals(combined_model)~X2)
plot(residuals(combined_model)~X3)
summary(combined_model)

qqnorm(residuals(combined_model))

combined_model_X3 <- lm(y~X1+X2)
plot(residuals(combined_model_X3)~X1)
plot(residuals(combined_model_X3)~X2)
plot(residuals(combined_model_X3)~X3)

### use causal networks !


#the real model would be :

final_model <- lm(y~X1+X3)

vcov(final_model)
covMatrix <- matrix(c(9.723427e-06 ,4.118650e-08,-2.605904e-08,4.118650e-08,1.072568e-05,-9.299632e-08,-2.605904e-08,-9.299632e-08,9.447251e-06),nrow=3,ncol=3,byrow=TRUE)
covMatrix
cov2cor(covMatrix)
