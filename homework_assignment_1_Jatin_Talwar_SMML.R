###   Statistical Modeling and Machine Learning 01 - Math refresher  -solutions:
###   submitted by : Jatin Talwar (Masters Biology LMU Munich)

####################          question 7 : OPTIMIZATION     ###########################

curve(x*exp(-((x-1)^2)/2),from=-2,to=2,n=100,col='red',xlab='x',ylab='f(x)',main='Optimization',lwd=5,box)
abline(b=0,v=(1/2+sqrt(5)/2),col='blue',lwd=2)
abline(b=0,v=(1/2-sqrt(5)/2),col='blue',lwd=2)
legend('topright',inset=0.5,title='local extremas')

#### blue lines represent minima and maxima respectively ######



####################          question 8,9 : Projections in 3D     ###########################

p1=c(-1,-1,0)
p2=c(-1,1,0)
p3=c(1,1,0)
p4=c(1,-1,0)
p5=c(0,0,2)

v=seq(0,1,0.05)                      
e1= v %o% (p2-p1) + rep(1,21) %o% p1 
e3= v %o% (p3-p2) + rep(1,21) %o% p2 
e2= v %o% (p4-p3) + rep(1,21) %o% p3 
e4= v %o% (p1-p4) + rep(1,21) %o% p4
e5= v %o% (p5-p1) + rep(1,21) %o% p1
e6= v %o% (p5-p2) + rep(1,21) %o% p2
e7= v %o% (p5-p3) + rep(1,21) %o% p3
e8= v %o% (p5-p4) + rep(1,21) %o% p4

Pyramid=rbind(e1,e2,e3,e4,e5,e6,e7,e8)
plot(Pyramid)

#### question 8 
#project a matrix in x,y plane with 2x3 dimensions
ProjectionMatrix = matrix(c(1,0,0,1,0,0),nrow =2)
#projection:

projectedMatrix= ProjectionMatrix %*% t(Pyramid)
plot(projectedMatrix[1,],projectedMatrix[2,])   # first clumn for x axis and second for y axis

## or it could also be :
projectedMatrix=t(ProjectionMatrix%*%t(Pyramid))
plot(projectedMatrix)
## to plot the projection: 

## for rotation of matrices :     ############### question 9 :

phi = 0.05*pi #Angle1
theta = 0.3*pi #Angle2

RotationMatrix1 = matrix(c(sin(phi),cos(phi),0,-cos(phi),sin(phi),0,0,0,1),nrow=3)
RotationMatrix2 = matrix(c(sin(theta),0,-cos(theta),0,1,0,cos(theta),0,sin(theta)),nrow=3)

##pyramid rotation 
RotatedPyramid=RotationMatrix1%*%RotationMatrix2%*%t(pyramid)
plot(RotatedPyramid[1,],RotatedPyramid[2,])
