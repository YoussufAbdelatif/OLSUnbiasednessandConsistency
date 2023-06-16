getwd()


install.packages("scatterplot3d")
library(scatterplot3d)
library(ggplot2)
#Unbiasedness

b0=c()
b1=c()
b2=c()
b3=c()

for (i in 1:1000) {

  x_1=rnorm(1000,mean = 5,sd = 5)
  x_2=runif(1000)
  x_3=rbinom(size = 10,n=1000,prob=0.3)
  
  u=rnorm(1000)
  y=5+2*x_1+3*x_2-6*x_3+u
  
  sum=lm(y~x_1+x_2+x_3)

  b0[i]=sum$coefficients[1]
  b1[i]=sum$coefficients[2]
  b2[i]=sum$coefficients[3]
  b3[i]=sum$coefficients[4]
  
  if (i %% 500 == 0) {
    dev.new()
    scatterplot3d(x_1, x_2, x_3, color = rainbow(1000), pch = 16,
                  main = "3D Plot of x_1, x_2, x_3", xlab = "x_1", ylab = "x_2", zlab = "x_3")
  }
}
mean(b0)
mean(b1)
mean(b2)
mean(b3)

#Consistency

b0=c()
b1=c()
b2=c()
b3=c()

for (i in 1:1000) {
  
  x_1=rnorm(i*100,mean = 5,sd = 5)
  x_2=runif(i*100)
  x_3=rbinom(size = 10,n=i*100,prob=0.3)
  
  u=rnorm(i*100)
  y=5+2*x_1+3*x_2-6*x_3+u
  
  sum=lm(y~x_1+x_2+x_3)
  
  b0[i]=sum$coefficients[1]
  b1[i]=sum$coefficients[2]
  b2[i]=sum$coefficients[3]
  b3[i]=sum$coefficients[4]
  
  if (i %% 500 == 0) {
    dev.new()
    scatterplot3d(x_1, x_2, x_3, color = rainbow(100*i), pch = 16,
                  main = "3D Plot of x_1, x_2, x_3", xlab = "x_1", ylab = "x_2", zlab = "x_3")
  }
}
mean(b0)
plot(b0,type="l")
mean(b1)
mean(b2)
mean(b3)