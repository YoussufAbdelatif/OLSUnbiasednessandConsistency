beta=c()


for (i in 1:10000) {
  #1.Step 
  #Shoe Size = 2*Age
  #The error terms follow a normal distribution
  
  #2.Step
  age=c(15,12,13,14,18)
  Size=2*age+rnorm(5)
  
  #3.Step
  reg=lm(Size~age-1)
  print(reg$coefficients)
  beta[i]=reg$coefficients
}

#4.Step 
#Repeat steps 1-3 R times in this case 10000

#5.Step
#Summary statistics
mean(beta)
hist(beta)
sd(beta)

#Generate non-stationary time series

n=100
x=numeric(n)
for(i in 2:length(x)){
  x[i] <- 1 * x[i - 1] + rnorm(1)
}
new_process_3=x
plot(new_process_3,type = "l")

summary(lm(diff(new_process_3)~ lag(new_process_3)[2:100]-1))

beta=c()
for (i in 1:5000) {
  new_process_3_new=sample(new_process_3,100,replace = T)
  reg=lm(diff(new_process_3_new)~ lag(new_process_3_new)[2:100]-1)
  print(reg$coefficients)
  beta[i]=summary(reg)$coefficients[,3]
}

mean(beta)
hist(beta,freq = F)
sd(beta)
quantile(beta,probs = c(0.975))

################################

beta=c()


for (i in 1:10000) {
  #1.Step 
  #Shoe Size = 2*Age
  #The error terms follow a uniform distribution
  
  #2.Step
  age=c(15,12,13,14,18)
  Size=2*age+runif(5,min = 1,max = 6)
  
  #3.Step
  reg=lm(Size~age-1)
  print(reg$coefficients)
  beta[i]=reg$coefficients
}

#4.Step 
#Repeat steps 1-3 R times in this case 10000

#5.Step
#Summary statistics
mean(beta)
hist(beta)
sd(beta)