x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
mean(x)



x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
summary(lm(y~x))
coef(lm(y~x-1))
?lm

data(mtcars) 
help("mtcars")
lm(mtcars$mpg~mtcars$wt)

#The slope of a regression line 
#is the correlation between the two sets of 
#dependent and independent variables 
#multiplied by the ratio of their standard deviations.

#normalization
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
x.normal = (x-mean(x))/sd(x)
x.normal

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

summary(lm(y~x))
coef(lm(y~x))
library(UsingR)
simple.lm(x,y)



x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
plot(x)
mean(x)

#Beta1  = Cor(Y, X) * SD(Y) / SD(X)
#Gamma1 = Cor(X, Y) * SD(X) / SD(Y)
#Beta1/Gamma1 = Cor(Y, X) / Cor(X, Y) * SD(Y)^2 / SD(X)^2
#             =           1           * Var(Y)  / Var(X)
#             = Var(Y) / Var(X)

