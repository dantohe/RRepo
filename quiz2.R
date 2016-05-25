
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

summary(lm(y~x))

data("mtcars")
plot(mtcars$wt, mtcars$mpg)
abline(lm(mpg~wt, data = mtcars))
help("mtcars")
summary(lm(mpg~wt, data = mtcars))

model = lm(mpg~wt, data = mtcars)
new.data = c(3)
newdata = data.frame(wt=new.data)
predict(model, newdata, interval = "prediction")
        
        
head(mtcars$wt/2)

x=mtcars$wt/2
y=mtcars$mpg
summary(lm(y~x))

x=runif(100, 50,200)
head(x)
y=25+3*x
simple.lm(x,y)
simple.lm(x/100,y)
