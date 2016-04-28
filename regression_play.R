
library(UsingR); data(galton); library(reshape); long <- melt(galton)

manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))
g <- ggplot(long, aes(x = value, fill = variable))
g <- g + geom_histogram(colour = "black", binwidth=1)
g <- g + facet_grid(. ~ variable)
g

library(manipulate)
myHist <- function(mu){
        mse <- mean((galton$child - mu)^2)
        g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = "salmon", colour= "black", binwidth=1)
        g <- g + geom_vline(xintercept = mu, size = 3)
        g <- g + ggtitle(paste("mu = ", mu, ", MSE = ", round(mse, 2), sep = ""))
        g
}

g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = "salmon", colour = "black", binwidth=1)
g <- g + geom_vline(xintercept = mean(galton$child), size = 3)
g

ggplot(galton, aes(x = parent, y = child)) + geom_point()

y <- galton$child - mean(galton$child)
x <- galton$parent - mean(galton$parent)
freqData <- as.data.frame(table(x, y))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
myPlot <- function(beta){
        g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
        g <- g + scale_size(range = c(2, 20), guide = "none" )
        g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
        g <- g + geom_point(aes(colour=freq, size = freq))
        g <- g + scale_colour_gradient(low = "lightblue", high="white")
        g <- g + geom_abline(intercept = 0, slope = beta, size = 3)
        mse <- mean( (y - beta * x) ^2 )
        g <- g + ggtitle(paste("beta = ", beta, "mse = ", round(mse, 3)))
        g
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))

#linear model
lm(I(child - mean(child))~ I(parent - mean(parent)) - 1, data = galton)

y <- galton$child
x <- galton$parent
beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
rbind(c(beta0, beta1), coef(lm(y ~ x)))

lm(x~y)
coef(lm(x~y))

x <- rnorm(100)
y <- rnorm(100)
odr <- order(x)
x[odr[100]]
x[odr[1]]
x

#
library(UsingR)
data(father.son)
y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)
rho <- cor(x, y)
library(ggplot2)
g = ggplot(data.frame(x, y), aes(x = x, y = y))
g = g + geom_point(size = 5, alpha = .2, colour = "black")
g = g + geom_point(size = 4, alpha = .2, colour = "red")
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)
g = g + geom_abline(position = "identity")
g = g + geom_abline(intercept = 0, slope = rho, size = 2)
g = g + geom_abline(intercept = 0, slope = 1 / rho, size = 2)
g = g + xlab("Father's height, normalized")
g = g + ylab("Son's height, normalized")
g

install.packages("swirl")
packageVersion("swirl")
library(swirl)

varChild <- var(galton$child)



library(UsingR)
data(diamond)
library(ggplot2)
g = ggplot(diamond, aes(x = carat, y = price))
g = g + xlab("Mass (carats)")
g = g + ylab("Price (SIN $)")
g = g + geom_point(size = 7, colour = "black", alpha=0.5)
g = g + geom_point(size = 5, colour = "blue", alpha=0.2)
#regression line
g = g + geom_smooth(method = "lm", colour = "black")
g

fit <- lm(price~carat, data=diamond)
coef(fit1)
fit2 <- lm(price~I(carat-mean(carat)), data=diamond)
coef(fit2)
fit3 <- lm(price~I(carat*10), data=diamond)
coef(fit3)

#3 diamonds 
newx <- c(0.16, 0.27, 0.34)
#estimate the price
#manualy
coef(fit)[1] + coef(fit)[2] * newx

#with a function
predict(fit, newdata = data.frame(carat=newx))

plot(diamond)

x <- diamond$price
y <- diamond$carat

fit <- lm(x~y)
plot(fit)

#getting residuals
e <- resid(fit)
e
yhat <- predict(fit)
yhat
#sum of the residuals should be 0
sum(e)

#plotting in a diffrent way 
plot(diamond$carat, diamond$price, xlab = "Mass - carats -0.2g", ylab = "Price - Singapore $", bg="lightblue", col="black", cex=1.1, pch=21, frame=FALSE)
abline(fit, lwd=2)
for(i in 1:n)
        lines(c(x[i],x[i]), c(y[1],yhat[i]), col="red", lwd=2)


#more residual examples - non linear
x = runif(100,-3,3); y=x+sin(x) +rnorm(100, sd=.2);
library(ggplot2)
g = ggplot(data.frame(x = x,y = y), aes( x=x, y=y))
g = g + geom_smooth( method ="lm", colour= "black")
g = g + geom_point(size=7, colour ="black", alpha=0.4)
g = g + geom_point(size=5, colour ="red", alpha=0.4)
g

#plot the residuals
g = ggplot(data.frame(x = x,y = resid(lm(x~y))), aes( x=x, y=y))
#g = g + geom_smooth( method ="lm", colour= "black")
g = g + geom_hline(yintercept=0, Size=2)
g = g + geom_point(size=7, colour ="black", alpha=0.4)
g = g + geom_point(size=5, colour ="red", alpha=0.4)
g = g + xlab("X") + ylab("Residual")
g





#Finding residual variance estimates
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
summary(fit)$sigma

sqrt(sum(resid(fit)^2) / (n - 2))

#r-squared  - the percentage of the total variability that is explained by the model
data(anscombe)
example(anscombe)

library(ggplot2)
newx = data.frame(x = seq(min(x), max(x), length = 100))
p1 = data.frame(predict(fit, newdata= newx,interval = ("confidence")))
p2 = data.frame(predict(fit, newdata = newx,interval = ("prediction")))
p1$interval = "confidence"
p2$interval = "prediction"
p1$x = newx$x
p2$x = newx$x
dat = rbind(p1, p2)
names(dat)[1] = "y"
g = ggplot(dat, aes(x = x, y = y))
g = g + geom_ribbon(aes(ymin = lwr, ymax = upr, fill = interval), alpha = 0.2)
g = g + geom_line()
g = g + geom_point(data = data.frame(x = x, y=y), aes(x = x, y = y), size = 4)
g

#quiz 4 -q 1
library(MASS)
?shuttle
shuttle$use.binary <- as.integer(shuttle$use == "auto")
fit <- glm(use.binary ~ wind - 1, data = shuttle, family = binomial)

summary(fit)$coef

unname(exp(coef(fit))[1]/exp(coef(fit))[2])

#q2
fit <- glm(use.binary ~ wind + magn - 1, data = shuttle, family = binomial)
exp(coef(fit))
unname(exp(coef(fit))[1]/exp(coef(fit))[2])

#q3
fit <- glm(use.binary ~ wind + magn - 1, data = shuttle, family = binomial)
exp(coef(fit))
unname(exp(coef(fit))[1]/exp(coef(fit))[2])

#q4
fit <- glm(count ~ spray - 1, data = InsectSprays, family = poisson)
coef.exp <- exp(coef(fit))
unname(coef.exp[1] / coef.exp[2])


#q5
x <- seq(1, 1000, by = 1)
t <- log(x)
t2 <- log(10) + t
y <- ppois(x, 2)

fit1 <- glm(y ~ x + offset(t), family = poisson, data = InsectSprays)
fit2 <- glm(y ~ x + offset(t2), family = poisson, data = InsectSprays)

summary(fit1)$coef
summary(fit2)$coef


require(graphics)
pairs(mtcars, main = "mtcars data")
coplot(mpg ~ disp | as.factor(cyl), data = mtcars,
       panel = panel.smooth, rows = 1)
str(mtcars)
head(mtcars)
summary(mtcars)

pairs(mpg ~ ., data=mtcars)

#finding correlations
cor(mtcars)

library(Hmisc)
rcorr(as.matrix(mtcars))

library(corrgram)
corrgram(mtcars, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Car Milage Data in PC2/PC1 Order")

install.packages("caret", dependencies = c("Depends", "Suggests"))
library(caret)
findCorrelation(cor(mtcars))

install.packages("minqa")
install.packages("caret")

initial.model <- lm(mpg ~., data= mtcars)
best.model <- step(initial.model, direction = "both")
summary(best.model)

shapiro.test(mtcars$mpg)

install.packages('ggplot2', dep = TRUE)
library(ggplot2)
g = ggplot(mtcars, aes(factor(am), mpg, fill=factor(am)))
g = g + geom_boxplot()
g = g + geom_jitter(position=position_jitter(width=.1, height=0))
g = g + scale_colour_discrete(name = "Type")
g = g + scale_fill_discrete(name="Type", breaks=c("0", "1"),
                            labels=c("Automatic", "Manual"))
g = g + scale_x_discrete(breaks=c("0", "1"), labels=c("Automatic", "Manual"))
g = g + xlab("")
g


n = 100; x = rnorm(n); x2 = rnorm(n); x3 = rnorm(n)
plot(x)
hist(x)
y = 1 + x + x2 + x3 + rnorm(n, sd = .1)
ey = resid(lm(y ~ x2 + x3))
ex = resid(lm(x ~ x2 + x3))
sum(ey * ex) / sum(ex ^ 2)
coef(lm(ey ~ ex - 1))

coef(lm(y ~ x + x2 + x3))

require(datasets)
data(swiss)
?swiss

summary(lm(Fertility ~ . , data = swiss))
summary(lm(Fertility ~ . , data = swiss))$coeficients
summary(lm(Fertility ~ Agriculture , data = swiss))

n = 100; x2 <- 1 : n; x1 = .01 * x2 + runif(n, -.1, .1); y = -x1 + x2 + rnorm(n, sd = .01)

summary(lm(y ~ x1))$coef
summary(lm(y ~ x1 + x2))$coef

library(ggplot2)

#including an unnecessary variable into the model
z <- swiss$Agriculture + swiss$Education
lm(Fertility ~ . +z, data = swiss)

#insects spray
require(datasets);data(InsectSprays); require(stats); require(ggplot2)
g = ggplot(data = InsectSprays, aes(y = count, x = spray, fill = spray))
g = g + geom_violin(colour = "black", size = 2)
g = g + xlab("Type of spray") + ylab("Insect count")
g

summary(lm(count ~ spray, data = InsectSprays))$coef

spray2 <- relevel(InsectSprays$spray, "C")
summary(lm(count~spray2, InsectSprays))$coef




n =c(1,2,3,4,5,6,7,8,9,10)
mean(n)
median(n)
var(n)
n[c(1,2,8)]
max(n)
n==4

which(n==3)
n[3]=33
n

which()
