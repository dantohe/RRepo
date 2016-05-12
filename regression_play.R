
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

which(n==33)
n==33
n[n==33]
n[c(n==33)]
n.b = n==33
n.b
n[n.b]

#logical extraction - extraction by logical vector
n.indeces = 1:length(n)
n.indeces
n.indeces[n==33]
#
(1:length(n))[n==max(n)]
(1:length(n))[n>=mean(n)]

sum(n)
sum(n>10)
n
n1=n
n2=n
n3=n1-n2
n2[3]=0
n3

cummax(n)
cummin(n)

whale = c(74, 122, 235, 111, 292, 111, 211, 133, 156, 79)
mean(whale)
median(whale)
var(whale)

std = function(x) sqrt(var(x))
std(n)
std(whale)


miles = c(65311, 65624, 65908, 66219, 66499, 66821, 67145, 67447)
x = diff(miles)
x

max(miles)
mean(x)
cm = c(17, 16,20, 24, 22, 15, 21, 15, 17, 22)
cm[which((cm==24))]=18
sum(cm>20)
which(cm>=20)
cm_mod = cm
cm.m = cm
cm.m[which(cm.m>=20)] = 1000

#finding percentage of commutes less than 17 minutes
length(which(cm<17))*100/length(cm)

bill = c(46, 33, 39, 37, 46, 30, 48, 32, 49, 35, 30, 48)
x = c(1,3,5,7,9,10)
y = c(2,3,5,7,11,13)
x+y

x = c(1, 8, 2, 6, 3, 8, 5, 5, 5, 5)

x=c("Yes","No","No","Yes","Yes")

table(x)

beer = scan()
#3 4 1 1 3 4 3 3 1 3 2 1 2 1 2 3 2 3 1 1 1 1 4 3 1
barplot(beer)
table(beer)
barplot(table(beer))
barplot(table(beer)/length(beer))

b.c = table(beer)
pie(b.c)
names(b.c) = c("first","second","third", "fourth")
pie(b.c, col = c("purple","red","yellow","green"))

# a distribution is described by center (mean) and spread (variance)

s = scan()
mean(s)
var(s)
sd(s)
median(s)
fivenum(s)
summary(s)
n = c(1,2,3,100, 200)
median(n)
summary(n)


mean(s)
mean(s, trim = 1/10)
IQR(s)

sc = scan()

sals = c(12, .4, 5, 2, 50, 8, 3, 1, 4, .25)
sb = cut(sals, breaks = c(0,5,10,max(sals)))
sb

table(sb)

sb
str(sb)

#movies gross income
x = scan()
hist(x)
hist(x, probability = TRUE)
rug(jitter(x))

hist(x,100)
fivenum(x)
install.packages("UsingR")

library("Simple")

# playing with movies 
library(ggplot2movies)
data(movies)
head(movies)
summary(movies)
names(movies)
max(movies$rating)
min(movies$rating)
mean(movies$rating)
summary(movies$rating)

names(movies)
length(movies)
str(movies)
length(movies)
attach(movies)
boxplot(rating, main="rating")
boxplot(rating, main="rating", horizontal = TRUE)
names(movies)
detach(movies)

head(movies$Action)
summary(movies$Animation)
m.action = movies[movies$Action>0,]
head(m.action)
length(m.action)
nrow(m.action)
min(m.action$year)
max(m.action$year)

boxplot(m.action$year, main ="Action Movies by year")
names(movies)
m.comedy = movies[movies$Comedy>0,]
nrow(m.comedy)
summary(m.action$budget)
summary(m.comedy$budget)
boxplot(m.action$budget)

#list all available packages
library()
library("datasets")
data()

#simple.hist.and.boxplot
library(UsingR)
simple.hist.and.boxplot(m.comedy$budget)
simple.freqpoly(m.comedy$rating)


data("faithful")
hist(faithful)
hist(faithful$eruptions, probability = TRUE)
simple.freqpoly(faithful$eruptions, main="Faithful Eruptions")
lines(density(faithful$eruptions))
lines(density(faithful$eruptions, bw="SJ"), col="red")
f= scan()
# remove NAs
mean(f, na.rm = TRUE)
mean(f[!is.na(f)])

#calculate proportion of a decimal in pi200
cal.prop = function(x) length(which(pi2000==x))/length(pi2000)*100
#calculate the density plot
plot(density(pi2000))
polygon(density(pi2000), col="grey", border = "blue")

#bivariate categorical data experiments
smokes = c("Y","N","N","Y","N","Y","Y","Y","N","Y")
amount = c(1,2,2,3,3,1,2,1,3,2)
table(smokes,amount)

tmp=table(smokes,amount)
old.digits = options("digits")
options(digits=3) # only print 3 decimal places
prop.table(tmp,1)

prop.table(tmp,2)
prop.table(tmp)
#restore old digits
options(digits=7)
old.digits

barplot(table(smokes,amount))
barplot(table(amount,smokes))

smokes=factor(smokes)
smokes
barplot(table(smokes,amount), beside=TRUE, legend.text=T)
barplot(table(amount,smokes),main="table(amount,smokes)",  beside=TRUE, legend.text=c("less than 5","5-10","more than 10"))

#Handling bivariate data: categorical vs. numerical
x = c(5, 5, 5, 13, 7, 11, 11, 9, 8, 9)
y = c(11, 8, 4, 5, 9, 5, 10, 5, 4, 10)
boxplot(x,y)

data("home")
boxplot(scale(home$old), scale(home$new))

plot(density(home$old))
plot(density(home$new))

data("homedata")
plot(homedata$y1970, homedata$y2000)
plot(homedata$y1970)
plot( scale(homedata$y2000))

## linear regression
# x predictor | y response
#(measured y) - (predicted y) = residual
#linear regression - find the b0 and bi that minimizes the sum of the sqaure residuals
# USE LEAST SQAURE METHOD TO IDENTIFY THE b0 and bi that minimazies the sum of square residuals


data("homedata")
attach(home)
x=old
y=new
plot(x,y)
abline(lm(y~x))
simple.lm(x,y)
mr.res = simple.lm(x,y)
coef(mr.res)
simple.lm(x,y)
simple.lm(x,y,show.residuals=TRUE)

lm.res = simple.lm(x,y)
the.residuals = resid(lm.res) # how to get residuals
plot(the.residuals)

#Values or R2 close to 1 indicate a strong linear relationship,
#values close to 0 a weak one. (There still may be a relationship, just not a linear one.) In R the correlation coecient
#is found with the cor function
cor(x,y)
cor(x,y)^2
summary(lm(y~x))

data("florida")
help("florida")
str(florida)
simple.lm(BUSH,BUCHANAN)
summary(simple.lm(BUSH,BUCHANAN))
identify(BUSH,BUCHANAN,n=2)
simple.lm(BUSH[-50],BUCHANAN[-50])
summary(simple.lm(BUSH[-50],BUCHANAN[-50]))

simple.lm(BUSH,BUCHANAN)
abline(65.57350,0.00348)

#Resistance in statistics means the procedure is resistant to some percentage of arbitrarily large outliers,
#robustness
#means the procedure is not greatly aected by slight deviations in the assumptions.
detach(florida)

library(MASS)
attach(florida)
plot(x)
plot(BUSH,BUCHANAN)
abline(lm(BUCHANAN ~ BUSH), lty=1)
abline(rlm(BUCHANAN ~ BUSH),lty=2)
legend(locator(1),legend=c('lm','rlm'),lty=1:2)

plot(BUSH,BUCHANAN)
abline(rlm(BUCHANAN ~ BUSH),lty=1)
abline(rlm(BUCHANAN[-50] ~ BUSH[-50]),lty=2)

#plotting a parabola
x=seq(0,4,by=.1)
plot(x, x^2, type="l")
curve(x^2, 0,4)
curve(x^3, 0,4)
curve(log10(x), 0,4)

miles = (0:8)*4
tread = scan()
plot(miles,tread)
abline(lm(tread~miles))
lm(tread~miles)
summary(lm(tread~miles))

#exercises
st=1:10
q1 = c(3,3,3,4,3,4,3,4,3,4)
length(q1)
q2 = c(5,2,5,5,2,2,5,5,4,2)
q3 = c(1,3,1,1,1,3,1,1,1,1)
length(q3)
table(q1)
table(q2)
barplot(q1)
barplot(table(q1,q2), col = c("grey","red"))
barplot(table(q2,q3), col = c("cyan","blue"))
barplot(q1,q2,q3)


library(MASS)
data("UScereal")
attach(UScereal)
names(UScereal)
help("UScereal")
str(UScereal)
head(UScereal)

#finding a relationship between the shelf and the manufacturer
plot(shelf,mfr)
abline(lm(shelf~mfr))
library(UsingR)
simple.lm(shelf,mfr)
simple.lm(mfr,shelf, show.residuals = TRUE)
summary(simple.lm(shelf,mfr))
summary(lm(shelf~mfr))
#find the correlation
#in order to calculate correlation I need to conver manufacturer to a number
mfr.numeric = gsub('N',1,mfr)
mfr.numeric = gsub('K',2,mfr.numeric)
mfr.numeric = gsub('R',3,mfr.numeric)
mfr.numeric = gsub('P',4,mfr.numeric)
mfr.numeric = gsub('Q',5,mfr.numeric)
mfr.numeric = as.numeric(mfr.numeric)
mfr.numeric
#calculating r sqaure
cor(shelf,mfr.numeric)^2

plot(UScereal$sugars, UScereal$carbo)
abline(lm(UScereal$sugars~UScereal$carbo))
summary(lm(UScereal$sugars~UScereal$carbo))

cor(shelf,mfr)

plot(UScereal$fibre, UScereal$mfr)
abline(lm(UScereal$fibre~UScereal$mfr))
summary(lm(UScereal$fibre~UScereal$mfr))

plot(UScereal$sodium, UScereal$sugars)
abline(lm(UScereal$sodium~UScereal$sugars))
summary(lm(UScereal$sodium~UScereal$sugars))

#using mamals dataset
data("mammals")
help("mammals")
summary(mammals)
str(mammals)
plot(mammals)
plot(lm(mammals$brain~mammals$body))
summary(lm(mammals$brain~mammals$body))
simple.lm(mammals$brain, mammals$body)
cor(mammals$brain,mammals$body)
cor(mammals$brain,mammals$body)^2
#r square is [-1,1] - in our case we have 0.8726621 which is close to 1 meaning is good
plot(mammals$brain, mammals$body)
plot(log(mammals$brain), log(mammals$body))
abline(lm(log(mammals$brain)~log(mammals$body)))
summary(lm(log(mammals$brain)~log(mammals$body)))

#linear regression on home prices over time
data("homedata")
str(homedata)
head(homedata)
plot(homedata$y1970)
plot(homedata$y2000)
plot(homedata$y1970, homedata$y2000)
abline(lm(homedata$y1970~homedata$y2000))
abline(lm(homedata$y2000~homedata$y1970), col=2)
plot(log(homedata$y1970), log(homedata$y2000))
?abline


#looks like we got NAs
#check if there is any na
apply(homedata,1,function(x) sum(is.na(x)))

homedata.clea = complete.cases(homedata)
homedata.clean = na.omit(homedata)
nrow(homedata)
nrow(homedata.clea)
head(homedata.clea)
nrow(homedata.clean)

row.has.na <- apply(homedata, 1, function(x){any(is.na(x))})
row.has.nan <- apply(homedata, 1, function(x){any(is.nan(x))})
sum(row.has.na)
sum(row.has.nan)
clean.homedata = homedata[!row.has.na,]
nrow(clean.homedata)

plot(homedata$y2000, homedata$y1970)
abline(lm(homedata$y2000~homedata$y1970), col=2)

summary(lm(homedata$y2000~homedata$y1970))
h.lm = lm(homedata$y2000~homedata$y1970)
to.predict = data.frame(y1970=75000)
predict(h.lm, data.frame(x=seq(75000,0)))

head(homedata)
h2 = homedata
nrow(h2)
h2.p = predict(h.lm, h2)
str(h2.p)
par(mfrow=c(1,1))

plot(homedata)
?identify
identify(homedata, n=3)
#1809 2048 3529
homedata[1809,]
homedata[2048,]
homedata[3529,]

plot(homedata)
plot(homedata[-2048])
identify(homedata[-2048], n=2)

#removing outliers
homedata.no.outliers=homedata[-c(1809,2048,3529,220), ]
plot(homedata.no.outliers)
simple.lm(homedata.no.outliers$y2000, homedata.no.outliers$y1970)
?simple.lm
slm = simple.lm(homedata.no.outliers$y2000, homedata.no.outliers$y1970)
summary(slm)
plot(lm(homedata.no.outliers$y2000~homedata.no.outliers$y1970))

plot(homedata.no.outliers)
abline(lm(homedata.no.outliers$y2000~homedata.no.outliers$y1970), col=3)


row.has.zero = apply(homedata, 1, function(x){any(x==0)})
sum(row.has.zero)
sum(row.has.nan)
clean.homedata = homedata[!row.has.na,]


#using LR for prediction
x = rnorm(100)
y = x + rnorm(100)
plot(y,x)
abline(lm(y~x), col=2)
summary(lm(y~x))
simple.lm(y,x)
new <- data.frame(x = 100)
predict(lm(y ~ x), new)
predict(lm(y ~ x), new, se.fit = TRUE)
pred.w.plim <- predict(lm(y ~ x), new, interval = "prediction")
pred.w.clim <- predict(lm(y ~ x), new, interval = "confidence")
matplot(new$x, cbind(pred.w.clim, pred.w.plim[,-1]),
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")

predict(lm(homedata.no.outliers$y2000~homedata.no.outliers$y1970), data.frame(y1970=75000))

#predicting the home value
x = homedata.no.outliers$y1970
y = homedata.no.outliers$y2000
predict(lm(y ~ x), data.frame(x=75000))

simple.lm(x,y, pred = 75000)


#using florida election data set
data("florida")
help(florida)
str(florida)
plot(florida$BUSH, florida$BUCHANAN)
abline(lm(florida$BUCHANAN~florida$BUSH))
identify(florida$BUSH, florida$BUCHANAN, n=2)
florida.no.outliers = florida[-c(13,50),]
nrow(florida)
nrow(florida.no.outliers)
plot(florida$BUSH, florida$BUCHANAN)

simple.lm(florida$BUCHANAN,florida$BUSH)
summary(lm(florida$BUCHANAN~florida$BUSH))
summary(lm(florida.no.outliers$BUCHANAN~florida.no.outliers$BUSH))
simple.lm(florida.no.outliers$BUCHANAN, florida.no.outliers$BUSH)
?simple.lm

#working on CO2 emissions per capita
setwd("r:/data/GD/OurDocuments/family/Daniel/US_education/Coursera/Regression Models/repository/RRepo")
library(UsingR)
data(emissions)
str(emissions)
head(emissions)
#predictor per-capita GDP
#response CO2

simple.lm(emissions$perCapita, emissions$CO2)

plot(emissions$perCapita, emissions$CO2)
lm.em.perCapita=lm(emissions$CO2~emissions$perCapita)
lm.em.perCapita2=lm(emissions$perCapita~emissions$CO2)
abline(lm.em.perCapita, col=2)
abline(lm.em.perCapita2)
summary(lm.em.perCapita)

#identify the outliner 
identify(emissions$perCapita, emissions$CO2)
emissions.no.outliers = emissions[-c(1,7),]
nrow(emissions)
nrow(emissions.no.outliers)

#building the regression model w/o the outliers
plot(emissions.no.outliers$perCapita, emissions.no.outliers$CO2)
model1 = lm(emissions$CO2~emissions$perCapita)
model2 = lm(emissions.no.outliers$CO2~emissions.no.outliers$perCapita)
abline(model2, col=5)
summary(model1)

#removing more outliers
identify(emissions.no.outliers$perCapita, emissions.no.outliers$CO2, n=2)
summary(model2)
emissions.no.outliers.2 = emissions.no.outliers[-c(1,2),]
nrow(emissions.no.outliers.2)

plot(emissions.no.outliers.2$perCapita, emissions.no.outliers.2$CO2)
model1 = lm(emissions$CO2~emissions$perCapita)
model3 = lm(emissions.no.outliers.2$CO2~emissions.no.outliers.2$perCapita)
abline(model3, col=5)
summary(model3)

########################################## correlation
data("babies")
help("babies")
names(babies)
# correlation between age and weight - age as predictor and weight as response
cor(babies$wt,babies$age)
cor(babies$age,babies$wt)
cor(babies$gestation,babies$wt)
cor(babies$wt,babies$gestation)
plot(babies$gestation, babies$wt)
abline(lm(babies$wt~babies$gestation))
?simple.lm
model1 = simple.lm(babies$gestation, babies$wt)
model1
summary(model1)


###create a dataset
#predictor
x=rnorm(50)
#response variable
y = 20 + x*2
plot(x,y)
model2 = lm(y~x)
abline(model2, col=2)
simple.lm(x,y)
summary(model2)
plot(y~x)

# linear regression using cars data set
data("mtcars")
names(mtcars)
help("mtcars")
#maximum mpg
max(mtcars$mpg)
head(mtcars[order(-mtcars$mpg),], n=1)
head(mtcars, n=5)
str(mtcars)
c = mtcars
head(c)
ncol(c)
c["Valiant",]
c["Valiant",]$hp
plot(c$cyl~c$mpg)
abline(lm(c$cyl~c$mpg))
abline(lm(c$mpg~c$cyl))
plot(c$mpg~c$cyl)
abline(lm(c$cyl~c$mpg), col=2)
summary(lm(c$mpg~c$cyl))

#multivariate data
weight = c(150, 135, 210, 140)
height = c(65, 61, 70, 65)
gender = c("Fe","Fe","M","Fe")
study = data.frame(weight,height,gender) # make the data frame

str(study)
study
row.names(study)<-c("Mary","Alice","Bob","Judy")
study["Bob",]
study[,1:3]
study[,'gender']
study[study$gender == 'Fe',]

data("PlantGrowth")
help("PlantGrowth")
require(stats); require(graphics)
boxplot(weight ~ group, data = PlantGrowth, main = "PlantGrowth data",
        ylab = "Dried weight of plants", col = "lightgray",
        notch = TRUE, varwidth = TRUE)
anova(lm(weight ~ group, data = PlantGrowth))

boxplot(unstack(PlantGrowth))
boxplot(unstack(myData))
myData = cbind(mtcars$cyl, mtcars$mpg)
myData
#The response variable is on the left hand
#side and the predictor on the righ
#like y ~ x (x is the predictor and y is the response)
boxplot(PlantGrowth$weight~PlantGrowth$group)
library(MASS)
data("Cars93")
help("Cars93")
attach(Cars93)
price = cut(Price,c(0,12,20,max(Price)))
head(price)
str(price)
levels(price)=c("cheap","okay","expensive")
mpg = cut(MPG.highway,c(0,20,30,max(MPG.highway)))
levels(mpg) = c("gas guzzler","okay","miser")

table(Type)
table(price,Type)
table(price,Type,mpg)

barplot(table(price,Type),beside=T)
barplot(table(Type,price),beside=T) # type by different prices
detach(Cars93)

#boxplotting
y=rnorm(1000)
length(y)
head(y)
f = factor(rep(1:10, 100))
length(f)
str(f)
head(f)
boxplot(y~f, main="some bx")

#stricharting
x = rnorm(100)
y = factor(rep(1:10,10))
stripchart(x ~ y)

#some cools graphs
par(mfrow=c(1,3))
data("InsectSprays")
boxplot(InsectSprays$count~InsectSprays$spray,  col ='lightgrey')
simple.violinplot(count ~ spray, data = InsectSprays, col = "lightgray")
simple.densityplot(count ~ spray, data = InsectSprays)

plot(x,y)
points(x,z,pch="2")

#tooth growth
data("ToothGrowth")
plot(ToothGrowth$len~ToothGrowth$dose)
plot(ToothGrowth$len~ToothGrowth$dose, pch = as.numeric(ToothGrowth$supp))
tmp = levels(ToothGrowth$supp)
legend(locator(1),legend=tmp,pch=1:length(tmp))

#CO2 emission and GPD-per-capita nalysis 2
data("emissions")
simple.scatterplot(emissions$perCapita, emissions$CO2)
title("Emissions vs. GDP per capita")
head(emissions)
emissions[emissions$CO2== max(emissions$CO2),]
e = emissions
nrow(e)
e = rbind(e,"xCountry"=c(NA,100,1000))
nrow(e)
e
nrow(na.omit(e))
pairs(emissions)

library(lattice)
attach(Cars93)
xyplot(MPG.highway ~ Fuel.tank.capacity | Type)
plot.regression = function(x,y) { 
        panel.xyplot(x,y)  
        panel.abline(lm(y~x))
}
trellis.device(color = "white")
xyplot(MPG.highway ~ Fuel.tank.capacity | Type, panel = plot.regression)
detach()



####### playing again on the emissions

data("emissions")
help("emissions")
plot(emissions$perCapita, emissions$CO2)

getwd()
library(UsingR)
data("emissions")
setwd("R:/data/GD/OurDocuments/family/Daniel/US_education/Coursera/Regression Models/repository/RRepo")
plot(emissions$perCapita, emissions$CO2)
identify(emissions$perCapita, emissions$CO2, n=1)
e = emissions[-1,]
nrow(e)
nrow(emissions)
plot(e$perCapita, e$CO2)
pairs(e)


data("chips")
help("chips")
head(chips)
boxplot(chips)

data("chicken")
boxplot(chicken)

data("carbon")
help("carbon")
boxplot(Monoxide ~ Site,data=carbon)

data("babies")
pairs(babies)
help("babies")
simple.scatterplot(babies$gestation, babies$wt)
simple.scatterplot(babies$wt, babies$gestation, pch = babies$smoke)


#generating random data
?sample
rollDie = function(x) sample(1:6, x, replace = T)
table(rollDie(100))

sample(1:3, 5, replace = T)
sample(1:6, 6)

#random numbers genrations are described by a distribution - a distribution is a function 
#that specifies probabilities that a random number is in a specific range
# distribution probability density - continuos

# uniform distribution - all the values are equaly likely - die roll




