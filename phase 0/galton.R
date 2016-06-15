#regression models using Galton data 
#to load galton
library(HistData)

library(UsingR)
library(HistData)
data("Galton")
help("galton")

library(reshape)
long = melt(galton)
head(galton)
str(long)
head(long)
nrow(galton)
nrow(long)
?melt
g = ggplot(long, aes(x=value, fill=variable))
g = g + geom_histogram(colour="black", binwidth = 1)
g= g + facet_grid(.~variable)
g

#finding the center mass as the mean or mu

library(manipulate)
myHist <- function(mu){
        mse <- mean((galton$child - mu)^2)
        g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = "salmon", colour= "black", binwidth=1)
        g <- g + geom_vline(xintercept = mu, size = 3)
        g <- g + ggtitle(paste("mu = ", mu, ", MSE = ", round(mse, 2), sep = ""))
        g
}

manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))
#move the slider and see how the MSE (minimum squared error changes)

g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = "salmon", colour = "black", binwidth=1)
g <- g + geom_vline(xintercept = mean(galton$child), size = 3)
g

#
ggplot(galton, aes(x = parent, y = child)) + geom_point()
plot(galton); abline(lm(galton$child~galton$parent))


library(dplyr)
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")                    
g
        

y <- galton$child - mean(galton$child)
x <- galton$parent - mean(galton$parent)
freqData <- as.data.frame(table(x, y))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
myPlot <- function(beta){
        g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
        g <- g  + scale_size(range = c(2, 20), guide = "none" )
        g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
        g <- g + geom_point(aes(colour=freq, size = freq))
        g <- g + scale_colour_gradient(low = "lightblue", high="white")                     
        g <- g + geom_abline(intercept = 0, slope = beta, size = 3)
        mse <- mean( (y - beta * x) ^2 )
        g <- g + ggtitle(paste("beta = ", beta, "mse = ", round(mse, 3)))
        g
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))


lm(I(child - mean(child))~ I(parent - mean(parent)) - 1, data = galton)

x = 1:100
str(x)
mean(x)
y = x-mean(x)
mean(y)

#centering the data 
x.centered = x-mean(x)
mean(x.scalled)

#scaling the data 
x.scalled = x/sd(x)
sd(x.scalled)

#normalization: centering and scaling
z=(x-mean(x))/sd(x)

#mean 0
mean(z)
#standard deviation 1
sd(z)

#correlation [-1,1]
#measure the strength of linearity between x and y
cor(x,z)
cor(x,x)

#galton data
#child.height = intercept + parent.height*slope
#linear least square LLS
cor(galton$child, galton$parent)
cor(galton$parent, galton$child)
l = lm(galton$child~galton$parent)
l1 = lm(galton$parent~galton$child)
summary(l)
summary(l1)
cor(galton$child, galton$parent)
simple.lm(galton$child, galton$parent)


data("father.son")
help("father.son")
plot(sheight ~ fheight, data=father.son,bty="l",pch=20)
abline(a=0,b=1,lty=2,lwd=2)
abline(lm(sheight ~ fheight, data=father.son),lty=1,lwd=2)

#normalizing
y = (father.son$sheight - mean(father.son$sheight))/sd(father.son$sheight)
x = (father.son$fheight - mean(father.son$fheight))/sd(father.son$fheight)
mean(x)
mean(y)
sd(x)
sd(y)

sho = cor(x,y)
sho
g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g = g + geom_point(size = 6, colour = "black", alpha = 0.2)
g = g + geom_point(size = 4, colour = "salmon", alpha = 0.2)
g = g + xlim(-4, 4) + ylim(-4, 4)
g = g + geom_abline(intercept = 0, slope = 1)
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)
rho <- cor(x, y)
g = g + geom_abline(intercept = 0, slope = rho, size = 2)
g = g + geom_abline(intercept = 0, slope = 1 / rho, size = 2)
g

