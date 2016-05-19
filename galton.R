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

