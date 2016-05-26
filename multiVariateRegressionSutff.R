
n = 100; x = rnorm(n); x2 = rnorm(n); x3 = rnorm(n)
y = 1 + x + x2 + x3 + rnorm(n, sd = .1)
ey = resid(lm(y ~ x2 + x3))
ex = resid(lm(x ~ x2 + x3))
sum(ey * ex) / sum(ex ^ 2)
coef(lm(ey ~ ex - 1))
coef(lm(y ~ x + x2 + x3)) 

library(datasets)
data("swiss")
help("swiss")
head(swiss)

require(GGally)
require(ggplot2)
g = ggpairs(swiss, lower = list(continuous = "smooth"),
            params = c(method = "loess"))
help("wrap", package = "GGally")
g

g = ggpairs(swiss)
g

#for more details on ggpairs see https://cran.r-project.org/web/packages/GGally/vignettes/ggpairs.html
help(swiss)

g = ggpairs(swiss, lower = list(
        continuous = "smooth",
        combo = "facetdensity")
        )
g

summary(lm(Fertility~., data = swiss))

summary(lm(Fertility~., data = swiss))$coefficients
summary(lm(Fertility~Agriculture, data = swiss))$coefficients


n <- 100
x2 <- 1 : n 
x1 <- .01 * x2 + runif(n, -.1, .1)
y = -x1 + x2 + rnorm(n, sd = .01)
summary(lm(y ~ x1))$coef
summary(lm(y ~ x1 + x2))$coef

dat = data.frame(y = y, x1 = x1, x2 = x2, ey = resid(lm(y ~ x2)), ex1 = resid(lm(x1 ~ x2)))
library(ggplot2)
g = ggplot(dat, aes(y = y, x = x1, colour = x2))
g = g + geom_point(colour="grey50", size = 5) + geom_smooth(method = lm, se = FALSE, colour = "black") 
g = g + geom_point(size = 4) 
g
g2 = ggplot(dat, aes(y = ey, x = ex1, colour = x2))  
g2 = g2 + geom_point(colour="grey50", size = 5) + geom_smooth(method = lm, se = FALSE, colour = "black") + geom_point(size = 4) 
g2


#back to swiss data
z =  swiss$Agriculture = swiss$Education
summary(lm(Fertility~.+z, data = swiss))$coefficients

#spray stuff
require(datasets);data(InsectSprays); require(stats); require(ggplot2)
help("InsectSprays")
boxplot(count ~ spray, data = InsectSprays,
        xlab = "Type of spray", ylab = "Insect count",
        main = "InsectSprays data", varwidth = TRUE, col = "lightgray")
head(InsectSprays)

g = ggplot(data = InsectSprays, aes(y = count, x = spray, fill  = spray))
g = g + geom_violin(colour = "black", size = 2)
g = g + xlab("Type of spray") + ylab("Insect count")
g

