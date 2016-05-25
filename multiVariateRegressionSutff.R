
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
