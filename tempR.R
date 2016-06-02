


data("mtcars")
head(mtcars)

library(UsingR)
simple.eda(mtcars$mpg)
?simple.eda

require(ggplot2)
require(GGally)

names(mtcars)
mtcars.reduced = mtcars[c("mpg", "cyl","am","gear")]

mtcars.reduced.2 = mtcars[c("am","cyl","gear", "mpg")]

g = ggpairs(mtcars.reduced.2, lower = list(
        continuous = "smooth",
        combo = "facetdensity")
)




g = ggpairs(mtcars, columns =c("am","cyl","gear", "mpg"), lower = list(
        continuous = "smooth",
        combo = "facetdensity", diag = NULL)
)
g


correlations.plot = ggpairs(mtcars, columns =c("mpg","cyl","gear", "am"),  
                    columnLabels = c("MPG", "Numbers of Cylinders", "Number of Gears", "Transmission"), lower = list(
                    continuous = "smooth",
                    combo = "facetdensity", diag = NULL)
)
correlations.plot


correlations.plot = ggpairs(mtcars, columns =c("mpg","cyl","am"),  
                            columnLabels = c("MPG", "Numbers of Cylinders","Transmission"), lower = list(
                                    continuous = "smooth",
                                    combo = "facetdensity", diag = NULL)
)
correlations.plot

g = ggpairs(mtcars, columns =c("am","cyl","gear", "mpg"))

g

pairs(mtcars.reduced)




g = ggplot(mtcars, aes(x = cyl, y = mpg))
g = g + xlab("NUmber of Cylinders")
g = g + ylab("MPG")
g = g + geom_point(size = 7, colour = "black", alpha=0.5)
g = g + geom_point(size = 5, colour = "blue", alpha=0.2)
g = g + geom_smooth(method = "lm", colour = "black")
g