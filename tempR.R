


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
g= ggpairs(mtcars, mapping = aes(color = am), columns =c("am","cyl","gear", "mpg"),continuous = "smooth" )
g
pairs(mtcars.reduced)
