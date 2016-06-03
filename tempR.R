


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



g = ggpairs(mtcars)

g

cor.table = cor(mtcars)
cor.table
str(cor.table)
nrow(cor.table)
cor.table[mpg>0.6]

cor(mtcars[,c("mpg", "cyl", "wt","am", "gear")])

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

g = ggplot(mtcars, aes(x = cyl, y = am))
g = g + xlab("NUmber of Cylinders")
g = g + ylab("MPG")
g = g + geom_point(size = 7, colour = "black", alpha=0.5)
g = g + geom_point(size = 5, colour = "blue", alpha=0.2)
g = g + geom_smooth(method = "lm", colour = "black")
g
model.1 = lm(mpg~cyl, data = mtcars)
summary(model.1)
coef(model.1)
model.2 = lm(mpg~wt, data = mtcars)
summary(model.2)
coef(model.2)
model.3 = lm(mpg~am, data = mtcars)
coef(model.3)
summary(model.3)
summary(model.1)$sigma
summary(model.1)$r.squared

summary(model.2)$sigma
summary(model.3)$sigma
anova(model.1, model.2, model.3)


plot(diamond$carat, diamond$price,  
     xlab = "Mass (carats)", 
     ylab = "Price (SIN $)", 
     bg = "lightblue", 
     col = "black", cex = 2, pch = 21,frame = FALSE)
abline(fit, lwd = 2)
for (i in 1 : n) 
        lines(c(x[i], x[i]), c(y[i], yhat[i]), col = "red" , lwd = 2)


data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
e <- resid(fit)
yhat <- predict(fit)
max(abs(e -(y - yhat)))
max(abs(e - (y - coef(fit)[1] - coef(fit)[2] * x)))

plot(x, e, xlab = "Mass (carats)", ylab = "Residuals (SIN $)", bg = "lightblue", col = "black", cex = 2, pch = 21,frame = FALSE)
abline(h = 0, lwd = 2)
for (i in 1 : n) 
        lines(c(x[i], x[i]), c(e[i], 0), col = "red" , lwd = 2)


plot(mtcars$wt, resid(lm(mtcars$mpg~mtcars$wt)), xlab = "Car Weight", ylab = "Residuals MPG", bg = "lightblue", col = "black", cex = 2, pch = 21,frame = FALSE)
abline(h = 0, lwd = 2)
for (i in 1 : length(mtcars$wt)) 
        lines(c(mtcars$wt[i], mtcars$wt[i]), c(resid(fit)[i], 0), col = "red" , lwd = 2)


e = c(resid(lm(price ~ 1, data = diamond)),
      resid(lm(price ~ carat, data = diamond)))
fit = factor(c(rep("Itc", nrow(diamond)),
               rep("Itc, slope", nrow(diamond))))
g = ggplot(data.frame(e = e, fit = fit), aes(y = e, x = fit, fill = fit))
g = g + geom_dotplot(binaxis = "y", size = 2, stackdir = "center", binwidth = 20)
g = g + xlab("Fitting approach")
g = g + ylab("Residual price")
g


model.1 = lm(mpg~cyl, data = mtcars)
model.2 = lm(mpg~wt, data = mtcars)
model.3 = lm(mpg~am, data = mtcars)
anova(model.1, model.2, model.3)



summary(lm(mpg ~ . , data = mtcars))$coef


initial.model <- lm(mpg ~., data= mtcars)
best.model <- step(initial.model, direction = "both")


require(MASS)
mtcars.glm <- glm(mpg ~ ., data = mtcars)
stepwise(mtcars.glm, trace = FALSE)
stepwise(mtcars.glm, direction="forward/backward")

step <- stepAIC(mtcars.glm, direction="both")
step$anova # display results

library(relaimpo)
pack
calc.relimp(fit,type=c("lmg","last","first","pratt"),
            rela=TRUE)

library(relaimpo)
install.packages("relaimpo")
data(swiss)
calc.relimp(swiss, 
            type = c("lmg", "last", "first", "betasq", "pratt", "genizi", "car") )
# calculation of all available relative importance metrics 
# non-US version offers the additional metric "pmvd", 
# i.e. call would be 
# calc.relimp(cov(swiss), 
# type = c("lmg", "pmvd", "last", "first", "betasq, "pratt"), 
# rela = TRUE )
## same analysis with formula or lm method and a few modified options
crf <- calc.relimp(Fertility~Agriculture+Examination+Education+Catholic+Infant.Mortality,swiss, 
                   subset = Catholic>40,
                   type = c("lmg", "last", "first", "betasq", "pratt"), rela = TRUE )
crf



library(relaimpo)
install.packages("relaimpo")


initial.model <- lm(mpg ~., data= mtcars)
best.model <- step(initial.model, direction = "both")
summary(best.model)
?calc.relimp()
calc.relimp(best.model)