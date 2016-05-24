
library(UsingR)
library(ggplot2)
data("diamond")
help("diamond")

g = ggplot(diamond, aes(x=carat, y = price))
g = g + xlab("mass in carat")
g = g + ylab("price in SIN$")
g = g + geom_point(size=6, colour= "black", alpha = 0.2)
g = g + geom_point(size=5, colour= "blue", alpha = 0.2)
g = g + geom_smooth(method = "lm", colour="black")
g

fit = lm(diamond$price~diamond$carat)
coef(fit)
summary(fit)

fit2 = lm(diamond$price~I(diamond$carat-mean(diamond$carat)))
coef(fit2)


fit3 = lm(diamond$price~I(diamond$carat*10))
coef(fit3)

#predicting the price for new diamonds
#redo the fit 
fit = lm(price~carat, data=diamond)
n = c(0.3, 0.5, 0.8)
#predict
predict(fit, newdata = data.frame(carat = n))

