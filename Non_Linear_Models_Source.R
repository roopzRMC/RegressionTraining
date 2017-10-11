##Non Linear models

library(tidyverse)
qplot(carat, price, data = diamonds)

## Simple transformations
qplot(carat, price, data = diamonds) #- looks exponential - but if you log the variables it becomes linear

qplot(log(carat), log(price), data = diamonds) # now it looks more linear and makes more sense

log_model <- lm(log(price) ~ log(carat), data = diamonds)

summary(log_model)
plot(log_model)

## WHat if no handy transformation exists? eg
qplot(age, earn, data = wages) + geom_smooth() + coord_cartesian(ylim = c(0,75000))

## Check out Polynomials
# model y to x^2, X^2 + x^3 etc

polymod1 <- lm(earn ~ poly(age, 3), data = wages)
summary(polymod1)
