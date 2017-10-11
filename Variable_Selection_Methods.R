##Variable Selection
install.packages("leaps")
library(tidyverse)
library(leaps)
library(glmnet)
options(stringsAsFactors = FALSE)
cts <- read.csv("data/counties.csv")

cts
## using the leaps library to determine best subsets
subs <- regsubsets(earn ~ ., data = wages)
subs
summary(subs)


summary(subs)$adjr2

summary(subs)$cp

summary(subs)$bic

#Create a dataframe to summarise the results

df <- data.frame(
  est = c(summary(subs)$adjr2, summary(subs)$cp, 
          summary(subs)$bic),
  x = rep(1:7, 3),
  type = rep(c("adjr2", "cp", "bic"), each = 7)
)

qplot(x, est, data = df, geom = "line") + theme_grey() + facet_grid(type ~ ., scales = "free_y")

?facet_grid

## now running this on the cts data

subs_cts <- regsubsets(crime ~., data = cts)
## so this comes up with an error checking you really want to do this based on the number of variables in the dataset
## at this point its best to do stepwise

## Stepwise regression
### First create some lm models

#stepwise doesnt deal with chr variables so well so create another dataframe without them
cts2 <- select(cts, -state, -county)

start.mod <- lm(crime ~ pop, data = cts2)

empty.mod <- lm(crime ~ 1, data = cts2)

full.mod <- lm(crime ~ ., data = cts2)

step (start.mod, scope = list(upper = full.mod, lower = empty.mod), direction = "both")

## the bottom has the best model BUT what happens if we had a different sample?

cts3 <- cts2[1:110,]

start.mod <- lm(crime ~ pop, data = cts3)

empty.mod <- lm(crime ~ 1, data = cts3)

full.mod <- lm(crime ~ ., data = cts3)

step (start.mod, scope = list(upper = full.mod, lower = empty.mod), direction = "both")


## Penalised Selection (LASSO method) - NO MATERIAL PROVIDED - Require extra research
# Much more robust than stepwise - can perform a variable selection to any number of variables
# Looks for the number of coefficients equal to 0 (ie contributes little) and minimises them
# Use a Log Lambda coefficient graph to select a model with the correct number of variables



