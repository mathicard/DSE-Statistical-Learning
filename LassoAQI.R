library(glmnet)
library(ISLR)
library(tidyr)
library(dplyr)

#this analysis has been done following the example of the book at chapter 6
#Ridge Reg and Lasso


#fixing and preparing data

usafinal <- usa_final_v2[,-1]
rownames(usafinal) <- usa_final_v2[,1]

sapply(usafinal, class)

usafinal$precipitations <- as.numeric(usafinal$precipitations) #here there was
                                                               #a problem with a
                                                               #character-column

rm(usa_final_v2)

usafinal=na.omit(usafinal) #omitting na values

View(usafinal)

x=model.matrix(aqi~., usafinal)[,-1] #it creates the model matrix with all vars

y=usafinal$aqi

View(y)
grid = 10^seq(10, -2, length = 100)

#preparing the training set

set.seed(123)
train=sample(1:nrow(x), 0.7*nrow(x))
test=(-train)
y.test=y[test]

View(train)

#finally we apply the lasso

lasso.mod=glmnet(x[train,], y[train], alpha=1, lambda = grid)
plot(lasso.mod)

#now perform cross-validation and test error

set.seed(123)
cv.out=cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)

bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam, newx = x[test,])
mean((lasso.pred-y.test)^2)

#now, we can analyse the dependence of the vars with aqi

out=glmnet(x,y,alpha = 1, lambda = grid)
lasso.coef=predict(out,type = "coefficients", s=bestlam)[1:18]
lasso.coef

#lastly, the Lasso filters the ones who have a non zero dependence with aqi,
#thus, creating the subset 

lasso.coef[lasso.coef!=0]

#the model presents a negative relation between the levels of pm2.5 in the air
#and the vars related to the level of precipitations, the lockdown and the population
#living in rural areas.
#On the other hand, the higher the number of factories, the higher
#the level of pm2.5. The levels of GDP related to manufacturing and information
#present a positive relation too






