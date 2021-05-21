#setwd('/Users/andreacutrera/Desktop/Statistical Learning/project')

data <- read.csv('usa_final_v2.csv', sep=';')

data$precipitations <- as.numeric(data$precipitations)
str(data)

#Check NORMALITY
qqnorm(data$aqi)
hist(data$aqi)
shapiro.test(data$aqi)

#try with log
log_aqi <- log(data$aqi)
par(mfrow=c(1,2))
qqnorm(data$aqi,  main="(aqi)")
qqnorm(log_aqi,  main="Transformed log(aqi)")

par(mfrow=c(1,2))
hist(data$aqi, main='aqi')
hist(log_aqi, main="Transformed log(aqi)")

#now seems normal
shapiro.test(log_aqi)

#we can substitute the variable with the log
data$ln_aqi <- log_aqi

#California, District of Columbia and Wyoming the cleanest states
library(car)
par(mfrow=c(1,1))
Boxplot(~aqi, data=data, id=list(labels=data$state))
Boxplot(~ln_aqi, data=data, id=list(labels=data$state))


#data without NA
library(tidyr)
dt <- data %>% drop_na()


#linear FULL model 
dt <- dt[,-c(1,2)]
full.model <- lm(ln_aqi~., data = dt)
summary(full.model)
vif(full.model)
#only lockdown FALSE
sqrt(vif(full.model))>1.5


#best subset
#healthcare, precipitation, n_factories
library(leaps)
regfit.full=regsubsets(ln_aqi~.,data=dt)
reg.summary=summary(regfit.full)
names(reg.summary)
par(mfrow=c(2,2))
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="AdjR2")
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC")

which.min(reg.summary$cp) #7 variables
which.min(reg.summary$rss) #8 variables
which.max(reg.summary$adjr2) #8 variables
which.min(reg.summary$bic) #3 variables
#points(8,reg.summary$cp[8],pch=20,col="red")

plot(regfit.full,scale="Cp")
coef(regfit.full,8)


#STEPWISE FORWARD 
#n_factories, precipitations, professional, pop_rural, lockdown
regfit.fwd=regsubsets(ln_aqi~.,data=dt,method="forward", nvmax=10)
summary(regfit.fwd)
plot(regfit.fwd,scale="Cp")

#linear model of selected
model <- lm(ln_aqi~ n_factories + precipitations + 
            professional + pop_rural + lockdown, 
            data = dt)
summary(model)
#knn
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
  }
dt_nor <- as.data.frame(lapply(dt, normalize))
dt_train <- dt_nor[1:31,]
dt_test <- dt_nor[32:46,]

data[,2]
summary(data[,2])[4]
mean(data[,2])

dt[,17]
summary(dt[,17])[4]
mean(dt[,17])

dt$polluted <- dt$ln_aqi < mean(dt[,18])
summary(dt$polluted)
dt$polluted <- as.factor(dt$polluted)

dt_train_labels <- dt[1:31,19]
dt_test_labels <- dt[32:46,19]
summary(dt_train_labels)
summary(dt_test_labels)


library(class)
knn <- knn(train = dt_train, test = dt_test,cl = dt_train_labels, k=5)
tab <- table(knn, dt_test_labels)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
#86% ACCURACY

library(gmodels)
CrossTable(x=dt_test_labels, y=dt_test_pred, prop.chisq = F)





###NOT WORKING
#function to predict
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  mat[,names(coefi)]%*%coefi
}

set.seed(11)
#3-fold
folds=sample(rep(1:3,length=nrow(dt)))
folds
table(folds)
cv.errors=matrix(NA,10,10)
for(k in 1:3){
  best.fit=regsubsets(ln_aqi~.,data=dt[folds!=k,],nvmax=10,method="forward")
  for(i in 1:10){
    pred=predict(best.fit,dt[folds==k,],id=i)
    cv.errors[k,i]=mean((dt$ln_aqi[folds==k]-pred)^2)
  }
}
rmse.cv=sqrt(apply(cv.errors,2,mean))
plot(rmse.cv,type="b", ylim = c(0, 200))

#lasso-ridge
library(glmnet)
x=model.matrix(ln_aqi~.-1,data=dt) 
y=dt$polluted

#ridge
fit.ridge=glmnet(x,y,alpha=0)
plot(fit.ridge,xvar="lambda",label=TRUE)
cv.ridge=cv.glmnet(x,y,alpha=0)
plot(cv.ridge)
coef(cv.ridge)

#lasso
fit.lasso=glmnet(x,y)
plot(fit.lasso,xvar="lambda",label=TRUE)
cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)
coef(cv.lasso)


