################################################################################
#load dataset

data <- read.csv('usa_final.csv', sep=',')
data <- data[,-c(20,21)]
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
#and without the first 2 variables
dt <- dt[,-c(1,2)]

################################################################################

#linear FULL model - Adjusted R-squared:  0.4933
full.model <- lm(ln_aqi~., data = dt)
summary(full.model)
vif(full.model)
sqrt(vif(full.model))>10

#DROP WASTE - Adjusted R-squared:  0.4966 
full.model <- lm(ln_aqi~.-waste, data = dt)
summary(full.model)
vif(full.model)
sqrt(vif(full.model))>10
sqrt(vif(full.model))>5

#DROP HEALTHCARE - Adjusted R-squared:  0.5044 
full.model <- lm(ln_aqi~.-waste-healthcare, data = dt)
summary(full.model)
vif(full.model)
sqrt(vif(full.model))>10
sqrt(vif(full.model))>5

#DROP CONSTRUCTION - Adjusted R-squared:  0.503
full.model <- lm(ln_aqi~.-waste-healthcare-construction, data = dt)
summary(full.model)
vif(full.model)
sqrt(vif(full.model))>10
sqrt(vif(full.model))>5

#DROP UTILITIES - Adjusted R-squared:  0.5021 
full.model <- lm(ln_aqi~.-waste-healthcare-construction-utilities, data = dt)
summary(full.model)
vif(full.model)
sqrt(vif(full.model))>10
sqrt(vif(full.model))>5

#DROP PROFESSIONAL - Adjusted R-squared:  0.4296
full.model <- lm(ln_aqi~.-waste-healthcare-construction-utilities-professional, data = dt)
summary(full.model)
vif(full.model)
sqrt(vif(full.model))>10
sqrt(vif(full.model))>5

#DROP RETAIL - Adjusted R-squared:  0.3978
full.model <- lm(ln_aqi~.-waste-healthcare-construction-utilities-professional-retail, data = dt)
summary(full.model)
vif(full.model)
sqrt(vif(full.model))>10
sqrt(vif(full.model))>5

#DROP FINANCE - Adjusted R-squared:  0.4129 
full.model <- lm(ln_aqi~.-waste-healthcare-construction-utilities-professional-retail-finance, data = dt)
summary(full.model)
vif(full.model)
sqrt(vif(full.model))>10
sqrt(vif(full.model))>5

################################################################################

#deleting all
dt <- dt[,-c(13, 5, 2, 12, 9, 10, 4)]
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}
dt <- as.data.frame(lapply(dt, normalize))
################################################################################

#best subset
library(leaps)
regfit.full=regsubsets(ln_aqi~.,data=dt)
reg.summary=summary(regfit.full)
names(reg.summary)
par(mfrow=c(2,2))
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="AdjR2")
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC")

which.min(reg.summary$cp) #6 variables
which.min(reg.summary$rss) #8 variables
which.max(reg.summary$adjr2) #7 variables
which.min(reg.summary$bic) #3 variables

#STEPWISE FORWARD 
#pop_rural, manufacturing, precipitations, n_factories
regfit.fwd=regsubsets(ln_aqi~.,data=dt,method="forward", nvmax=10)
summary(regfit.fwd)

reg.summary<-summary(regfit.full)
par(mfrow=c(1,1))
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",types="l")

#linear model of selected - Adjusted R-squared:  0.3853 
model <- lm(ln_aqi~ pop_rural + manufacturing + precipitations + n_factories, 
            data = dt)
summary(model)

################################################################################
#knn

#CATEGORIZATION - CLASSIFICATION
library(dplyr)
data$cl <- cut(data$aqi, breaks = c(50,100,150,200),
               labels = c('yellow', 'orange', 'red'))
dt <- dt[,-11]
dt$polluted <- data$cl
dt$polluted <- as.factor(dt$polluted)
summary(dt$polluted)
str(dt)

################################################################################
#train test split

set.seed(123)
train = sample(1:nrow(dt), 0.7*nrow(dt))
dt_train = dt[train,-11]
dt_test = dt[-train,-11]
dt_train_labels <- dt[train, 11]
dt_test_labels <- dt[-train, 11]
summary(dt_train_labels)
summary(dt_test_labels)


#K-selection
library(caret)
library(e1071)
ctrl <- trainControl(method="repeatedcv",repeats = 3)
knnFit <- train(polluted~., data = dt, method = "knn", 
                trControl = ctrl, preProcess = c("center","scale"), 
                tuneLength = 20)

plot(knnFit)
knnFit$bestTune ###### 11-NN the best one

#9-NN
library(class)
knn <- knn(train = dt_train, test = dt_test,cl = dt_train_labels, k=9)
tab <- table(knn, dt_test_labels)
tab
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
#81.25% ACCURACY

#KNN with relevant variables
#pop_rural, manufacturing, precipitations, n_factories
knnFit_subset <- train(polluted ~ pop_rural + manufacturing + precipitations 
                       + n_factories,
                       data = dt, method = "knn", trControl = ctrl, 
                       preProcess = c("center","scale"), tuneLength = 20)
plot(knnFit_subset)
knnFit_subset$bestTune #11-NN best one again

knn_subset <- knn(train = dt_train[,c(9,4,7)], test = dt_test[,c(9,4,7)],
           cl = dt_train_labels, k=11)
tab <- table(knn_subset, dt_test_labels)
tab
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
#same result but less variables used (just 3)

#PLOT IN 3 dimensional space
library(plot3D)

colVar <- factor(dt$polluted)


scatter3D(dt$pop_rural, dt$manufacturing, dt$precipitations, 
          colvar=as.integer(colVar),
          phi = 0, bty ="g",
          pch = 20, cex = 1.5,
          col = c("#1B9E77", "#D95F02", "#FF0000"),
          xlab = "Rural pop",
          ylab ="Manufacturing", zlab = "Precipitations",
          colkey = list(at = c(1, 2, 3), side = 4, 
                        addlines = TRUE, length = 0.5, width = 0.5,
                        labels = c("Yellow", "Orange", "Red")))
library(rgl)
plotrgl()


###############################################################################
library(tree)
library(ISLR)

tree = tree(polluted~pop_rural + manufacturing + precipitations 
            + n_factories, dt)
summary(tree)
plot(tree)
text(tree, pretty = 0)

tree_train <- tree(polluted~pop_rural + manufacturing + precipitations 
                   + n_factories, dt[train,])
tree_pred <- predict(tree_train, dt[-train,], type = 'class')
table(tree_pred, dt_test_labels)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(table(tree_pred, dt_test_labels))

#PRUNING
tree_cv <- prune.misclass(tree_train, k = NULL, best = NULL, dt[-train,],
                          eps = 1e-3)

plot(tree_cv)

#cv
dtt <- dt[,c(4,7,9,10,11)]
dtt
tree1=tree(polluted~.,dtt,subset=train)
plot(tree1);text(tree1,pretty=0)
cv_tree1=cv.tree(tree1,FUN=prune.misclass)
cv_tree1
plot(cv_tree1);text(cv_tree1,pretty=0)
plot(cv_tree1)

#TREE with relevany variables
tree = tree(polluted~., dt)
summary(tree)
plot(tree)
text(tree, pretty = 0)

tree_train <- tree(polluted~., dt[train,])
tree_pred <- predict(tree_train, dt[-train,], type = 'class')
table(tree_pred, dt_test_labels)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(table(tree_pred, dt_test_labels))



####################
require(randomForest)
rf.tree=randomForest(polluted~.,data=dt,subset=train)
rf.tree

oob.err=double(10)
test.err=double(10)
for(mtry in 1:10){
  fit=randomForest(polluted~.,data=dt,subset=train,mtry=mtry,ntree=400)
  oob.err[mtry]=fit$mse[400]
  pred=predict(fit,dt[-train,])
  test.err[mtry]=with(dt[-train,],mean((polluted-pred)^2))
  cat(mtry," ")
}

####################
#random forest
library(randomForest)
rf.tree=randomForest(polluted~.,data=dt,subset=train)
rf.tree

pred_rf <- predict(rf.tree, dt_test)
table(pred_rf, dt_test_labels)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(table(pred_rf, dt_test_labels))




oob.err=double(11)
test.err=double(11)
for(mtry in 1:11){
  fit=randomForest(polluted~.,data=dt,subset=train,mtry=mtry,ntree=500)
  oob.err[mtry]=fit$mse[500]
  pred=predict(fit,dt[-train,])
  test.err[mtry]=with(dt[-train,],mean((polluted-pred)^2))
  cat(mtry," ")
}


#lasso
fit.lasso=glmnet(x,y)
plot(fit.lasso,xvar="lambda",label=TRUE)
cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)
coef(cv.lasso)


