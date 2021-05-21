setwd("~/Documents/DSE/Statistical Learning/Projects")
data <- read.csv('usa_final.csv', sep=',')

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
sqrt(vif(full.model))>10
# with 10 --> waste, healthcare, professional, retail THEY ARE DANGEROUS 
#because of multicollinearity
full.model <- lm(ln_aqi~.-waste, data = dt)
summary(full.model)
vif(full.model)
sqrt(vif(full.model))>10
#healthcare to remove
full.model <- lm(ln_aqi~.-waste-healthcare, data = dt)
summary(full.model)
vif(full.model)
sqrt(vif(full.model))>10
#NOW ALL FALSE


###############
#deleting healthcare and waste

dt <- dt[,-c(5,13)]
##############

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

which.min(reg.summary$cp) #7 variables
which.min(reg.summary$rss) #8 variables
which.max(reg.summary$adjr2) #8 variables
which.min(reg.summary$bic) #3 variables

#STEPWISE FORWARD 
#pop_rural, manufacturing, precipitations
regfit.fwd=regsubsets(ln_aqi~.,data=dt,method="forward", nvmax=10)
summary(regfit.fwd)

reg.summary<-summary(regfit.full)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",types="l")

#linear model of selected
model <- lm(ln_aqi~ pop_rural + manufacturing + precipitations, 
            data = dt)
summary(model)

#knn
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}

dt_nor <- as.data.frame(lapply(dt, normalize))

#CATEGORIZATION- CLASSIFICATION
library(plyr)
dt_nor$polluted <- dt_nor$ln_aqi < mean(dt_nor[,16])
dt_nor$polluted <- mapvalues(dt_nor$polluted, from=c(TRUE, FALSE), to=c('POLLUTED', 'CLEAN'))
dt_nor$polluted <- as.factor(dt_nor$polluted)
summary(dt_nor$polluted)
str(dt_nor)

#drop ln_aqi 
dt <- dt_nor[,-16]

set.seed(2)
train = sample(1:nrow(dt), 0.7*nrow(dt))
dt_train = dt[train,-16]
dt_test = dt[-train,-16]
dt_train_labels <- dt[train, 16]
dt_test_labels <- dt[-train, 16]
summary(dt_train_labels)
summary(dt_test_labels)


#K-selection
library(caret)
library(e1071)
ctrl <- trainControl(method="repeatedcv",repeats = 3)
knnFit <- train(polluted ~ ., data = dt, method = "knn", 
                trControl = ctrl, preProcess = c("center","scale"), 
                tuneLength = 20)

plot(knnFit)
knnFit$bestTune ###### 7-NN the best one

#7-NN
library(class)
knn <- knn(train = dt_train, test = dt_test,cl = dt_train_labels, k=7)
tab <- table(knn, dt_test_labels)
tab
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
#62.5% ACCURACY


#CV ---- > knn_cv <- knn.cv(dt_train, dt_train_labels, k = 7, prob = FALSE)

#KNN with relevant variables
#pop_rural, manufacturing, precipitations
knnFit_subset <- train(polluted ~ pop_rural + manufacturing + precipitations,
                       data = dt, method = "knn", trControl = ctrl, 
                       preProcess = c("center","scale"), tuneLength = 20)
plot(knnFit_subset)
knnFit_subset$bestTune #5-NN best one

knn_subset <- knn(train = dt_train[,c(6,12,14)], test = dt_test[,c(6,12,14)],
                  cl = dt_train_labels, k=5)
tab <- table(knn_subset, dt_test_labels)
tab
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
#same result but less variables used (just 3)

#PLOT IN 3 dimensional space

install.packages("plot3D")
library(plot3D)

colVar <- factor(dt$polluted)


scatter3D(dt$pop_rural, dt$manufacturing, dt$precipitations, 
          colvar=as.integer(colVar),
          phi = 0, bty ="g",
          pch = 20, cex = 1.5,
          col = c("#1B9E77", "#D95F02"),
          xlab = "Rural pop",
          ylab ="Manufacturing", zlab = "Precipitations",
          colkey = list(at = c(1, 2), side = 4, 
                        addlines = TRUE, length = 0.5, width = 0.5,
                        labels = c("Clean", "Polluted")))
library(rgl)
plotrgl()


##### KNN with 2 variables

#KNN with relevant variables
#pop_rural, manufacturing, precipitations
knnFit_subset_2 <- train(polluted ~ pop_rural + manufacturing,
                       data = dt, method = "knn", trControl = ctrl, 
                       preProcess = c("center","scale"), tuneLength = 20)
plot(knnFit_subset_2)
knnFit_subset_2$bestTune #5-NN best one

knn_subset_2 <- knn(train = dt_train[,c(12,14)], test = dt_test[,c(12,14)],
                  cl = dt_train_labels, k=31)
tab <- table(knn_subset_2, dt_test_labels)
tab
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
#same result but less variables used (just 2)


## Plot

library(plyr)
library(ggplot2)
set.seed(123)


# Create a dataframe to simplify charting
plot.df = data.frame(dt, predicted = knnFit_subset_2)

# Use ggplot
# 2-D plots example only
# Sepal.Length vs Sepal.Width

# First use Convex hull to determine boundary points of each cluster
plot.df1 = data.frame(x = plot.df$Sepal.Length, 
                      y = plot.df$Sepal.Width, 
                      predicted = plot.df$predicted)

find_hull = function(df) df[chull(df$x, df$y), ]
boundary = ddply(plot.df1, .variables = "predicted", .fun = find_hull)

ggplot(plot.df, aes(Sepal.Length, Sepal.Width, color = predicted, fill = predicted)) + 
  geom_point(size = 5) + 
  geom_polygon(data = boundary, aes(x,y), alpha = 0.5)



###############################################################################
library(tree)
library(ISLR)
#attach(dt)

tree = tree(polluted~., dt)
summary(tree)
plot(tree)
text(tree, pretty = 0)

tree_train <- tree(polluted~., dt[train,])
tree_pred <- predict(tree_train, dt[-train,], type = 'class')
table(tree_pred, dt_test_labels)

(7+3)/16  #2 out of 3 are correct

#PRUNING
tree_cv <- prune.misclass(tree_train, k = NULL, best = NULL, dt[-train,],
                          eps = 1e-3)

plot(tree_cv)

plot(print(tree))
tree <- tree(polluted~., dt, subset=train)
tree_cv <- cv.tree(tree, FUN = prune.misclass, K=10)


###############################################################################
###NOT WORKING

for(i in 2:5)  fgl.cv$dev <- fgl.cv$dev +
  cv.tree(fgl.tr,, prune.tree)$dev
fgl.cv$dev <- fgl.cv$dev/5
plot(fgl.cv)






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
