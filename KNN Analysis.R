### K-NN with Caret package ###

library(RCurl)
x <- getURL("https://raw.githubusercontent.com/mathicard/Statistical-Learning-DSE/main/usa_final.csv")
dataset <- read.csv(text = x)


#AQI categorization
dataset$polluted <- cut(dataset$aqi, breaks = c(50,100,150,200),
                        labels = c('yellow', 'orange', 'red'))


#kNN requires variables to be normalized

#Clustering analysis is not negatively affected by heteroscedasticity 
#but the results are negatively impacted by multicollinearity of features/variables 
#used in clustering as the correlated feature/variable will carry extra weight 
#on the distance calculation than desired.
#These will influence our coefficients, but not the predictions. 
#Therefore, we decided to keep them.


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}

dt <- as.data.frame(cbind(lapply(dataset[,-c(1,2,20,21,22)], normalize), dataset[,c(1,20,21,22)]))

summary(dt)

#Extract training and test sets (stratified)

set.seed(12)
library(caret)
train.index <- createDataPartition(dt$polluted, p = .7, list = FALSE)
dt_train <- dt[ train.index,-c(18,19,20)]
dt_test  <- dt[-train.index,-c(18,19,20)]

dt_train_labels <- dt[train.index, 21]
dt_test_labels <- dt[-train.index, 21]

table1 <- table(dt_train_labels)
round(prop.table(table1), 2)

table2 <- table(dt_test_labels)
round(prop.table(table2), 2)

dt_train <- dt_train[,-18]
dt_test <- dt_test[,-18]


##load the package class
library(class)


### Plot training-test error by K value

library(class)
error.train <- replicate(0,20)

for(k in 1:20) {
  pred_pol <- knn(train = dt_train, test = dt_test, cl = dt_train_labels, k)
  error.train[k]<-1-mean(pred_pol==dt_train_labels)
}

error.train <- unlist(error.train, use.names=FALSE)

error.test <- replicate(0,20)
for(k in 1:20) {
  pred_pol <- knn(train = dt_train, test = dt_test, cl = dt_train_labels, k)
  error.test[k]<-1-mean(pred_pol==dt_test_labels)
}

error.test <- unlist(error.test, use.names = FALSE)

plot(error.train, type="o", col="blue", ylim=c(0,0.5), xlab = "K values", ylab = "Misclassification errors")
lines(error.test, type = "o", ylim=c(0,0.5), col="red")
legend("topright", legend=c("Training error","Test error"), col = c("blue","red"), lty=1:1)

which(error.train==min(error.train)) # k=5
which(error.test==min(error.test)) # k=5

##run knn function (k=5)
pr <- knn(dt_train,dt_test,cl=dt_train_labels,k=5)

##create confusion matrix
tab <- table(pr,dt_test_labels)
tab

##this function divides the correct predictions by total number of predictions that tell us how accurate the model is.

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

#Accuracy of 85%

combinetest <- cbind(dt_test, dt[-train.index, 18])
combinetest[dt_test_labels != pr,]

#Arizona and Michigan are the States misclassified. Let see the AQI

dataset[dataset$state=="Arizona", 2] #128.23
dataset[dataset$state=="Michigan", 2] #114.11


## Cross Validation to choose K

set.seed(12)
ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)

knnFit <- train(polluted ~ .,
                      data = dt[ train.index,-c(18,19,20)], method = "knn", trControl = ctrl, 
                      preProcess = c("center","scale"), tuneLength = 20)

plot(knnFit)
knnFit$bestTune #higher accuracy with k=7

#Output of kNN fit
knnFit


##run knn function (k=7)
pr <- knn(dt_train,dt_test,cl=dt_train_labels,k=7)

##create confusion matrix
tab <- table(pr,dt_test_labels)
tab

accuracy(tab)

#Accuracy of 77%

### Conclusion: we work with only 5 nearest neighbours to classify the air quality of the States.


### Now we only use most relevant variables to predict AQI (upon FORWARD SEARCH)
# (pop_rural + manufacturing + precipitations + n_factories)

### Plot training-test error by K value
set.seed(12)

library(class)
error.train <- replicate(0,20)

dt_train2 <- dt_train[,c("manufacturing","pop_rural","precipitations",'n_factories')]
dt_test2 <- dt_test[,c("manufacturing","pop_rural","precipitations",'n_factories')]


for(k in 1:20) {
  pred_pol <- knn(train = dt_train2, test = dt_test2, cl = dt_train_labels, k)
  error.train[k]<-1-mean(pred_pol==dt_train_labels)
}

error.train <- unlist(error.train, use.names=FALSE)

error.test <- replicate(0,20)
for(k in 1:20) {
  pred_pol <- knn(train = dt_train2, test = dt_test2, cl = dt_train_labels, k)
  error.test[k]<-1-mean(pred_pol==dt_test_labels)
}

error.test <- unlist(error.test, use.names = FALSE)

plot(error.train, type="o", col="blue", ylim=c(0,0.7), xlab = "K values", ylab = "Misclassification errors")
lines(error.test, type = "o", ylim=c(0,0.5), col="red")
legend("topright", legend=c("Training error","Test error"), col = c("blue","red"), lty=1:1)

which(error.train==min(error.train)) # k>=8
which(error.test==min(error.test)) # k>=3 

##run knn function (k=9)
pr9 <- knn(dt_train2,dt_test2,cl=dt_train_labels,k=9)

##create confusion matrix
tab <- table(pr9,dt_test_labels)
tab

accuracy(tab)

#Accuracy of 77%


##run knn function (k=5)
pr5 <- knn(dt_train2,dt_test2,cl=dt_train_labels,k=5)

##create confusion matrix
tab <- table(pr5,dt_test_labels)
tab

accuracy(tab)

#Accuracy of 77%



##run knn function (k=6)
pr6 <- knn(dt_train2,dt_test2,cl=dt_train_labels,k=6)

##create confusion matrix
tab <- table(pr6,dt_test_labels)
tab

accuracy(tab)

#Accuracy of 69%




### Now we only use the 3 most relevant variables to predict AQI (upon FORWARD SEARCH)
# (pop_rural + manufacturing + precipitations)

### Plot training-test error by K value

library(class)
error.train <- replicate(0,20)

dt_train2 <- dt_train[,c("manufacturing","pop_rural","precipitations")]
dt_test2 <- dt_test[,c("manufacturing","pop_rural","precipitations")]


for(k in 1:20) {
  pred_pol <- knn(train = dt_train2, test = dt_test2, cl = dt_train_labels, k)
  error.train[k]<-1-mean(pred_pol==dt_train_labels)
}

error.train <- unlist(error.train, use.names=FALSE)

error.test <- replicate(0,20)
for(k in 1:20) {
  pred_pol <- knn(train = dt_train2, test = dt_test2, cl = dt_train_labels, k)
  error.test[k]<-1-mean(pred_pol==dt_test_labels)
}

error.test <- unlist(error.test, use.names = FALSE)

plot(error.train, type="o", col="blue", ylim=c(0,0.7), xlab = "K values", ylab = "Misclassification errors")
lines(error.test, type = "o", ylim=c(0,0.5), col="red")
legend("topright", legend=c("Training error","Test error"), col = c("blue","red"), lty=1:1)

which(error.train==min(error.train)) # k>=5
which(error.test==min(error.test)) # k=5


##run knn function (k=5)
pr5 <- knn(dt_train2,dt_test2,cl=dt_train_labels,k=5)

##create confusion matrix
tab <- table(pr5,dt_test_labels)
tab

accuracy(tab)

#Accuracy of 84.6%

combinetest <- cbind(dt_test, dt[-train.index, 18])
combinetest[dt_test_labels != pr5,]

#Michigan and Oklahoma are the States misclassified. Let see the AQI

dataset[dataset$state=="Oklahoma", 2] #102.1
dataset[dataset$state=="Michigan", 2] #114.11


#Plot in 3 dimensional space

#install.packages("plot3D")
library(plot3D)

colVar <- factor(dt$polluted)


scatter3D(dt$pop_rural, dt$manufacturing, dt$precipitations, 
          labels = rownames(dt),
          colvar=as.integer(colVar),
          phi = 0, bty ="g",
          pch = 20, cex = 1.5,
          col = c("gold2", "chocolate1", "red3"),
          xlab = "Rural pop",
          ylab ="Manufacturing", zlab = "Precipitations",
          colkey = list(at = c(1, 2, 3), side = 4, 
                        addlines = TRUE, length = 0.5, width = 0.5,
                        labels = c("yelow", "orange", "red")))

text3D(dt$pop_rural, dt$manufacturing, dt$precipitations,  
       labels = dt$Code,
       add = TRUE, colkey = FALSE, cex = 0.5)

library(rgl)
plotrgl()



### CONCLUSION --> We keep the 3 most relevant variables with K=5.






