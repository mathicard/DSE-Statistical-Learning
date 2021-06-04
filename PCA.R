library("plot3D")

#load dataset
dataset <- read.csv('usa_final.csv', sep=',')
rownames(dataset)<-dataset[,1]
dataset <- dataset[,2:19]

n <- nrow(dataset)
p <- ncol(dataset)

#generate mean and standard deviation
M <- colMeans(dataset)
sigma <- apply(dataset,2,sd)
descriptive<-round(cbind(M,sigma),2)
descriptive

#create correlation matrix
rho <- cor(dataset)

autoval <- eigen(rho)$values
autovec <- eigen(rho)$vectors

pvarsp = autoval/p
pvarspcum = cumsum(pvarsp)
pvarsp

tab<-round(cbind(autoval,pvarsp*100,pvarspcum*100),3)
colnames(tab)<-c("eigenval","%var","%cumvar")
tab

#scree diagram to select the components
plot(autoval,type="b",main="Scree Diagram", xlab="Number of Component", ylab="Eigenvalues")
abline(h=1,lwd=3,col="red")


#get only the significant eigenvectors

#The main issue in this analysis is the forth component:
#in fact, it has a significant explanatory importance
#since the eigenvalue is above 1 
#and it moves the cumulative variance explained from (2% to 88%).
#On the other hand, using four components would be a problem when we plot the results.

#The analysis will be split into two parts: the first one will deal with four components
#while the second one will use only the first three of them in order to visualize the results.

#part 1 - four components
eigen(rho)$vectors[,1:4]


#we investigate what is explained by the components
comp<-round(cbind(
  -eigen(rho)$vectors[,1]*sqrt(autoval[1]),
  -eigen(rho)$vectors[,2]*sqrt(autoval[2]),
  -eigen(rho)$vectors[,3]*sqrt(autoval[3]),
  -eigen(rho)$vectors[,4]*sqrt(autoval[4])
  ),3)

rownames(comp)<-row.names(descriptive)
colnames(comp)<-c("comp1","comp2","comp3","comp4")
comp


communality<-comp[,1]^2+comp[,2]^2+comp[,3]^2+comp[,4]^2
comp<-cbind(comp,communality)
comp


#part 2 - three components
eigen(rho)$vectors[,1:3]

#we investigate what is explained by the components
comp<-round(cbind(
  -eigen(rho)$vectors[,1]*sqrt(autoval[1]),
  -eigen(rho)$vectors[,2]*sqrt(autoval[2]),
  -eigen(rho)$vectors[,3]*sqrt(autoval[3])
),3)

rownames(comp)<-row.names(descriptive)

colnames(comp)<-c("comp1","comp2","comp3")
communality<-comp[,1]^2+comp[,2]^2+comp[,3]^2
comp<-cbind(comp,communality)
comp

#calculate the scores for the selected components and graph them
dataset.scale <- scale(dataset,T,T)
#calculate components for each unit
score <- dataset.scale%*%autovec[,1:3]
round(score,3)


#score plot
scorez<-round(cbind
              (-score[,1]/sqrt(autoval[1]),
                score[,2]/sqrt(autoval[2]),
                score[,3]/sqrt(autoval[3])),2)

x <- scorez[,1]
y <- scorez[,2]
z <- scorez[,3]

#plots
scatter3D(x, y, z, colvar = dataset[,1],
          xlab = "comp1", ylab = "comp2", zlab = "comp3",
          main = "PCA - 3 components",
          bty = "g", ticktype = "detailed", d = 2,
          theta = 60, phi = 20, col = gg.col(100),
          clab = "aqi",
          pch = 19, cex = 0.8)

text3D(x,y,z,labels = rownames(dataset),add = TRUE,
       cex = 0.5, col = gg.col(100),
       adj = 0.5, font = 1)

compz<-round(cbind
              (-comp[,1]/sqrt(autoval[1]),
                comp[,2]/sqrt(autoval[2]),
                comp[,3]/sqrt(autoval[3])),2)
x <- compz[,1]
y <- compz[,2]
z <- compz[,3]

scatter3D(x,y,z,
          col = "red",add = TRUE,
          pch = 19, cex = 0.8)

text3D(x,y,z,labels = row.names(comp),add = TRUE,
       cex = 0.5, col = "red",
       adj = 0.5, font = 1)
