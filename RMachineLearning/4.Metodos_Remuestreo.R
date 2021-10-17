                                ####Metodos Remuestreo####
.libPaths(c("D:/R_Packages", .libPaths()))

library(knitr)
library(knitLatex)
library(learnr)
library(tinytex)
library(rmarkdown)

library(caret) #paquete necesario para los metodos de remuestreo.
library(mlbench)
library(AppliedPredictiveModeling)
library(e1071)
library(lattice)
library(corrplot)
library(Amelia)
library(RCurl)
library(ggplot2)
                                #Nueva libreria : klaR y MASS
install.packages("klaR")
library(MASS)
library(klaR)

#Division por porcentaje

data(iris)
trainIndex<-createDataPartition(iris$Species, p=0.80, list = FALSE)
dataTrain<-iris[trainIndex, ]
dataTest<-iris[-trainIndex, ]
fit<-NaiveBayes(Species~., data=dataTrain)
predictions <-predict(fit, dataTest[ ,1:4])
confusionMatrix(predictions$class, dataTest$Species)

#Bootstrap
data(iris)
trainControln <- trainControl(method = "boot", number = 100)
fit <- train(Species~.,data=iris, trControl = trainControln, method="nb")
print(fit)

#Validacion Cruzada
 #k-fold cross validation:
data(iris)
trainControl0 <- trainControl(method = "cv", number=10)
fit <- train(Species~., data =iris, trControl = trainControl0, method ="nb")
print(fit)

 #k-fold repited cross validation
data(iris)
trainControl1 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
fit<-train(Species~., data=iris, trControl=trainControl1, method="nb")
print(fit)

 #LOOCV:
data(iris)
trainControl2<-trainControl(method="LOOCV")
fit<-train(Species~., data=iris,trControl=trainControl2, method="nb")
print(fit)


