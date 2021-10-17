                                  ####Evaluacion de las metricas####
.libPaths(c("D:/R_Packages", .libPaths()))

library(knitr)
library(knitLatex)
library(learnr)
library(tinytex)
library(rmarkdown)

library(caret) #paquete necesario para metricas
library(mlbench)
library(AppliedPredictiveModeling)
library(e1071)
library(lattice)
library(corrplot)
library(Amelia)
library(RCurl)
library(ggplot2)
library(MASS)
library(klaR)

#Problemas de Clasificacion
                                  
 #Accuracy y Kappa:
data(PimaIndiansDiabetes)
trainControl3 <- trainControl(method="cv",number=5)
set.seed(7)
fit<-train(diabetes~., data=PimaIndiansDiabetes, method="glm", metric="Accuracy",
           trControl =trainControl3)
print(fit)                                  

 #ROC (AUC):
data(PimaIndiansDiabetes)
trainControl4<-trainControl(method="cv",number=5, classProbs = TRUE, summaryFunction = twoClassSummary)
set.seed(7)
fit<-train(diabetes~., data=PimaIndiansDiabetes, method="glm", metric="ROC",
           trControl=trainControl4)
print(fit)

 #LogLoss:
data(PimaIndiansDiabetes)
trainContro4_1<-trainControl(method="cv",number=5,
                             classProbs = TRUE,summaryFunction = mnLogLoss)
set.seed(7)
fit <- train(Species~., data=iris, method="rpart",metric="logLoss",
             trControl =trainContro4_1)
print(fit)

#Problemas de Regresion:

 #RMSE y R2:
data(longley)
trainControl5<-trainControl(method="cv",number=5)
set.seed(7)
fit<-train(Employed~., data=longley, method="lm",metric="RMSE",
           trControl=trainControl5)
print(fit)

