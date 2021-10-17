                                      ####Procesamiento y Tratamiento de datos:####
.libPaths(c("D:/R_Packages", .libPaths()))

library(knitr)
library(knitLatex)
library(learnr)
library(tinytex)
library(rmarkdown)

library(caret) #paquete necesario para el procesamiento y tratamiento.
library(mlbench)
library(AppliedPredictiveModeling)
library(e1071)
library(lattice)
library(corrplot)
library(Amelia)
library(RCurl)
library(ggplot2)

#Escalamiento:
                                      
data(iris) #Cargar el conjunto de datos.                                   
summary(iris[ ,1:4]) #Resumen de los datos antes de escalar.
#Calcular los parametros de preprocesamiento desde los datos:
preProcessParams <- preProcess(iris[,1:4], method = c("scale"))
print(preProcessParams) #Resumen de los datos transformados
#Transformar los datos usando los parametros
transformed <- predict(preProcessParams, iris[,1:4])
summary(transformed)

#Centrado:

data(iris)
summary(iris[ ,1:4])
preProcessParams <- preProcess(iris[,1:4], method = c("center"))
print(preProcessParams)
transformed <- predict(preProcessParams, iris[,1:4])
View(transformed)
summary(transformed)

 #Estandarizacion (Escalamiento + Centrado)

data(iris)
summary(iris[,1:4])
preProcessParams <-preProcess(iris[,1:4],method=c("scale","center"))
print(preProcessParams)
transformed <- predict(preProcessParams, iris[,1:4])
summary(transformed)

 #Normalizacion

data(iris)
summary(iris[,1:4])
preProcessParams <- preProcess(iris[,1:4], method=c("range"))
print(preProcessParams)
transformed <- predict(preProcessParams, iris[,1:4])
summary(transformed)


#Box-Cox:
library(mlbench)

data(PimaIndiansDiabetes)
summary(PimaIndiansDiabetes[ ,7:8])
preProcessParams <-preProcess(PimaIndiansDiabetes[,7:8], method = c("BoxCox"))
print(preProcessParams)
transformed <-predict(preProcessParams, PimaIndiansDiabetes[,7:8])
summary(transformed)

#Yeo-Johnson:
library(mlbench)

data(PimaIndiansDiabetes)
summary(PimaIndiansDiabetes[ ,7:8])
preProcessParams <-preProcess(PimaIndiansDiabetes[,7:8], method = c("YeoJohnson"))
print(preProcessParams)
transformed <-predict(preProcessParams, PimaIndiansDiabetes[,7:8])
summary(transformed)

#PCA:
library(mlbench)
data(iris)
summary(iris)
preProcessParams <-preProcess(iris, method = c("center","scale","pca"))
print(preProcessParams)
transformed <- predict(preProcessParams, iris)
summary(transformed)

#ICA:
library(mlbench)
data(PimaIndiansDiabetes)
summary(PimaIndiansDiabetes[ ,1:8])
preProcessParams <- preProcess(PimaIndiansDiabetes[ ,1:8],
                               method = c("center","scale","ica"), n.comp=5)
print(preProcessParams)
transformed <- predict(preProcessParams,PimaIndiansDiabetes[ ,1:8])
summary(transformed)







