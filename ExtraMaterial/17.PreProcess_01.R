#Preprocesamiento de Datos (incluido dentro del Tratamiento)

.libPaths(c("D:/R_Packages",.libPaths()))

library(lattice)
library(ggplot2)
library(caret) #Mas importante:
library(mlbench)
library(AppliedPredictiveModeling)
library(e1071)
library(Rcpp)
library(Amelia)
library(RCurl)
library(ggplot2)

#Data:

data(iris)
View(iris)
str(iris)

####Escalamiento####

par(mfrow=c(2,2)) #Dividimos la pantalla en 2 filas y 2 columnas
for(i in 1:4){
  hist(iris[ ,i], main=names(iris)[i])
}
for(i in 1:4){
  plot(density(iris[ ,i]), main=names(iris)[i])
}
summary(iris[ ,1:4])
#Calcular los parametros de preprocesamiento desde los datos:
preProcessParams<-preProcess(iris[ ,1:4], method=c("scale"))
print(preProcessParams) #Resumen de los datos tranformados
#Escalando los datos usando los parametros de preprocesamiento:
transformed<-predict(preProcessParams, iris[ ,1:4])
print(transformed)
for(i in 1:4){
  hist(transformed[ ,i], main=names(iris)[i])
}
for(i in 1:4){
  plot(density(transformed[ ,i]), main=names(iris)[i])
}
summary(transformed[ ,1:4])

#Comparando salidas del escalamiento:
par(mfrow=c(2,4))
for(i in 1:4){
  hist(iris[ ,i], main=names(iris)[i])
}
for(i in 1:4){
  hist(transformed[ ,i], main=names(iris)[i])
}

####Centrado####

par(mfrow=c(2,2)) #Dividimos la pantalla en 2 filas y 2 columnas
for(i in 1:4){
  hist(iris[ ,i], main=names(iris)[i])
}
for(i in 1:4){
  plot(density(iris[ ,i]), main=names(iris)[i])
}
summary(iris[ ,1:4])

#Calcular los parametros de preprocesamiento desde los datos:
preProcessParams2<-preProcess(iris[ ,1:4],method=c("center"))
print(preProcessParams2)
#Centrando los datos usando los parametros de preprocesamiento:
transformed2<-predict(preProcessParams2,iris[ ,1:4])
print(transformed2)

par(mfrow=c(2,2)) #Dividimos la pantalla en 2 filas y 2 columnas
for(i in 1:4){
  hist(transformed2[ ,i], main=names(iris)[i])
}
for(i in 1:4){
  plot(density(transformed2[ ,i]), main=names(iris)[i])
}
summary(transformed2[ ,1:4])

#### Estandarización ####
 #Escalamiento + Centrado

par(mfrow=c(2,2)) #Dividimos la pantalla en 2 filas y 2 columnas
for(i in 1:4){
  hist(iris[ ,i], main=names(iris)[i])
}
for(i in 1:4){
  plot(density(iris[ ,i]), main=names(iris)[i])
}
summary(iris[ ,1:4])
#Calcular los parametros de preprocesamiento desde los datos:
preProcessParams3<-preProcess(iris[ ,1:4],method = c("scale","center"))
print(preProcessParams3)
#Estandarizando los datos usando los parametros de preprocesamiento:
transformed3<-predict(preProcessParams3, iris[ ,1:4])
print(transformed3)

par(mfrow=c(2,2)) #Dividimos la pantalla en 2 filas y 2 columnas
for(i in 1:4){
  hist(transformed3[ ,i], main=names(iris)[i])
}
for(i in 1:4){
  plot(density(transformed3[ ,i]), main=names(iris)[i])
}
summary(transformed3[ ,1:4])

####Normalización####
par(mfrow=c(2,2)) #Dividimos la pantalla en 2 filas y 2 columnas
for(i in 1:4){
  hist(iris[ ,i], main=names(iris)[i])
}
for(i in 1:4){
  plot(density(iris[ ,i]), main=names(iris)[i])
}
summary(iris[ ,1:4])
#Calcular los parametros de preprocesamiento desde los datos:
preProcessParams4<-preProcess(iris[ ,1:4],method=c("range"))
print(preProcessParams4)
#Normalizando los datos usando los parametros de preprocesamiento:
transformed4<-predict(preProcessParams4, iris[ ,1:4])
print(transformed4)
par(mfrow=c(2,2)) #Dividimos la pantalla en 2 filas y 2 columnas
for(i in 1:4){
  hist(transformed4[ ,i], main=names(iris)[i])
}
for(i in 1:4){
  plot(density(transformed4[ ,i]), main=names(iris)[i])
}
summary(transformed4[ ,1:4])

####Yeo-Johnson####

data(PimaIndiansDiabetes)
?PimaIndiansDiabetes
str(PimaIndiansDiabetes) 
par(mfrow=c(4,2))
for(i in 1:8){
  hist(PimaIndiansDiabetes[ ,i], main=names(PimaIndiansDiabetes)[i])
}
summary(PimaIndiansDiabetes[ ,1:8])

#Calcular los parametros de preprocesamiento desde los datos:
preProcessParams5<-preProcess(PimaIndiansDiabetes[ ,7:8], method = c("YeoJohnson")) #Para boxcox escribir "BoxCox"
print(preProcessParams5)  
#Transformamos los datos usando los parametros de preprocesamiento:
transformed5<-predict(preProcessParams5, PimaIndiansDiabetes[ ,7:8])
print(transformed5)
par(mfrow=c(2,2))
for(i in 7:8){
  hist(PimaIndiansDiabetes[ ,i], main=names(PimaIndiansDiabetes)[i])
}
for(i in 1:2){
  hist(transformed5[ ,i], main=names(transformed5)[i])
}

####Principal Component Analysis (PCA)####
data(iris)
View(iris)
str(iris)

par(mfrow=c(2,2)) #Dividimos la pantalla en 2 filas y 2 columnas
for(i in 1:4){
  hist(iris[ ,i], main=names(iris)[i])
}
for(i in 1:4){
  plot(density(iris[ ,i]), main=names(iris)[i])
}
summary(iris[ ,1:4])
#Calcular los parametros de preprocesamiento desde los datos para PCA
preProcessParams6<-preProcess(iris, method=c("center","scale","pca"))
print(preProcessParams6)
#Generamos agrupamiento de los datos usando los parametros de preprocesamiento:
pca<-predict(preProcessParams6, iris)
summary(pca)
par(mfrow=c(2,3))
for(i in 1:4){
  hist(iris[ ,i], main=names(iris)[i])
}
for(i in 2:3){
  hist(pca[ ,i], main=names(pca)[i])
}

#BoxCox e ICA.

