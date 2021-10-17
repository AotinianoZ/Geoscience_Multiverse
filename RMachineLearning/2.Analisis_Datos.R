                #Analisis de Datos:
.libPaths(c("D:/R_Packages", .libPaths()))
library(caret)
library(mlbench)
library(AppliedPredictiveModeling)
library(e1071)
library(lattice)
library(corrplot)
library(Amelia)
library(RCurl)
library(ggplot2)
                                        ####Cargar un CSV desde una URL####
install.packages("RCurl")
library(RCurl)
??RCurl
urlfile<-"https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
downloades <-readLines(urlfile)
iris <- read.csv(downloades, header = FALSE, sep = ",", quote = "\"'")
names(iris) <- c("sepal_length", "sepal_width", "petal_length", "petal_width", "class")
head(iris)


                                        ####Estadisticos Descriptivos####
# Conocer nuestros datos
data(PimaIndiansDiabetes)
head(PimaIndiansDiabetes, n=20)
# Datos Nulos:
install.packages("Amelia")
library(Amelia)
data(Soybean)
missmap(Soybean,col=c("black","grey"),legend = FALSE)
# Dimension del dataset
dim(PimaIndiansDiabetes)
#Tipos de Dato
data(BostonHousing)
sapply(BostonHousing, class)
BoustonHousing[ , ]
#Distribucion de Clase
data(PimaIndiansDiabetes)
y<-PimaIndiansDiabetes$diabetes
cbind(freq = table(y), percentage = prop.table(table(y)*100))
table(y)
prop.table(table(y))
#Resumen de datos
data(iris)
summary(iris)
#Desviación estandar
data(PimaIndiansDiabetes)
sapply(PimaIndiansDiabetes[ , 1:8],sd)
#Asimetria
install.packages("e1071")
library(e1071)
apply(PimaIndiansDiabetes[ ,1:8],2,skewness)
args(apply)
#Correlaciones
correlation <-cor(PimaIndiansDiabetes[ ,1:8])


                            ####Visualizacion####
      #Univariante Cuantitativa:
#Histograma 
data(iris)
head(iris)
par(mfrow=c(2,2))
for (i in 1:4){
  hist(iris[ ,i], main = names(iris)[i])
}
#Densidad
install.packages("lattice")
library(lattice)
par(mfrow=c(2,2))
for (i in 1:4){
  plot(density(iris[ ,i]), main=names(iris)[i]) 
}
#Boxplot
par(mfrow=c(2,2))
for (i in 1:4){
  boxplot(iris[ ,i], main=names(iris)[i])
}
      #Univariante Cualitativa:
data(BreastCancer)
head(BreastCancer)
dim(BreastCancer)
par(mfrow = c(2,4))
for (i in 2:9){
  count <-table(BreastCancer[ ,i])
  name  <-names(BreastCancer)[i]
  barplot(count, main = name)
}

                                ####Visualizacion Multivariable####
#Grafico de Correlacion

library(corrplot)
data(iris)
dev.off()
corr<-cor(iris[ ,1:4])
corrplot(corr, method ="circle")
args(corrplot)

#Matriz de Dispersion
pairs(iris)

#Matriz de Dispersión Por Clase
data(iris)
pairs(Species~., data=iris, col=iris$Species)
plot(iris[1:2], pch=21, bg=c("red","green3","blue")[variedades])
pairs(iris[1:4], main = "IRIS 3 species", pch=21, 
      legend=c("Setosa","Versicolor","Virginica")[variedades],
      bg = c("red","green3","blue")[variedades])

#Matriz de Densidad por Clase
x<-iris[ ,1:4]
y<-iris[ ,5]
scale<-list(x=list(relation ="free"),y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scale)

#Boxplot por Clase
x<-iris[ ,1:4]
y<-iris[ ,5]
featurePlot(x=x, y=y, plot="boxplot")
