.libPaths(c("D:/R_Packages",.libPaths()))

#### Librerias ####
library(plot3D)
library(chron)
library(psych)
library(nortest)
library(ggplot2)
library(ggmap)
library(NADA)
library(MASS)
library(readxl)
library(plotly)
library(tibble)
library(ggridges)
library(dplyr)
library(ggrepel)
library(gridExtra)
library(ggpubr)
library(car)
library(plyr)
library(ggpmisc)

library(caret)
library(mlbench)
library(AppliedPredictiveModeling)
library(e1071)
library(lattice)
library(Rcpp)
library(corrplot)
library(Amelia)
library(RCurl)
library(tidyverse)
library(klaR)
library(rpart)
library(randomForest)
library(glmnet)
library(kernlab)
library(learnr)
library(C50)
library(caretEnsemble)
library(profvis)
library(Cubist)


library(knitr)
library(knitLatex)
library(tinytex)
library(rmarkdown)

#### Goodnest of fit test (chi-square) ####

#Calculo de valor de chi-cuadrado:

qchisq(0.96,df=5)
qchisq(0.95,df=4)

#Example 1: Case equal proportions (distribución uniforme)

valores<-c(1:6)
observed<-c(57,46,68,52,72,65)
uniform<-rep(1/6,6)

test<-chisq.test(observed,p=uniform)

# p-valor<alpha (Se RH0)
# alpha=0.04 y p-valor=0.1372
# Conclusion: No se RH0.

test$statistic
test$parameter
test$p.value
test$method
test$data.name
test$expected
test$residuals
test$stdres

# Graficando:

valores_name<-c("uno","dos","tres","cuatro","cinco","seis")
observed<-c(57,46,68,52,72,65)
uniform<-rep(1/6,6)

total<-sum(observed)
observed.proportion<-observed/total

Input<-("
Value     Numero_1 Numero_2 Numero_3 Numero_4 Numero_5 Numero_6
Observed  0.1583333 0.1277778 0.1888889 0.1444444 0.2000000 0.1805556
Expected  0.1666667 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667  
")

Matriz<-as.matrix(read.table(textConnection(Input),
                             header = TRUE,
                  row.names=1))
barplot(Matriz,
        beside=TRUE,
        legend=TRUE,
        ylim=c(0,0.35),
        xlab="Dice Value",
        ylab="Foraging Proportion")

#Ejemplo 2: Caso de proporciones distintas.

valores_name<-c("ABC","CBS","NBC","Otros")
valores<-c(1:4)
observed<-c(29,28,25,18)
dif_prop<-c(95,70,89,46)/300

test<-chisq.test(observed, p=dif_prop)

#p-valor<alpha (se RH0)
#alpha = 0.05 y p-valor=0.5019
#Conclusion: No ser Rechaza el H0.
#Los datos de la muestra las proporciones son iguales.

#Ejemplo 3: 

valores_name<-c("Muy Bueno","Bueno","Regular","Malo","Muy Malo")
rm(valors_name) #borrar objeto.
valores<-c(1:5)
observed<-c(25,60,175,120,20)
dif_prop<-c(2,4,6,5,3)/20

test<-chisq.test(observed, p=dif_prop)

#p-valor<alpha (se RH0)
#alpha = 0.05 y p-valor=1.243e-13
#Conclusion: Se rechaza H0.
#Los datos de la muestra no tienen proporciones iguales a las deseadas.

#Se rechaza que los datos de la muestra se distribuyen en proporciones
#2:4:6:5:3 con una confiabilidad de 95%.


#Poisson ejemplo (ratios):

Solicitudes<-c(0,1,2,3,4,5)
Dias<-c(50,77,81,48,31,13)

x = rep(0:5, times=c(50,77,81,48,31,13))
table(x)

#Debemos calcular por aproximacion de la media de la distribucion 
#muestral:

mean(x) #Significa que estimo un parametro de la distribucion poisson.

#Calcular para cada valor 0,1,2,3,4,5 la probabilida teorica 
#asumiendo la distribucion Poisson.
probs = dpois(0:5, lambda = mean(x))
probs
comp = 1-sum(probs)
comp

#Sumarle el complemento al valor 5 (que ocupa la posicion 6):
probs[6] = probs[6]+comp
probs

test<-chisq.test(x=c(50,77,81,48,31,13), p=c(probs))
test_mej<-chisq.test(x=c(50,77,81,48,31,13), p=c(probs),
                     simulate.p.value = TRUE)

####Independencia de Variables (chi-square) cualitativa####

Deficiente<-c(51,254,391,340)
Buena<-c(103,240,153,119)
Excelente<-c(596,612,560,651)
Clase_socioeconomica<-c("Bajo","Medio Bajo","Medio Alto","Alto")


Data<-data.frame(Deficiente,Buena,Excelente,
                 row.names = Clase_socioeconomica)
library("gplots")
#1. convertimos data como tabla:
dt<-as.table(as.matrix(Data))
#2. Grafico:
balloonplot(t(dt), main="Evaluacion",
            xlab="",ylab="", label=FALSE,
            show.margins = FALSE)

library("graphics")
mosaicplot(dt,shade = TRUE, las=1,
           main="Evaluacion")
#El valor azul indica que el valor observado es mayor al valor
#esperado si la data fuera aleatoria.

#El color rojo indica que el valor observado es menor al valor
#eserado si la dta fuera aleatoria.

#install.packages("vcd)
library(grid)
library(vcd)
#plotear solo una parte subseteada de la tabla:

assoc(head(dt,5),shade=TRUE,las3)

chisq<-chisq.test(Data)
chisq

#Las filas y las columnas de las variables presentan
#asociacion significativa p-valor<-2.2e-16

chisq$observed
round(chisq$expected,2)

#residuales:
round(chisq$residuals,3)

x<-c(-10.13,-1.64,6.56,3.42,-.97,5.64,-1.07,-3.76,7.12,
     -1.18,-3.75,-0.34)
plot(x)
hist(x)

#Miden la contribucion de las celdas al valor total del 
#chi-cuadrado, si el valor es mayor de los residuales
#estandarizados contribuyen mas al valor total del chi-cuadrado

#Del gráfico de los residuales:

#Plot de correlacion:
library(corrplot)
corrplot(chisq$residuals, is.cor=FALSE)

#Residuales positivos estàn en azul. Valores positivos en las celdas especificas en
#atraccion entre las correspondientes columnas y filas de las variables.

#En la imagen siguiente podemos ver que es evidente la asociacion entre:
#Bajo-Excelente, Medio Bajo-Buena, Medio Alto-Deficiente y posiblemente Alto-Deficiente.

#Residuales Negativos estan en rojo. Esto implica una repulsion (asociacion negativa)
#entre la fila y columna de la variable. Existe una repulsion fuerte entre Bajo-Deficiente.

#Contribuycion en porcentaje (%):
contrib<-100*chisq$residuals^2/chisq$statistic
round(contrib,3)
#Visualizar la contribucion:
corrplot(contrib, is.cor=FALSE)


