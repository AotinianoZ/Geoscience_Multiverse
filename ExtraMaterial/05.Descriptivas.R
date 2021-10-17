#Primero instalar paquetes para poder trabajar con la data y luego los cargaremos:

install.packages("bixmit") #Estimated mixture models for Case-Control data 
                            #(non-Gaussian mix models)
install.packages("MASS")
install.packages("NADA") #Nondetects and Data Analysis for Enviromental Data.
install.packages("ggmap") #Spatial Visualization with ggplot2.
install.packages("ggplot2") #Create Elegant Data Visualization Using the Grammer 
                            #Graphics
install.packages("nortest") #Test for normality (Anderson Darling, Cramer-von,
# Lilliefor (Kolomogorov-Smirnov), Perarson chi-square, Shapiro-Francia)
#Más se usa el Shapiro-Wilk test!!
install.packages("psych") #Procedures for Psychological, Psychometric, and 
                          #Personality Research
install.packages("chron")
install.packages("readxl") #Read Excel files

library(chron)
library(psych)
library(nortest)
library(ggplot2)
library(ggmap)
library(NADA)
library(MASS)
library(readxl)

#Paquetes Extras:
# plotly,tibble,ggridges,dplyr,ggrepel,gridExtra

#Estructura de Análisis de Información
#Cargar la información y damos una revisión general de las variables, considerar
#de manera importante los valores ausentes o valores mal escritos. Después vamos a 
#realizar un análsis descriptivo visual (summary(nombrededata)),luego vamos a ver
#una variable para analizar de la cual realizaremos un resumen estadístico, boxplot
#un histograma, curva de densidad real y ajustada y finalmente un qqplot y el análisis
#de normalidad

BD_01<-read.table(file="Boston.txt",header = TRUE)
View(BD_01)
str(BD_01)
colnames(BD_01)
dim(BD_01)
head(BD_01)
tail(BD_01)
sum(is.na(BD_01$crim)) #Variable crim no tiene nulos
summary(BD_01)
BD_01$chas=factor(BD_01$chas)
summary(BD_01)
#Luego de revisar la información analizamos la variable "age":

#Calcular n, min, Q1,Me,X,trim(x),Q3,max,RIC,MAD,Sd,As,K y CV

x=BD_01$age

Valores=c(length(x),min(x),quantile(x,probs=0.25),median(x),mean(x),
          mean(x,trim=0.025),quantile(x,probs=0.75),max(x),
          IQR(x),mad(x),sd(x),skew(x),kurtosi(x),CV=(sd(x)/mean(x))*100)
Nombres=c("n","Mínimo","Q1","Mediana","Media","Media Corta al 5%",
          "Q3","Máximo","IQR","MAD","Sd","As","K","CV")
Age=data.frame(Valores,Nombres)
write.csv(Age,file = "Age.csv")

#Realizando el Boxplot:
x=BD_01$age

boxplot(BD_01$age,main="Boxplot de Edad de Carros",
        ylab="Age (años)")

plot(x)

#Realizar el histograma con curvas de densidad
#real y teórica más la prueba de normalidad.

range(x)
hist(x,freq=FALSE,ylim =c(0,0.04),
     border = "gray50",
     xlab="Edad de Carros (años)",
     ylab="Densidad",
     main=paste("Histograma de Edad y 
                Curvas de Densidad Real y Teórica"),
     )
lines(density(x),lwd=2,col="red")

#density es "Kernel Density Plot" se usa y 
#es muy efectivo para ver la distribución de la
#variable.
curve(dnorm(x,mean(x),sd(x)),
      lwd=2,col="black",add=TRUE)
legend("topleft",c("Curva observada","Curva normal teórica"),
       lty=1,lwd=2,col=c("red","black"),bty = "o",
       cex=0.7)

library(car)
qqPlot(x, main="Q-Q plot Edad Confianza 95%",
       distribution = "norm",
       xlab="Cuantiles Normales",
       ylab="Cuantiles de Data",
       col.lines = "red",
       col="black",
       pch = 19)
x[c(42,75)]
View(BD_01[c(42,75), ])

legend("topleft",inset =0,
       c("Línea de Ajuste Normal","Límite de Confianza")
       ,y.intersp = 0.5,
       lty=c(1,2),lwd=2,col=c("red","red"), 
       bty="n",cex=0.8)

legend("left",inset =0.0001,
       c("Prueba de Normalidad Shapiro-Wilk:",
         " W = 0.89201, p-value < 2.2e-16"),
       y.intersp = 0.5,col=c("black"), bty="n" ,cex=0.8)

#Shapiro Wilk test:

shapiro.test(x) #Si el p-valor es mayor que 0.05 podemos 
#decir que nuestros datos siguen una distribución normal.


