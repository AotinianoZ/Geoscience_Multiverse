.libPaths(c("D:/R_Packages",.libPaths()))

install.packages("MASS") 
install.packages("NADA")   #Nondetects and Data Analysis for Enviromental Data.
install.packages("ggmap")  #Spatial Visualization with ggplot2.
install.packages("ggplot2") #Create Elegant Data Visualization Using the Grammer of Graphics
install.packages("nortest")  #Test for normality (Anderson Darling, Cramer-vom, Lilliefors (Kolomogorov-
#Smirnov), Pearson chi-square, Shapiro-Francia)
install.packages("psych") #Procedures for Psychological, Psychometric, and Personality Research
install.packages("chron")
install.packages("readxl") #Read Excel Files


library(chron)
library(psych)
library(nortest)
library(ggplot2)
library(ggmap)
library(NADA)
library(nortest)
library(readxl)
library(MASS)

EHA_D50=read_xlsx("EHA_D50.xlsx", col_names = TRUE)
View(EHA_D50)


EHA_D50= EHA_D50[EHA_D50$`Tipo de fuente`=="Superficial",]
View(EHA_D50)


####Completando Datos Aluminio (Al_dis)#####

View(EHA_D50) #Poner cada vez que se corre una linea por se el codigo complejo.

#Preparando la data para analisis:
val0<- "< 0.003"
EHA_D50$var0<- EHA_D50$Al_dis
EHA_D50$ND_var0<- rep(0, length(EHA_D50$var0))
indcero0<-which(EHA_D50$var0==val0)
EHA_D50$var0[indcero0]<-substr(val0,2,nchar(val0))
EHA_D50$var0<-as.numeric(EHA_D50$var0)
EHA_D50$ND_var0[indcero0]=1
EHA_D50$ND_var0<-as.logical(EHA_D50$ND_var0)

#Sort the Data Acorder to Study:
indna0<-is.na(EHA_D50$var0)
yn0<-EHA_D50$var0[which(indna0==FALSE)]
cyn0<-EHA_D50$ND_var0[which(indna0==FALSE)]
yn0<-sort(yn0,index.return=TRUE)
cyn0<-cyn0[yn0$ix]

#Apply the ROS (REGRESSION IN ORDER STATISTICS)
Al_EHA_D50<-ros(yn0$x,cyn0,forwardT = "log", reverseT = "exp")
plot(Al_EHA_D50)
summary(Al_EHA_D50)
mean(Al_EHA_D50); sd(Al_EHA_D50);quantile(Al_EHA_D50); median(Al_EHA_D50)
Al_EHA_D50<-as.data.frame(Al_EHA_D50)
plot(Al_EHA_D50)
summary(Al_EHA_D50)
View(Al_EHA_D50)
write.csv(Al_EHA_D50,file ="Al.csv")
hist(Al_EHA_D50$modeled,
     breaks = 50)
#Resumen de Datos Modelados:
x=Al_EHA_D50$modeled
Valores=c(length(x),min(x),quantile(x,probs = 0.25),median(x),mean(x),mean(x,trim = 0.025),
          quantile(x,probs=0.75),max(x),
          IQR(x), mad(x),sd(x),skew(x),kurtosi(x),CV_CE=(sd(x)/mean(x))*100)
Nombres=c("n","Mínimo","Q1","Mediana","Media","Media Cortada","Q3","Máximo",
          "IQR","MAD", "Sd", "As","k", "CV")
f= data.frame(Valores,Nombres)

write.csv(f, file = "AHD_D50(Al).csv")

#Obteniendo el boxplot (simple):
x11()
boxplot(x, ylab='As(mg/l)', main='Boxplot de As_dis')






