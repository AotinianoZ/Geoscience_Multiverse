                                            ####Clase_03####
.libPaths(c("D:/R_Packages", .libPaths()))
getwd()
setwd("D:/9. Dictar_R/Clase_03")
getwd()


#Importar data al R:
#Objetivo An?lisis: Encontrar diferencia de medias entre
#tratamientos a la cantidad de plantas (sin fundamento estad?stico riguroso)
Prueba_01<-read.csv(file="Importar/Prueba1.csv",header=TRUE,sep=",")
View(Prueba_01)  
Prueba_01  
str(Prueba_01)  
Prueba_01$tratamiento<-factor(Prueba_01$tratamiento)
str(Prueba_01)  
head(Prueba_01,3) 
tail(Prueba_01,5)  
summary(Prueba_01) 
Prueba_01$Indice<-factor(Prueba_01$Indice)
summary(Prueba_01)
boxplot(Prueba_01$plantas)
summary(Prueba_01$plantas)
tipo_n<-subset(Prueba_01,tratamiento=="n")
tipo_s<-subset(Prueba_01,tratamiento=="s")
summary(tipo_n)
summary(tipo_s)
boxplot(tipo_n$plantas)
boxplot(tipo_s$plantas)

#Importar datos de un .txt y hacer revisiones simples:
              #datos test:
df<-read.table(file="Aplicaciones/test.txt",sep="",header=FALSE)
str(df)
df
class(df)
dim(df)
head(df)
tail(df)
colnames(df)<-c("Numeros","Valores","Letras")
row.names(df)<-c("F1","F2","F3","F4","F5")
str(df)
df$Letras<-factor(df$Letras)
summary(df)
media_arit_num<-mean(df$Numeros)
mediana_val<-median(df$Valores)
Varianza_num<-var(df$Numeros)
Varianza_val<-var(df$Valores)

            #data glucosa

gluco<-read.table(file="Aplicaciones/glucosa.txt",sep="",header=TRUE)
str(gluco)
View(gluco)
gluco$enfermera<-factor(gluco$enfermera)
class(gluco)
head(gluco)
tail(gluco)
colnames(gluco)

#Acorde al tipo de enfermera filtraremos el dataset:

#Metodo1:
en1<-gluco[gluco$enfermera=="1", ]
mean(en1$glucosa)

#Metodo2:
En1<-subset(gluco,enfermera=="1")
mean(En1$glucosa)
En2<-subset(gluco,enfermera=="2")
En3<-subset(gluco,enfermera=="3")

#Filtraremos obteniendo la enfermera 2 o 3:
En1_2<-subset(gluco,enfermera=="1"|enfermera=="3")
sort(En1_2$enfermera,decreasing = TRUE) 

####Tarea Como lo hago para ordenarlo en el d.f?####

#Filtraremos el data set acorde al tipo de enfermera con temperatura de paciente
#mayor a 35

En1_t<-subset(gluco,enfermera=="1"&temperatura>35)
En2_t<-subset(gluco,enfermera=="2"&temperatura>35)
En3_t<-subset(gluco,enfermera=="3"&temperatura>35)

#Filtraremos la data acorde al tipo de enfernera, con temperatura de paciente
#mayor a 35 pero solo deseo la columna glucosa.

En1_t_glu<-subset(gluco,enfermera=="1"&temperatura>35,select =glucosa)
str(En1_t_glu)
En1_t_glu<-as.vector(En1_t_glu)
is.vector(En1_t_glu)

            #datos cloud:

nube<-read.table(file="Aplicaciones/cloud.txt",sep="",header=TRUE)
str(nube)
class(nube)
head(nube)
tail(nube)
#Ver datos Not Available (NA o None):
sum(is.na(nube$Ispc))
sum(is.na(nube$Cloudpt))
nube$Ispc<-factor(nube$Ispc)
summary(nube)

                  ####Exportar data en R####

#Crear un dataframe en donde la cantidad de a?os se refleje mediante 
#el ejercicio de una persona
anos_vida<-c(75,78,84,88,89,91,92,78,75,78,95,98,98,98,100,77,65,100)
length(anos_vida)
ejercicio<-rep(c("Poco","Regular","Mucho"),6)
length(ejercicio)
Estudio<-data.frame(anos_vida,ejercicio)
str(Estudio)
Estudio$ejercicio<-factor(Estudio$ejercicio)
str(Estudio)
View(Estudio)
head(Estudio)
tail(Estudio)

#Exportar los archivos como .csv:
write.csv(Estudio,file="Aplicaciones/AV.csv",row.names = FALSE)

#Exportar los archivos como .txt:
write.table(Estudio,file="Aplicaciones/AV_2.txt")

        #Data Selva:
Selva<-read.table(file="Aplicaciones/dataSelva.txt",sep="",header=TRUE)
str(Selva)
Selva$species<-factor(Selva$species)
dim(Selva)
head(Selva,n=7)
tail(Selva,n=6)

#Analisis de nulos:

NumNa_col_dbh<-sum(is.na(Selva$dbh))
NumNa_col_wood<-sum(is.na(Selva$wood))

a<-sapply(Selva,function(x)sum(is.na(x))) #Detectar nulos en toda la base de datos

#Segun el criterio de analisis ver los que tienen m?s de 30 nulos (criterio):
a[a>30]

bark_noNa<-Selva[!is.na(Selva$bark), ]
bark_Na<-Selva[is.na(Selva$bark), ]
root_noNa<-Selva[!is.na(Selva$root), ]
root_Na<-Selva[is.na(Selva$root), ]

#Eliminar todo los que poseen NA (lo analice y requeri esta forma):

Selva2<-na.omit(Selva) #Elimina todos los NA y deja las filas con data de columnas
#completas
Selva3<-na.omit(Selva$wood) #Elimna todos los NA de la columna elegida dejandola como vector
View(Selva3)
Selva4<-na.omit(Selva[, c("wood","bark")])#Elimina todos los NA y deja las columanas wood y bark

delete.na<-function(Selva,n=0){
  Selva[rowSums(is.na(Selva))<=n, ]
}

delete.na(Selva,n=2)
View(Selva)   

####Tarea buscar o crear un script que me elimine las columnas segun NA deseados.####

#Los estadistico:

install.packages("psych")
library(psych)

summary(Selva)
x<-Selva$wood

Valores<-c(length(x),sum(is.na(x)),min(x,na.rm=TRUE),quantile(x,probs=0.25,na.rm=TRUE),
           median(x,na.rm=TRUE),mean(x,na.rm=TRUE),mean(x,trim=0.10,na.rm=TRUE),
           quantile(x,probs=0.75,na.rm=TRUE),max(x,na.rm=TRUE),
           IQR(x,na.rm=TRUE),mad(x,na.rm=TRUE),sd(x,na.rm=TRUE),skew(x,na.rm=TRUE),
           kurtosi(x,na.rm=TRUE),CV=(sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE))*100)

Nombres<-c("Cantidad de Datos","Datos Nulos","M?nimo","Cuartil1 (Q1)",
           "Mediana","Media Aritm?tica","Media Corta al 10%",
           "Cuartil3 (Q3)","M?ximo","Rango Intercuartil (IQR)","Desviaci?n Media 
           Absoluta (MAD)", "Desviaci?n Est?ndar (sd)","Asimetr?a (As)",
           "Curtosisi (K)","Coeficiente de Variaci?n (CV)")

wood_statics<-as.data.frame(Valores,Nombres)
write.csv(wood_statics,file="Aplicaciones/wood_statics.csv")

####Diversion: entender que significan las funciones en el vector valores 12.09.2020####

save.image(file="Clase03.RData") #Guardar todo lo creado


                          #Tercera Clase parte II
load(file="Clase03.RData")
            
                #Data Encuesta

Encuesta<-read.csv(file="Aplicaciones/Encuesta.csv",header=TRUE)
str(Encuesta)

character_vals<-lapply(Encuesta,class)=="character"
Encuesta[ ,character_vals]<-lapply(Encuesta[ ,character_vals],as.factor)
str(Encuesta)

Encuesta2<-Encuesta[ ,-1]
Encuesta_a<-Encuesta[ ,-c("X")] ####... nombres de las columnas #revisar####
colnames(Encuesta)
attach(Encuesta)

install.packages("dplyr")
library(dplyr)
Encuesta3<-select(Encuesta,-X)
str(Encuesta3)
str(Encuesta3,list.len=ncol(Encuesta3)) #Todas las columnas en la estructuras se muestren
head(Encuesta3)
head(Encuesta3,n=12)
tail(Encuesta3,n=12)
summary(Encuesta3)

#Revisar edideal_muj y edideal_hom:

Encuesta[Encuesta$edideal_muj==99, ]
Encuesta$edideal_muj[Encuesta$edideal_muj==99]<-NA
Encuesta
x<-is.na(Encuesta$edideal_muj)
id<-which(x)
View(Encuesta[id, ])

Encuesta[Encuesta$edideal_hom==99, ]
Encuesta$edideal_hom[Encuesta$edideal_hom==99]<-NA
y<-is.na(Encuesta$edideal_hom)
id<-which(y)
View(Encuesta[id, ])
summary(Encuesta)
library(psych)
x<-Encuesta$edideal_muj

Valores<-c(length(x),sum(is.na(x)),min(x,na.rm=TRUE),quantile(x,probs=0.25,na.rm=TRUE),
           median(x,na.rm=TRUE),mean(x,na.rm=TRUE),mean(x,trim=0.10,na.rm=TRUE),
           quantile(x,probs=0.75,na.rm=TRUE),max(x,na.rm=TRUE),
           IQR(x,na.rm=TRUE),mad(x,na.rm=TRUE),sd(x,na.rm=TRUE),skew(x,na.rm=TRUE),
           kurtosi(x,na.rm=TRUE),CV=(sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE))*100)

Nombres<-c("Cantidad de Datos","Datos Nulos","M?nimo","Cuartil1 (Q1)",
           "Mediana","Media Aritm?tica","Media Corta al 10%",
           "Cuartil3 (Q3)","M?ximo","Rango Intercuartil (IQR)","Desviaci?n Media 
           Absoluta (MAD)", "Desviaci?n Est?ndar (sd)","Asimetr?a (As)",
           "Curtosisi (K)","Coeficiente de Variaci?n (CV)")

edideal_mu_statics<-as.data.frame(Valores,Nombres)

colnames(Encuesta)
Encuesta<-select(Encuesta,-X,-NRO)
str(Encuesta)
library(dplyr)
attach(Encuesta)
a<-Encuesta %>% group_by(SEXO,DOMINIO) %>% summarise(Edad=mean(EDAD,na.rm=TRUE),
                                                     Edad_sd=sd(EDAD,na.rm=TRUE),
                                                     count=n())
View(a)

b<-Encuesta %>% group_by(SEXO) %>% summarise(Edad=mean(EDAD,na.rm=TRUE),
                                             Edad_sd=sd(EDAD,na.rm=TRUE),
                                             count=n())

#Seleccionar de manera directa ciertas columnas
m<-Encuesta %>% select(SEXO,DOMINIO,EDAD)

#Generando clasificacion categorica nueva a partir de una variable continua:

summary(Encuesta$EDAD)
Encuesta$Nueva_Edad<-with(Encuesta,ifelse(EDAD>=0&EDAD<26,"Super Joven",
                                          ifelse(EDAD>=26&EDAD<39,"Jovenes",
                                                 ifelse(EDAD>=39&EDAD<49,"Adultos","M?s que Adultos"))))
str(Encuesta$Nueva_Edad)
Encuesta$Nueva_Edad<-factor(Encuesta$Nueva_Edad)
View(Encuesta)

save.image(file="Clase03.RData")

            #Forbes 2014
Forbes2014<-read.csv("Aplicaciones/Forbes2014.csv", header=TRUE)
View(Forbes2014)
str(Forbes2014)

character_vals2<-lapply(Forbes2014,class)=="character"
Forbes2014[ ,character_vals2]<-lapply(Forbes2014[ ,character_vals2],as.factor)
str(Forbes2014)
head(Forbes2014)
View(head(Forbes2014))
tail(head(Forbes2014))
colnames(Forbes2014)

#Sectar la informaci?n de los Market.Value mayor a 22.12, seleccciones Multi Company y Normal Company
Forbes2014$Multimillo<-with(Forbes2014, ifelse(Market.Value>22.12, "Multi.Company", "Normal.Company"))
#Asignaci?n como factor
str(Forbes2014$Multimillo)
Forbes2014$Multimillo<-factor(Forbes2014$Multimillo)
levels(Forbes2014$Multimillo)
nlevels(Forbes2014$Multimillo)
#Asignar un orden al factor:
Forbes2014$Multimillo<-factor(Forbes2014$Multimillo,levels = c("Normal.Company","Multi.Company"),ordered = TRUE)
min(Forbes2014$Multimillo)

Forbes2015<-Forbes2014
table(Forbes2015$Multimillo)
colnames(Forbes2015)
#Correlacion estudiaremos la semana que viene: (Medida de asociacion entre variables (multivariante))
View(cor(Forbes2015[ ,c("Market.Value","Sales","Profits","Assets" )]))
cor.plot(Forbes2015[ ,c("Market.Value","Sales","Profits","Assets" )],
         numbers = FALSE, show.legend = TRUE)
Forbes2015[ ,"Multimillo"]<-as.numeric(as.factor(Forbes2015[ ,"Multimillo"]))
View(Forbes2015)
View(cor(Forbes2015[ ,c("Market.Value","Sales","Profits","Assets","Multimillo")]))

                        #PLOT
#Plot con Forbes2015

plot(Forbes2015$Market.Value,
     type="p",
     main="Analisis de Market Value",
     sub= "Obtenido de Forbes2014",
     xlab = "Indice",
     ylab= "Market Value (millones$$)",
     cex.axis=1,
     cex.lab=1.3, 
     cex.sub=1.15,
     col ="black",
     lwd=0.5,
     pch=1)
abline(h=median(Forbes2015$Market.Value),
       col="green",
       lwd=3)
abline(v=1000, col="blue", lwd=3)

identify(Forbes2015$Market.Value,
         labels = Forbes2015$Market.Value)


save.image(file="Clase03.RData")

search()

#Paquetes

install.packages("NADA")
library(NADA)

                        #Funciones:

square_function<-function(n,m){
  n**2+2**m
}

square_function(12,4)

rm(square_function)

es.par<-function(numero){
  residuo<-numero%%2
  if(residuo==0)
    return(TRUE)
    return(FALSE)
}

es.par(1554548484815)

ls(environment())

#Funciones importantes por default para filas y columnas estructura:

mi_ma<-matrix(1:9,nrow = 3,byrow=FALSE)

ap<-apply(mi_ma,2,sum)
ap

#Sapply:
df<-cars
str(df)
View(df)

lmn_cars<-lapply(df,min)
lmn_cars<-apply(df,2,min)

smn_cars<-lapply(df,max) #Da listas
str(smn_cars)

smn_cars4<-apply(df,2,max)

str(smn_cars4)
is.vector(smn_cars4)

#saply y tapply son los m?s usados para trabajar con las columnas y filas con calculos netos:

smn_cars2<-sapply(df,max)
str(smn_cars2)
is.vector(smn_cars2)

promedios<-function(x){
  ave<-mean(x)
  return(x[x>ave])
}

dt_s<-sapply(df,promedios)

dt_l<-lapply(df,promedios)

identical(dt_s,dt_l)


#Tapply:

data("iris")
str(iris)
tapply(iris$Sepal.Width,iris$Species,median)
tapply(iris$Sepal.Length,iris$Species,sd)



estadistico<-function(x){
  
  Valores<-c(length(x),sum(is.na(x)),min(x,na.rm=TRUE),quantile(x,probs=0.25,na.rm=TRUE),
             median(x,na.rm=TRUE),mean(x,na.rm=TRUE),mean(x,trim=0.10,na.rm=TRUE),
             quantile(x,probs=0.75,na.rm=TRUE),max(x,na.rm=TRUE),
             IQR(x,na.rm=TRUE),mad(x,na.rm=TRUE),sd(x,na.rm=TRUE),skew(x,na.rm=TRUE),
             kurtosi(x,na.rm=TRUE),CV=(sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE))*100)
}

m<-c(10,12,15,16,17,18)

estadistico(m)
print(estadistico(m))

library(help = "datasets")
randu

es<-sapply(randu,estadistico)
print(es)

tap<-tapply(CO2$uptake,CO2$Treatment,estadistico)

#CLASE 04 PARTE INICIAL				
.libPaths(c("D:/R_Packages", .libPaths()))
getwd()
setwd("D:/9. Dictar_R/Clase_04/Script_ED")
getwd()
install.packages("psych")
library(psych)
#Ejemplo 1:

#Tenemos la siguiente data:
# x : valores ppm de un elemento en distintas rocas.
x<-c(4,5,6,7,4,10,4,5,7,8)

#Se pide calcular las medidas de centralización, posición y dispersión.

#Medidas de centralización:
#media aritmética:
mean(x,na.rm=TRUE)
#mediana:
median(x,na.rm=TRUE)
#moda:
y<-table(as.vector(x))
modex<-as.numeric(names(y)[y==max(y)])
#media geometrica
#1forma: 
exp(mean(log(x)))
#2da forma
geometric.mean(x,na.rm=TRUE)
#media corta (alpha=0.20)
mean(x, na.rm=TRUE,trim=0.20)

#media winzorida-alpha (tarea)

#Medidas de posición:
#Cuantiles:
quantile(x,probs = 0.20,na.rm=FALSE,type = 5)
Mediana<-quantile(x, probs = 0.50, na.rm = FALSE, type=5)
#minimo
min(x, na.rm=FALSE)
max(x, na.rm=FALSE)


#Medidas de Dispersión:
#Rango:
range(x, na.rm=FALSE)
#Varianza:
var(x, na.rm=FALSE)
#Desviación Estandar
sd(x, na.rm=TRUE)
#Rango Intercuartil
IQR(x, na.rm=FALSE, type = 5)
#Rango
Range(x, na.rm=FALSE)
#Intercuartil
RIQ<-quantile(x, probs=0.75, na.rm=TRUE, type=5)-quantile(x, probs=0.25, na.rm=TRUE,type=5)
#Desviación Media Absoluta:
med<-median(x,na.rm=TRUE)
MAD<-median(abs(x-med))
#Coeficiente de Variación:
CV=(sd(x)/mean(x))*100