#Es una miscelanea de Vectores y Matrices con algunas cosas basicas de R.

####Primera Parte####
getwd() #Ubicarnos en el proyecto
dir()   #Ver los archivos del directorio de trabajo

vector_01 <- rnorm(20,mean=15,sd=5.2)
length(vector_01)
class(vector_01)

vector_02<-c(10,12,14,16,18,18,17)

length(vector_02)
class(vector_02)
vector_01 + vector_02
vector_02[2]                        #Alt+91 = [] #Entrar al objeto
vector_02[c(3,4,7)]
summary(vector_02)

vector_02[(vector_02>15)]             #Parentesis sirve para una condicion.
vector_02[(vector_02>mean(vector_02))]

Marlon <- c(rnorm(10,mean=4,sd=2.5),rnorm(20,mean=10,sd=2.8))
Marlon
View(Marlon)
range(Marlon)
range(Marlon)[1]
range(Marlon)[2]

View(mtcars)

x <- mtcars$disp

summary(x)
range(x)
sum(x)
mean(x)

#### Datos Especiales####
# NA (Not Available)

GM <- c(10,12,14,NA,15,16,NA)

mean(GM)
mean(GM,na.rm=TRUE) #Para no considerar los no disponibles poner na.rm=TRUE
summary(GM)
is.na(GM)
sum(is.na(GM))
id <- is.na(GM)
View(GM[id])

# Infinito (Inf) y NaN (Not a number)

sqrt(-24)
sqrt(-24+0i)
23/0
-23/0

#####Introduccion a Matrices####
#Creando matrices:
c(1:12)

#Cantidad de elementos de la matriz es filas*columnas:

Alonso <- matrix(c(1:12),nrow=4,ncol=3,byrow = TRUE)
Joseps <- matrix(c(1:12),nrow=4,ncol=3,byrow = FALSE)
is.matrix(Alonso)
is.matrix(Joseps)

y <- c(1:8)
dim(y) <- c(2,4)              #dim() se usa para dimensionar una matriz
y
dimnames(y)                 #dim() se usa para asignar nombres a un matriz
dimnames(y)<-list(c("F1","F2"),c("C1","C2","C3","C4"))
y

covid_19<-matrix(1:12,nrow=4,dimnames = list(c("Luis","Enrique","Jose",
                                               "Manuel"),c("A","B","C")))
covid_19[1,1] #Primera fila y primera columna
covid_19[3,2] #Tercera fila y segunda columna
covid_19[ ,1] #Todas las filas pero la primera columna
covid_19[1, ] #La primera fila de todas las columnas
covid_19[1,c(2,3)] #La primera fila, pero las columnas 2 y 3.
summary(covid_19)

mtcars[1,1]

#Repasando 
vector <- c(14,15,19,21,25,27,19)   #Las funciones van entre parrntesis
class(vector)
length(vector)
summary(vector)
vector[4]              #Alt+91=[] entrar a los elementos del vector


#Valores mayores a la media aritm?tica 
summary(vector)      #Ver los valores centrales de la variable (incluida la media aritm?tica)
vector[(vector>20)]
length(vector[(vector>20)])

#Ejemplos:
Andre<-rnorm(15,mean = 20,sd=2.5)
Andre
Alonso<-sample(1:50)
Alonso
View(Andre)
View(Alonso)

random <- c(rnorm(5,10,5),runif(15,14,20))
random
class(random)
range(random)
range(random)[1]

####Datos Especiales####
#Primero los NA:
peso<-c(1,2,4,54,25,45,NA,NA,2)
peso
summary(peso)
mean(peso, na.rm = TRUE) #na.rm=TRUE le digo al software que trabaje con datos v?lidos

x <- NA
y <- c(2,4,6,8,10)
x+1
x+y
z <- c(x,4,5,x)
mean(z, na.rm=TRUE)

#Segundo los NaN (Not a Number)

15/0    #Inf : Infinito
sqrt(-23+0i)
t<- -23/0
t

####MATRICES####

primera <- matrix(1:12,nrow=6, byrow=TRUE)
primera
segunda <- matrix(1:3, nrow=6, byrow=TRUE)
segunda

y <- c(1:8)
dim(y) <- c(2,4)  #Asignamos a los valores una matriz de 2 filas y 4 columnas
dimnames(y) <- list(c("F1","F2"),c("C1","C2","C3","C4"))
View(y)

covid_19 <- matrix(1:12,ncol=3,dimnames = list(c("Desobedece mucho","Desobedece",
                                               "Obedece","Es fiel"),c("A","B","C")))
View(covid_19)
summary(covid_19)
covid_19[1,1]    #Accediendo a la 1era fila de la 1era columna
covid_19[ ,1]
covid_19[1, ]
covid_19[ ,c(2,3)]
covid_20<-covid_19[ ,c(2,3)]

jueves<-matrix(rep(c(T,F),6),ncol=4,nrow=3,byrow=TRUE)
c(T,F)
rep(c(T,F),6)
length(rep(c(T,F),6))

M<-matrix(1:8,2,4)
diag(M)
M[ ,3]
M[ ,3,drop=FALSE]   #Para tomar a los elementos como matriz es poner drop=FALSE.

Economia<-matrix(1:10,nrow=5,byrow=TRUE)
View(Economia)

Geologia<-c(1:5)

Mineria_Economica<-cbind(Economia,Geologia)  #cbind junta vectores o matrices por columna
dim(Mineria_Economica)

mama <- matrix(1:12,byrow=FALSE,ncol=3)
ama <- matrix(13:24,byrow=FALSE,ncol=3)

mama_ama <- cbind(mama,ama)
ama_mama <- rbind(mama,ama)

mama
ama
mama_ama
ama_mama


#Formas de ayuda en Rstudio
help("library")
help("summary")
?cars

vector <- c(25,15,17,18,19,20,28)
Angie_Celene <- c("Mi mama me ama",24,"Me despidieron")
summary(vector)

vector_normal <- rnorm(n=1000000,mean=15,sd=1.5)
vector_normal

View(vector_normal)

#La cantidad de elementos
Boris <- c(2,1,4,5,4,1,4,7,4,5,4,7,4,5,4,7,4)
length(Boris) #Longitud del vector (elementos)
class(Boris) #clase del vector

#rnorm()  r: random , norm:distribuci?n normal
#rnorm(n,mean=0,sd=1)

vec1 <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
vec <- c(1:17)

2+2
log(2)
log(log(2.4^54))
#Cambio de variable:
x <- BD_TACNA
sin(x)+cos(x)

#Tipos de data en Rstudio:
x<-28
class(x)
y<-"Alonso ensena muy bien con nota 17"
class(y)
z<-FALSE
class(z)
w<-0+4i
class(w)

x<-c(28,19,18)
class(x)

x <- 15
x == 18

Jo <- c("Jo","Cobre")
Jo == "Cobre"

#Creando vectores:
peso <- c(33,45,78,77,45,89,48,75)
peso
class(peso) #Tipo de elementos del vector
length(peso) #Cantidad de elementos
str(peso) #Estructura de la data

peso[4] #Alt+91 [] para entrar a los elementos creados
peso[5]
peso[20] #NA porque no existe el elemento 20
peso[peso>25]
peso[peso==45]
length(peso[peso==45])
#Para conocer las posiciones (filtrar la data)
#vamos a usar el comando which (poderoso)
peso>45 #Detectar verdaderos o falsos
peso[peso>45] #Para obtener los elementos
which(peso>45) #Me da las posiciones
length(peso>45) #Longitud la mide del vector no de la condicion
length(peso)

peso_45 <- peso[peso>45]
length(peso_45)

#Funciones basicas: (algoritmos que estan por 
#default en el R)

# Data cars:
str(mtcars)
View(mtcars)
summary(mtcars) #probar que funcione el Rstudio
boxplot(mtcars)
range(mtcars) #Para acceder a los elementos del dataframe
#usamos el simbolo  $ (dolar).
x <- mtcars$hp
class(x)
length(x)
summary(x)
range(x)

#Mas funciones
min(mtcars$hp)
max(mtcars$mpg)
var(mtcars$mpg) #Varianza de la variable mpg del data frame
# mtcars
sd(mtcars$mpg)  #Desviacion estandar de la variable mpg
#del data frame  mtcars
mean(mtcars$mpg) #Media aritmetica de la varible mpg...
median(mtcars$mpg) #Mediana de los datos.
#Mediana es robusta frente a la media aritmetica.
#Medida de tendencia central.
vectorx<-c(1,20,34,45,54,6,57,8,78,4500)
mean(x)
median(x)

#Del vector x voy a elegir las posiciones 2,5 y 9
vectorx[c(2,5,9)]
vectorx[c(1,4,8)]

#Obtener los elementos que superan la media aritmetica
#y cuantos son:
which() #Filtrar obtener elementos (Para Alonso)
vectorx[vectorx>median(vectorx)]
length(vectorx[vectorx>median(vectorx)])

vectorx[vectorx>median(vectorx)|vectorx<=150000]

####Datos Especiales####

# NA (Not Available == No disponible)
Edad<-c(10,12,14,NA,15,16,NA,10,10,10,10)
str(Edad)
mean(Edad) #Hay problemas por elementos vacios (no disponible)
mean(Edad,na.rm = TRUE) #Para no considerar los no
#disponibles se pone "na.rm =TRUE"
mean(Edad,na.rm = TRUE) 
#Contar los vac?os
is.na(Edad) #Pregunto si tiene vacios "is.na()"
sum(is.na(Edad))
which(is.na(Edad))
#True=1 False=0 (c?digos binarios)

is.vector(Edad)
is.matrix(Edad)
is.data.frame(Edad)

23/0

####Matrices####
primera<-c(1:12)
mi_matrix01<-matrix(data=primera,nrow=6,ncol = 2,byrow=TRUE)

mi_matrix01[1, ]
mi_matrix01[2,5]
mi_matrix01[c(3,4),2]

mi_matrix01[ ,1]
mi_matrix01[-c(1,2), ]
mi_matrix01[-c(1,2),1]
mi_matrix01[-c(1,2),2]

mi_matrix02<-matrix(data=primera,nrow=4,ncol = 3,byrow=TRUE)
mi_matrix02[ ,c(1,2)]
