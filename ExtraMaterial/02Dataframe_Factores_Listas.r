
                                                    #### Repaso Anterior####
#Matrices:
matrix_01 <- matrix(1:12,nrow=6,byrow = TRUE)
dim(matrix_01)

y <- c(1:8)
dim(y) <- c(2,4) #al poner dim(vector) genero una matriz
#tengo que poner cuantas filas y columnas c(filas,columnas)
dimnames(y) <- list(c("F1","F2"), c("C1","C2","C3","C4"))
y

covid_19 <- matrix(1:12,ncol=3,
                 dimnames = list(c("Desobedece mucho","Desobedece",
                                   "Obedece","Es fiel"),
                                 c("a","b","c")))

#Entrar al primer elemento de primera fila
#Todas las filas pero columna 2 y 3
#Saque las columnas 1 y 3 y el resultado sea una matriz
covid_19[1,1]
covid_19[ ,c(2,3)]
covid_19[ ,c(1,3) ]
covid_19[ ,c("a","c")]
covid_19[ ,c(-1,-3),drop=FALSE] #drop se usa para dejar el filtro como matriz
covid_19[ ,c(-1,-3),drop=TRUE]

ncol(covid_19)
nrow(covid_19)
colnames(covid_19)
rownames(covid_19)
str(covid_19)

x <- c(1:10)
is.vector(x)
as.matrix(x)

##Creamos una matrix
Economia <- matrix(1:10,byrow=TRUE,nrow=5)
View(Economia)
Geologia <- c(1:5)
Mineria <- cbind(Economia,Geologia)

mama <- matrix(13:24,byrow=TRUE,ncol=3)
ama <- matrix(1:12,byrow=FALSE,ncol=3)

mama_ama <- cbind(mama,ama)
Mineria_mama <- rbind(Mineria,mama)

cbind(1:3,1:6) #Lo veo a explicar a continuacion :D

####Funcion seq###
seq(0,50,by=2)
seq(5,45,by=8)
seq(48,0,by=-8)
seq(10,45,by=2.2)
seq(1,9,by=pi)
seq(7,14, length.out = 30)
c(seq(1,6,by=3),seq(8,2,length.out = 20))

###Funcion rep###
rep(1:4,2)
rep(1:4,each=2)
rep(1:4,c(2,2,2,2))
rep(1:4,c(2,1,2,1))
rep(1:4,each=2,len=40)
rep(c(1,2),each=4)

####Funcion sort###
vector1 <- c(1,2,4,8,57,42,11,15,16,17,18,22,14)
class(vector1)
str(vector1)
summary(vector1)
vec_ord_cre <- sort(vector1,decreasing = FALSE)
vec_ord_dec <- sort(vector1, decreasing=TRUE)

####Miscelanea###

secuencia <- seq(1,25,by=3)
repeticion <- rep(secuencia,each=3)
sort(repeticion, decreasing = TRUE)

                                                #### Data Frame ####

a <- c(10,20,30,40)
b <- c("libro","lapicero","Notas","portapapel")
c <- c(TRUE,FALSE,TRUE,FALSE)
d <- c(7.4,4.5,8,22)

#Unir todas las variables para generar el dataframe:

df <- data.frame(a,b,c,d,stringsAsFactors = TRUE)
View(df)
df
names(df)<-c("ID","Items","Stock","Precio")
df
str(df)

df2 <- data.frame(a,b,c,d)
str(df2)

summary(df)
summary(df2)

# Data frame CO2
?CO2
str(CO2)

#MOVERSE EN EL DATA FRAME  nombre_de_la_data [primero,segundo]:
View(CO2)

CO2[1,2]
CO2[1:2, ]
CO2[ ,1]
summary(CO2)
summary(CO2[ ,c("conc")])

colnames(CO2)

db2 <- CO2[ ,c("Treatment","conc")]

summary(db2)

x <- c(2,7,8,9,2,4)
y <- matrix(x,ncol=3)
colnames(y) <- c("Y1","Y2","Y3")
rownames(y) <- c("CasoA","CasoB")

w <- as.data.frame(y)

w$Alonso <- c("si","no")
w$Andree <- c(mean(w$Y1),mean(w$Y1))

w$Alonso
w$Andree

#Filtrar informacion respecto a concentraciones

#SUBSET
filtro <- subset(CO2,subset=conc>mean(CO2$conc))
mean(CO2$conc)
View(filtro)

filtro2 <- subset(CO2,subset=Treatment=="chilled"&uptake>40|Type=="Quebec",select = )
colnames(CO2)
View(filtro2)

#FILTER
colnames(CO2)
summary(CO2)
filtro4 <- filter(CO2,Plant=="Qn1",Type=="Quebec",conc>min(CO2$conc),uptake>2*mean(CO2$uptake))
BD_CO2 <- CO2

                                               

#En caso se necesite subset la data se puede usar Pipeline %>%:

a <- BD_HA %>% group_by(Temporada,Microcuenca) %>% summarize(pHmean=mean(pH), pHsd=sd(pH), count = n())
View(a)

b<- BD_HA %>% group_by(Temporada) %>% summarize(pHmean=mean(pH), pHsd=sd(pH), count = n())
View(b)

c <- filter(BD_HA, Temporada=="Avenida", pH<7.5) #Se pueden poner m?s condiciones con comas.
View(c)

d <- filter(BD_HA, Temporada=="Estiaje",CE_uS_cm<700 )
View(d)

e <- subset(BD_HA, Temporada=="Estiaje" & CE_uS_cm>500, select = Codigo) #Se pueden seleccionar columnas espec?ficas.
View(e)


                                        ####Estructuras de Decision####

##Estructura if, if else, else:

cantidad_papa<-50

if(cantidad_papa>80){
  print("Tu vendiste mucho")
}else{
  print("No fue suficiente hoy")
}

cantidad<-90

if(cantidad<50){
  print("No fue suficiente hoy")
}else if(cantidad>=50&cantidad<=60){
  print("Promedio de ventas")
}else{
  print("El dia fue maravilloso")
}

x<-c("yo","tu","els")
str(x)

# %in% :valor logico
if("el" %in% x){
  print("Con el curso me va con el")
}else if("tu" %in% x){
  print("Con el curso me va contigo")
}else{
  print("Yo fui enganado")
}

p <- runif(n=10,min=0,max=20)
Vec_Booleano <- p>=14
NumAprob1 <- length(p)-5
NumAprob2 <- length(Vec_Booleano[Vec_Booleano==TRUE])
which(Vec_Booleano)

if(NumAprob1>0){
  p[which(Vec_Booleano)]
}

p<-runif(n=1000000000,min=0,max=30)
Vec_Booleano2<-p>=15
NumAprob1<-sum(Vec_Booleano2)
NumAprob2<-length(Vec_Booleano2[Vec_Booleano2==TRUE])
system.time(sum(Vec_Booleano2))
system.time(length(Vec_Booleano2[Vec_Booleano2==TRUE]))

#Ejemplo final:
Score<-as.integer(readline("Ingresa tu nota: "))
if(Score>=19){
  print("Excelente!!")
}else if(Score>=16){
  print("Felicidades!!")
}else if(Score>=14){
  print("Ok pasable")
}else{
  print("Mejor estudia de nuevo")
}
rm() #Sirve para eliminar un objeto este objeto va dentro de las
#parentesis

#For loop:

for(i in 1:5){
  print(i)
}

y<-c(1:5)
for(x in 1:5){
  w<-y**x
  print(w)
}
fruta<-c("Manzana","Naranja","Melocoton","Platano")
for(i in fruta){
  print(i)
}

lista<-c()
for(i in seq(1,4,by=1)){
  lista[[i]]<-i*i
}
print(lista)

frutas<-list(Caja=c("Manzana","Naranja","Melocotpn","Platano"),
             dinero=c(10,12,15),compra=FALSE)

for(p in frutas){
  print(p)
}

matriz<-matrix(data=seq(9,20,by=1),nrow=6,ncol=2)
for(r in 1:nrow(matriz)){
  for(c in 1:ncol(matriz)){
    print(paste("Row",r,"and column",c,"have value of",matriz[r,c]))
  }
}

for(year in 2015:2050){
  print(paste("The year is",year))
}

x<-c("a","b","c","d")
seq_along(x)
for(i in seq_along(x)){
  print(x[i])
}

x<-matrix(1:6,2,3)
for(i in seq_len(nrow(x))){
  for(j in seq_len(ncol(x))){
    print(x[i,j])
  }
}

#While (loop)
count<-0 #Condicion de inicio
while(count<10){
  print(count)
  count<-count+1
}

Comenzar<-1
while(Comenzar<=8){
  cat("Este es un loop while n?mero",Comenzar)
  Comenzar<-Comenzar+1
}

set.seed(123)
stock<-50
precio<-50
loop<-0
while(precio>45){
  precio<-stock+sample(-10:10,1)
  loop<-loop+1
  print(loop)
}
cat("si tomo",loop,"loop antes menor precio.","El menor precio",precio)


#Switch

x <- 1:10
type <- "mi_mama"

switch(type,
       mean=mean(x),
       median=median(x),
       sd=sd(x),
       mi_mama=mean(x)+median(x),
       summary=summary(x))

x <- CO2$uptake
type <- "alonso"
switch(type,
       mean=mean(x),
       median=median(x),
       sed=sd(x))

#Repeat (para programadores NETOS)
#Next/Break

for(i in 1:10){
  if(i%%2==0)next
  print(i)
}

for(i in 1:10){
  if(!i%%2){
    next
  }
  print(i)
}

for(i in mtcars$mpg){
  print(i)
  if(i<15)break
}

##IMPRIMIR LA TABLA DE MULTIPLICAR:
num<-as.integer(readline("Ingrese un numero: "))
for(i in 1:12){
  print(paste(num,"x",i,"=",num*i))
}

                                                    ####Factores en R:####

vec_genero<-c("Mas","Fe","Fe","Ma","Fe","Ma") #variable categorica nominal
str(vec_genero)
fac_vec_genero<-factor(vec_genero)
str(fac_vec_genero)

vec_color<-c("azul","rojo","verde","blanco","negro","amarillo")
str(vec_color)
vec_color<-factor(vec_color)

# c(dia,tarde,noche) #variable categorica ordinal

vec_par_dia<-c("noche","manana","tarde","mediodia",
               "medianoche","noche")
str(vec_par_dia)
factor_dia<-factor(vec_par_dia,
                   order=TRUE,
                   levels=c("manana","mediodia","tarde","noche",
                            "medianoche"))
factor_dia
View(factor_dia)

                                                    ####Listas en R:####

#
vec <- 1:5
mat <- matrix(1:10,ncol=5)
df <- EuStockMarkets[1:10, ]
?EuStockMarkets
View(df)

mi_lista <- list(vec,mat,df)

df_tabrajo <- mi_lista[[3]]

#
a <- matrix(1:50,nrow=10)
b <- c("higado","pescado")
c <- TRUE

mis_cosas <- list(a,b,c)

mis_cosas[3]




