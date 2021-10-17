.libPaths(c("D:/R_Packages",.libPaths()))


##### Regresion Lineal Multiple ####

load(file = "BostonHousing.rda")

View(BostonHousing)

# Datos contienen informacion de 506 viviendas de la ciudad de boston (censo de 1970)

colnames(BostonHousing)

# crim : crimen per capita por ciudad 
# zn : proporcion de terrenos residenciales 
# indus : proporcion de acres por negocios no minoristas
# chas : Variables categorica 
# nox : concentracion de oxidos nitricos
# rm : numero promedio de habitaciones por vivienda 
# age : proporcion de unidaddes ocupadas por sus propietarios construidas antes de 1940
# dis : distancias al trabajo
# rad : indice de accesibilidad a las autopipistas
# tax : tasa de impuesto 
#  ptratio : colegios por localidad 
#  b : proporcion de personas de color en la ciudad 
# lstat :variables estadistica 
# medv : valor medio de las viviendas ocupadas 

housing <- BostonHousing

class(housing)

str(housing)

head(housing)
summary(housing)

View(lm)

### Analisis de regresion ###
# usamos la funcion lm : linear modelling
model <- lm(medv ~ . , data = housing)
#  medv = alfa_1 crim + alfa_2 zn + alfa_3 indus + alfa_4 chas1 + alfa5 nox + alfa_6 rm +
#         alfa_7age + alfa_8 dis + alfa_9rad + alfa_10 tax + alfa11 ptratio + alfa_12 b + 
#         alfa_13 lstat + intercepto
model
summary(model)

# Segundo modelo : Eliminar la variable categorica 
model <- lm(medv ~ crim + zn + indus +nox + rm + age+dis+rad+tax+ptratio + b +lstat , data = housing)
summary(model)


# tercer modelo 
model <- lm(medv ~ crim + zn + nox + rm +dis+rad+tax+ptratio + b +lstat , data = housing)
summary(model)


# cuarto  modelo : Criterio personal 
model <- lm(medv ~  nox + rm +dis+ ptratio  + lstat , data = housing)
summary(model)

# Regresion lineal simple : 1 variable dependiente y 1 variables independiente 
# Regresion lineal multiple : 1 variable dependiente y varias/muchas  variables independiente 

plot(model)

#### Regresion Logistica####


# Es un modelo de regresion no lineal, pero es lineal en escala logaritmica.

#Conjunto de datos de Kaggle

#Para este ejemplo se han seleccionado 200 registros:

datos<-read.csv("HumanResourcesAnalytics.csv", header = TRUE)
str(datos)
muestra<-dim(datos)[1]

datos<-datos[sample(muestra,200, replace=TRUE), ]
class(datos)
str(datos)
head(datos)
tail(datos)
View(datos)

colnames(datos) = c("nivel_satisfaccion","ultima_evaluacion","numero_proyectos", "promedio_horas_mensuales",
                    "antiguedad", "accidente","abandona","promocionado","departamento","salario")
colnames(datos)

# En este conjunto tenemos 3 variables sobre las que podemos estimar un modelo logit: *accidente, abandona y promocionado*.
# Un estudio de interes puede ser intentar explicar/predecir si un empleado abandonara o no la empresa en funcion de las
# puntuaciones de las variables *nivel_satisfaccion  ultima_evaluacion". Seleccionaremos esas variables del modelo:

datos.modelo<- subset(datos, select = c(abandona, nivel_satisfaccion, ultima_evaluacion))
datos.modelo$abandona <- factor(datos.modelo$abandona)
head(datos.modelo)
plot(datos.modelo$nivel_satisfaccion, datos.modelo$abandona)


#Debemos asegurarnos que la variable factor solo toma 2 valores.

table(datos.modelo$abandona)
summary(datos.modelo$nivel_satisfaccion)
summary(datos.modelo$ultima_evaluacion)

# Ademas asegurarnos de que la variable respuesta factor solo toma dos valores.

library(ggplot2)
ggplot(datos.modelo, aes(x=nivel_satisfaccion, y=ultima_evaluacion, color =abandona))+
  geom_point()

# En r los GLMs se ajustan a la funcion glm. La principal diferencia con la funcion lm para ajustar modelos lineales
# es que tenemos que proporcionar la familia de la distribucion. En nuestro caso variable dicotomica:

modelo.logit<-glm(abandona~ultima_evaluacion+nivel_satisfaccion, data=datos.modelo,
                  family = "binomial")
summary(modelo.logit)

# La interpretacion de los p-valores es similar a la del modelo lineal. Podemos ver que la *ultima_evaluacion* no es
#significativa en el modelo, el p-valor debe ser mucho mayor de 0.05, mientras que la variable *nivel_satisfaccion* es 
# moderadamente significativa si es que su p-valor esta entre 0.01 y 0.05.

#En cuanto a los coeficientes, cambia la interpretacion. El modelo GLM no ajusta la variable respuesta sino una funcion
# de enlace. η = ln(p/1-p) siendo p la probabilidad que el individuo tome el valor de 1 en la variable dicotomica.
# Al cociente (p/1-p) se le conoce como odds ratio. Por lo tanto, los coeficientes del modelo logit se interpretan
# como el logaritmo de odds ratio. Si nos fijamos en el coeficiente de la variable nivel_satisfaccion (-3.00984), nos
# esta indicando que el logaritmo de odds ratio de abandonar la empresa disminuye 2.163 unidades por cada unidad que 
# aumenta la puntuacion en el nivel de satisfaccion.

# Una forma de facilitar de los coeficientes es evaluando en la exponencial
exp(coefficients(modelo.logit))

# Que corresponde al modelo : odds=(e^β0)*(e^β1x1)*(e^β2x2) lo interpretamos de la siguiente manera:
# al aumentar en la ultima evaluacion un punto aumenta un 5.91% las posibilidades de abandonar la empresa, mientras
# que aumentar un punto en el nivel de satisfaccion las reduce a casi 96%.


# Interpretacion probabilista seria estimar la probabilidad p de que un individuo abandone la empresa
# p = e^η/1-e^η, asi podemos predecir la funcion η para un individuo, por ejemplo una evaluacion de 0.75 y nivel
# de satisfaccion de 0.6.

log.odds<-predict(modelo.logit, data.frame(nivel_satisfaccion=0.6, ultima_evaluacion=0.75))
log.odds

# la probabilidad de abandonar la empresa seria:

exp(log.odds)/(1-exp(log.odds))


#### Regresion Polinomial ####
library(ISLR)
data(Wage)
View(Wage)

# al usar lm , el primer argumento es una formula 
# Variable dependiente : wage (salario)
# variable independientes : age (edad)
# 
# lm(wage ~ I(age^2)+ I(age^3) + I(age^4) , data = Wage )
# 
# lm(wage ~ poly(age , 4 ) , data = Wage )
# 
# lm(wage ~ poly(age , 4 , raw = TRUE) , data = Wage )

### MOdelo polinomial de grado 4 ###
modelo_poli4 <- lm(wage ~ poly(age,4), data = Wage)
summary(modelo_poli4)
#  En funcion de los resultados obtenidos , podemos obviar los coeficientes 
# de grado 4 y de grado : Utilizar es un polinomio de grado 2 
modelo_poli2 <- lm(wage ~ poly(age,2), data = Wage)
summary(modelo_poli2)

modelo_lineal <- lm(wage ~ age , data = Wage)
summary(modelo_lineal)

### Otro Modelo Polinomial ###
q<-seq(from=0, to=20, by=0.1)
y<-500+0.4*(q-10)^3
noise<-rnorm(length(q), mean=10, sd=80)
noisy.y<- y + noise
plot(q, noisy.y, col="deepskyblue4", xlab="q",main="Observed data")
lines(q,y, col="firebrick1",lwd=3)
model<-lm(noisy.y ~ poly(q,3))
model

# Usando la funcion confint podemos obtener los intervalos de confianza de los parametros de nuestro modelo:

confint(model, level=0.95)

#### Regresion Poisson ####

# Datos de recuento - revisar la distribucion poisson!

# Datos de recuento aquella que toman valores positivos enteross (incluyendo el cero)

# Dado que los modelos de regresion lineal y eleccion binaria fallan en estos casos.

# En una regresion logistica se predice una respuesta (como ya hemos visto) que viene en una de las formas, cara o sello,
# varon o mujer, hubo terremoto o no hubo.  La generalizacion a un tip de respuesta que viene en varios eventos discretos
# que pueden ser mas de dos se llama regresion de Poisson.

# Analicemos los datos de una clase, dado que nuestra tarea es predecir la nota, que puede tomar valore de 1,2,3,4,5
# a partir de los desciptores, el primero mide el trabajo en casa y el segundo la asistencia a clases:

rm(list=ls())
NumResueltos<-c(0,1,2,1,5,3,2,5,7,8,12,13,12,11,10,12,10,15)
HorasClase<-c(1,3,4,1,3,5,1,3,5,2,3,5,0,3,5,4,3,5)
Nota<- c(0,2,3,0,4,3,1,3,4,3,4,5,4,5,4,5,5,5)
tabla<-data.frame(NumResueltos, HorasClase, Nota)

regPoisson<-glm(Nota~NumResueltos+HorasClase,
                data=tabla, family = poisson())
summary(regPoisson)
predict(regPoisson, type = "response")


