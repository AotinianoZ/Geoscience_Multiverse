.libPaths(c("D:/R_Packages",.libPaths()))

####Librerias####
library(tidyverse)
library(ggpubr)
library(rstatix)


#### T-test ####

#Ejemplo practico:

x = rnorm(10)
y = rnorm(10)

ttest=t.test(x,y)
names(ttest)

ts = replicate(1000,t.test(rnorm(10),rnorm(10))$statistic)

#Grados de libertad 18 perdemos 2 porque tenemos que estimar
#la media

#One Way 
range(ts)

#Se supone que es simétrica
pts=seq(-4.5,4.5,length=100)
plot(pts, dt(pts,df=18), col="red",type="l")

#Agregamos una linea para mostrar la densidad para la muestra
#simulada
lines(density(ts))

#Otra forma de comparar: Q-Qplot

qqplot(ts,rt(100,df=18))
abline(0,1)


#Usamos funcion cuantil:
probs = c(.9,.95,.99)
quantile(ts,probs)
qt(probs, df=18)


t.test(x,y)
t.test(x,y,var.equal=TRUE)


#Probemos con la distribucion normal:

tps = replicate(1000,t.test(rnorm(10),rnorm(10))$p.value)
plot(density(tps))

qqplot(tps,runif(1000))
abline(0,1)


tps = replicate(1000,t.test(rnorm(10),rnorm(10),var.equal=TRUE)$p.value)
probs = c(.5,.7,.9,.95,.99)
quantile(tps,probs)

tps = replicate(1000,t.test(rnorm(10),rnorm(10),var.equal=TRUE)$p.value)
probs = c(.5,.7,.9,.95,.99)
quantile(tps,probs)

tps = replicate(1000,t.test(rnorm(10),rnorm(10,sd=5),var.equal=TRUE)$p.value)
quantile(tps,probs)

tps = replicate(1000,t.test(rnorm(10),rnorm(10,sd=5))$p.value)
quantile(tps,probs)

#Power of the t-test ??

####One Sample T-test (Ejemplo 3 @R)####

set.seed(1234)
my_data <- data.frame(
  name = paste0(rep("M_", 10), 1:10),
  weight = round(rnorm(10, 20, 2), 1)
)

head(my_data, 10)
tail(my_data,10)
summary(my_data$weight)

library(ggpubr)
ggboxplot(my_data$weight, 
          ylab = "Weight (g)", xlab = FALSE,
          ggtheme = theme_minimal())

#¿Es la muestra grande? - No, porque n < 30.
#Desde que la muestra no es lo suficientemente grande 
#(menor que 30, teorema central del limite), 
#debemos verificar que proviene de una distribución normal.

# Shapiro-Wilk test:

#Null hypothesis: the data are normally distributed
#Alternative hypothesis: the data are not normally distributed

shapiro.test(my_data$weight)

#Desde que el valor de salida p-valor es mayor que 0.05 implica que 
#la distribucion de la data es no difiere significativamente de la normal
#en otras palabras asumimos la normalidad.

library("ggpubr")
ggqqplot(my_data$weight, ylab = "Men's weight",
         ggtheme = theme_minimal())

#Del plot de normalidad se concluye que la data proviene de una 
#distribucion normal.

#Observacion: Si la data es no normal se recomienda realizar un 
#test no parametrico como Wilcoxon.

#Como queremos que sea diferentes es "two-tailed test"

# One-sample t-test
res <- t.test(my_data$weight, mu = 25)
# Ver resultados
res 

#Si queremos que sea menor que 25 (one tailed test) usamos "less"
t.test(my_data$weight, mu=25, alternative = "less")

#Si queremos que sea mayor que 25 (one tailed test) usamos "greater"
t.test(my_data$weight, mu=25, alternative = "greater")

#Resultados

#The p-value of the test is 7.95310^{-6}, 
#which is less than the significance level alpha = 0.05.
#We can conclude that the mean weight of the mice is 
#significantly different from 25g with a p-value = 7.95310^{-6}

# printing the p-value
res$p.value
# printing the mean
res$estimate
# printing the confidence interval
res$conf.int


####Unpaired Two-Sample T-test (Ejemplo aplicado en R:)####

# Data in two numeric vectors
banda1_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
banda2_weight <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4) 
# Create a data frame
my_data <- data.frame( 
  group = rep(c("Banda1", "Banda2"), each = 9),
  weight = c(banda1_weight, banda2_weight )
)
# Verificaremos si el promedio de uno es diferente al otro:

print(my_data)

#Sumario estadistico por grupos:
library(dplyr)
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )
# Ploteamos los pesos por grupo y color por grupo:

library("ggpubr")
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "Weight", xlab = "Groups")

#Verificamos los supuestos de independencia del t-test:

#1. ¿Son las dos muestras independientes?

#Esto es si se cumple debido a que son dos bandas distintas y no estan relacionadas.

#2. La data de los dos grupos viene de una distribucion normal:

# Shapiro-Wilk test de normalidad para pesos de banda1

with(my_data, shapiro.test(weight[group == "Banda2"]))# p = 0.11

# Shapiro-Wilk test de normalidad para pesos de banda2

with(my_data, shapiro.test(weight[group == "Banda1"])) # p = 0.61

#Como podemos ver las salidas de los p-valor son mayores que 0.05 implica que la 
#distribucion de los datos proviene de una data que no difiere de la normal. Asumimos normalidad.

#3. ¿La poblacion tiene igual varianza?

#Usaremos el F-test para la homogenidad de varianzas:

res.ftest <- var.test(weight ~ group, data = my_data)
res.ftest

#El p-valor de F-test es p=0.1713596. Es mayor que el nivel de significancia 0.05.
#En conclusion, no existe diferencia significativa entre las varianzas de los sets de data.
#Por lo tanto, usaremos el t-test clasico asumiendo iguales las dos varianzas.


# Haremos el t-test:

#Forma 1:

res <- t.test(banda1_weight, banda2_weight, var.equal = TRUE)
res


#Forma 2:
res <- t.test(weight ~ group, data = my_data, var.equal = TRUE)
res

#Acceder a los datos:
res$p.value
res$estimate
res$conf.int

#Para complementar en caso deseamos saber si el promedio de la banda2 es menor que la banda 1:

t.test(weight ~ group, data = my_data,
       var.equal = TRUE, alternative = "less")

#Si queremos saber que el promedio de la banda 2 es mayor que el promedio de la banda 1.
t.test(weight ~ group, data = my_data,
       var.equal = TRUE, alternative = "greater")

#### Paired two-sample t-test (Ejemplo aplicado en R_2)#####

# Rendimiento del geologo antes del entrenamiento:
before <-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
# Rendimiento del geologo despues del tratamiento:
after <-c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
# Crear el data frame:
my_data <- data.frame( 
  group = rep(c("before", "after"), each = 10),
  rendimiento = c(before,  after)
)
my_data

#Sumario estadistico por grupos:
library("dplyr")
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(rendimiento, na.rm=TRUE),
    sd = sd(rendimiento, na.rm = TRUE),
    CV = (sd/mean)*100
  )
# Plot rendimiento by group and color by group
library("ggpubr")
ggboxplot(my_data, x = "group", y = "rendimiento", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          order = c("before", "after"),
          ylab = "rendimiento", xlab = "Groups")

#El boxplot es util pero se pierde la visualizacion de la comparacion por pares:
install.packages("PairedData")

# Subset rendimiento de data antes del tratamiento
before <- subset(my_data,  group == "before", rendimiento,
                 drop = TRUE)
# subset weight data after treatment
after <- subset(my_data,  group == "after", rendimiento,
                drop = TRUE)
# Plot paired data
library(PairedData)
pd <- paired(before, after)
plot(pd, type = "profile") + theme_bw()

?plot
#Supuestos:

#1.¿Son las muestras apareadas?

#Si, desde que las datas han sido tomada del mismo geologo en dos tiempos.

#2. ¿Es la muestra grande?

#No, porque n<30. Desde que la muestra no es suficientemente grande (menos de 30), neccesitamos
#verificar si las diferencias de las datas siguen una distribución normal.

#Usaremos el test de Shapiro-Wilk:

# computar las diferencias
d <- with(my_data, 
          rendimiento[group == "before"] - rendimiento[group == "after"])
# Shapiro-Wilk normality test for the differences
shapiro.test(d) # => p-value = 0.6141

#El p-valor es mayor que el nivel de significancia de 0.05 implica que la distribución de las 
#diferencias  (d) no son significativamente diferentes de la normal. En otras palabras, podemos asumir
#normalidad.


#Computar t-test metodo 1:

res <- t.test(before, after, paired = TRUE)
res

#Computar t-test metodo 2:
res <- t.test(rendimiento ~ group, data = my_data, paired = TRUE)
res

#Si deseamos testear si el promedio del rendimiento before es menor que el rendimiento promedio despues
#del entrenamiento:

t.test(rendimiento ~ group, data = my_data, paired = TRUE,
       alternative = "less")

#Si deseamos testear si el promedio del rendimiento before es mayor que el rendimiento promedio despues
#del entrenamiento:

t.test(rendimiento ~ group, data = my_data, paired = TRUE,
       alternative = "greater")

res$statistic
res$parameter
res$p.value
res$conf.int
res$estimate
res$null.value
res$stderr
res$alternative
res$method
res$data.name








