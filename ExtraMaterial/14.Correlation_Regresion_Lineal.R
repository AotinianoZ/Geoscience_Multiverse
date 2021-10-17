.libPaths(c("D:/R_Packages",.libPaths()))

grasas<-read.table("age_weight_fat.txt", header = TRUE)
View(grasas)
colnames(grasas)
length(grasas)
str(grasas)
head(grasas)
tail(grasas)
grasas$peso

####Correlacion####
#Grafico por pares
pairs(grasas)

#Calcular la matriz de correlación
cor(grasas, method = "pearson")
cor(grasas,method = "spearman")
cor(grasas,method = "kendall")
plot(grasas$edad)
plot(grasas$peso)

id<-which(grasas$peso<31)
View(grasas[id, ])

correl<-cor(grasas, y = NULL, use = "everything",method = c("pearson"))

#Plot de la matriz de correlación
library(corrplot)
corrplot(correl, method = "circle", order="hclust")

####Regresion Lineal####

#Ejemplo1:
#Cálculo y representación de la recta de mínimos cuadrados
#comando - lm (linear models)
#El primer argumento es una fórmula : (variable dependiente) y ~ x (variable independiente)
# y ~ x : y <- variable dependiente
#         x <- variable independiente
regresion <- lm(grasas ~ edad, data = grasas)
regresion
regresion$coefficients
regresion$residuals
regresion$effects
regresion$rank
regresion$fitted.values
regresion$assign
regresion$qr
regresion$df.residual
regresion$xlevels
regresion$call
regresion$terms
regresion$model

regresion
#Obtenemos los coeficientes del modelo:
#b_0 <- 102.575
#b_1 <- 5.321
# grasas <- 5.321 (edad) + 102.575

regresion$residuals

#Positivo por encima
#Negativo por debajo

res <- regresion$residuals
class(res)

#jpeg(filename = "Grafico_residuos.jpg")
plot(res,
     main = "Residuos del modelo lineal",
     sub = "age_weight_fat.txt",
     ylab = "errores")
#Para revisar visualmente debemos graficar un histograma
hist(res)

#jpeg(filename = "Regresion_edad_peso.jepg")
plot(grasas$edad, grasas$grasas,
     main = "Regresion Lineal",
     xlab = "Edad",
     ylab = "Grasas")
abline(regresion)

# Para ver la calidad de mi regresion
summary(regresion)

#Ejemplo2:

edad_roca<-c(56,42,72,36,63,47,55,47,38,42)
conc_cobre<-c(148,126,159,118,149,130,151,142,114,141)
edad_roca
conc_cobre

plot(edad_roca,conc_cobre)


#Cierto grado de linealidad entre ambas variables:

reg_lin<-lm(edad_roca ~ conc_cobre)
reg_lin
plot(edad_roca,conc_cobre)
abline(reg_lin)

# edad_roca(i)=-43.5440+0.6774*conc_cobre (asumiendo que sigue esa logica)

summary(reg_lin)


#Respecto a estos valores mientras mas peque?os 
#sean mejor se ajustan al modelo de prediccion los valores futuros.

# Coefficients:
#         Estimate Std. Error  t value   Pr(>|t|)    
# (Intercept) -43.5440    17.6126  -2.472    0.038571 *  
#         presion    0.6774     0.1271    5.328    0.000704 ***
#         ---
#         Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Estimate son los valores de los coeficientes, segunda columna es el error estandar
#distribucion t a partir de la cual se calcula el p-valor, * y *** nos lleva en este
#caso aceptar el modelo tiene una significancia estadistica del 95%, bajo
#una perspectiva param?trica el modelo es aceptado.

# Residual standard error: 5.742 on 8 degrees of freedom
# Multiple R-squared:  0.7802,	Adjusted R-squared:  0.7527 

#El R-squared indicador de la bondad de ajuste del modelo, oscila
#entre 0 y 1, proximo a 1 buen ajuste lineal de los datos.

# F-statistic: 28.39 on 1 and 8 DF,  p-value: 0.000704

#Para ver si el modelo lineal es adecuado para modelar nuestro conjunto de datos
#se conoce como la F de Snedecor, se conoce como contraste omnibus.En nuestro
#caso el modelo lineal es adecuado (p-value<0.05)

#Suposiciones asociadas a los residuos:

#**Normalidad de residuos**:

#Aplicaremos el test de normalidad Kolmogorov-Smirnov
#con ks.test(x, distrib)

ks.test(reg_lin$residuals, "pnorm")
shapiro.test(reg_lin$residuals)

# La salida es la siguiente:


# One-sample Kolmogorov-Smirnov test
# 
# data:  reg_lin$residuals
# D = 0.3935, p-value = 0.06608
# alternative hypothesis: two-sided
# 
# Shapiro-Wilk normality test
# 
# data:  reg_lin$residuals
# W = 0.97844, p-value = 0.9563

#Segun estos resultados tenemos que aun 10% de significacion los residuos siguen una
#distribucion normal, puesto que el p-valor (0.06608) es mayor que 0.05.
#El test de Shapiro-Wilk por lo general es mas potente y tambien nos indican
#que los residuos siguen una distribucion normal.

#**Independencia de los residuos**:

#Aplicaremos el test de Durbin-Watson que contrasta la independencia de los residuos 
#la funcion que calcula este test se llama dwtest y se encuentra dentro del paquete
#lmtest.

install.packages("lmtest")
library(lmtest)
dwtest(edad_roca ~ conc_cobre)
#La salida es la siguiente:

# Durbin-Watson test
# 
# data:  edad_roca ~ conc_cobre
# DW = 1.9667, p-value = 0.5879
# alternative hypothesis: true autocorrelation is greater than 0


#Para un p-valor de 0.5879 no podemos rechazar la hipotesis de que los residuos son
#independientes.

#Homocedasticidad (igualdad de las varianzas de los residuos)
#Linealidad de los residuos

#Para estos ultimos usamos:
plot(reg_lin)

#para el grafico Residual vs Fitted, Scale-Location los residuos deben estar aleatoriamente distribuidos
#a lo largo del grafico, sin forman ningun tipo de patron.

#El grafico Q-Q plot residuos nos sirve para contrastar la normalidad de los residuos. Lo 
#deseable es que los residuos estandarizados estan lo mas cerca posible a la 
#linea punteada que aparece en el grafico. Esto esta matematicamente sustentado
#con el test de normalidad K-S o S-W.

#El grafico de los residuos estandarizados frente a leverages se utiliza para detectar
#puntos con influencia importante en el calculo de las estimaciones de parametros.
#En caso de detectarse algun patron fuera de los limites que establecen las
#lineas discontinuas debe estudiarse ese punto de forma aislada para detectar.
#Es bueno para ver si la importancia elevada de una observacion se debe a un error.
