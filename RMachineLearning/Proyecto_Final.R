---
title: "Machine Learning Data Facebook - Regression Analysis"
author: "Alonso Otiniano Zavala"
date: "`r Sys.Date()`"
output:
  html_document:
    df_print: paged
    code_folding: show
    toc: true
    toc_float:
      collapsed: true
      smooth_scroll: true
    theme: flatly
    highlight: "espresso"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message = FALSE, eval = FALSE)
library(ggplot2)
library(lattice)
library(caret) 
library(mlbench)
library(AppliedPredictiveModeling)
library(e1071)
library(rpart)
library(corrplot)
library(Amelia)
library(RCurl)
library(MASS)
library(klaR)
library(randomForest)
library(glmnet)
library(kernlab)
library(C50)
library(caretEnsemble)
```

# Descripcion del proyecto:

El dataset Facebook es una data que tiene como objetivo modelar los valores continuos de "Lifetime Post Consumers" por lo cual tenemos un **problema de regresion**. Contiene _500 filas y 19 columnas_, las variables a estudio son las siguientes:

"Page.total.likes" : Numero de personas quienes dieron like a la página de la compañía.

"Type" : Tipo de contenido (Link, Photo, Status, Video)

"Category" : Caracterización Manual del contenido: acción, producto e inspiración.

"Post.Month" : Mes que fue publicado.

"Post.Weekday" : Dia en que el post fue publicado.

"Post.Hour": Hora en que fue publicado.

"Paid": Si la compañia pago a Facebok por la publicación.

"Lifetime.Post.Total.Reach" : La cantidad de personas que vieron una publicación de página. (usuarios únicos)

"Lifetime.Post.Total.Impressions" : Las impresiones son la cantidad de veces que una publicación de una página se muestra, si la publicación es haga clic o no. La gente puede ver múltiples impresiones del mismo post. Por ejemplo, alguien puede ver una actualización de página en Noticias alimente una vez, y luego una segunda vez si un amigo lo comparte.

"Lifetime.Engaged.Users": La cantidad de personas que hicieron clic en cualquier lugar en una publicación (usuarios únicos).

"Lifetime.Post.Consumers" : La cantidad de personas que hicieron clic en cualquier lugar en una publicación. (**TARGET**)

"Lifetime.Post.Consumptions" : La cantidad de clics en cualquier lugar de una publicación.

"Lifetime.Post.Impressions.by.people.who.have.liked.your.Page": Número total de impresiones solo de personas a quienes les ha gustado una página.

"Lifetime.Post.reach.by.people.who.like.your.Page" : La cantidad de personas que vieron una publicación de página porque les ha gustado esa página (usuarios únicos).

"Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post" : La cantidad de personas a las que les ha gustado una página y haga clic en cualquier parte de una publicación (Único usuarios).

"comment": Número de comentarios en la publicación.

"like": Número de likes en la publicación.

"share": Número de veces que la publicación fue compartida.

"Total.Interactions": La suma de comment+like+share.

# Cargar los Datos:

```{r}
FB<-read.csv(file="Fb_limpio.csv",header = TRUE)
#Cambiamos los datos que son categoricos a factores
FB$Type <-factor(FB$Type) 
FB$Cat<-factor(FB$Cat)
FB$Paid <-factor(FB$Paid)
#Vemos la estructura
str(FB) #resumen de la informacion
colnames(FB) #El nombre de columnas modificado
```

## Revisamos los nulos

```{r}
a<-sapply(FB,function(x)sum(is.na(x))) 
print(a) #No existen nulos porque fueron eliminados (eran menos del 1%)
```

## Estadisticas Descriptivas

Revisamos la dimension y las filas superiores e inferiores del dataframe.

```{r}
dim(FB)
head(FB, n=6)
tail(FB, n=6)
```
## Creamos un resumen estadistico personalizado:

```{r}
estadistico<-function(x){
    Valores<-round(c(min(x,na.rm=TRUE),quantile(x,probs=0.25,na.rm=TRUE),
             median(x,na.rm=TRUE),mean(x,na.rm=TRUE),mean(x,trim=0.10,na.rm=TRUE),
             quantile(x,probs=0.75,na.rm=TRUE),max(x,na.rm=TRUE),
             IQR(x,na.rm=TRUE),sd(x,na.rm=TRUE),skewness(x,na.rm=TRUE)
             ),digits = 2)
}
Estadisticos <- sapply(FB[ ,-c(2,3,7)],estadistico)
rownames(Estadisticos) <- 1:nrow(Estadisticos)
Nombres<-c("Min","Q1","Median","Mean","Trim 10%",
           "Q3","Max","IQR","Sd","As")
Sumario<-cbind(Nombres, Estadisticos)
Sumario<-data.frame(Sumario)

```

## Aplicamos el resumen en nuestra data:

```{r}
knitr::kable(Sumario[ ,1:11],"simple",caption = "Resumen Estadistico de Datos Facebook.")
```

```{r}
knitr::kable(Sumario[ ,c(1,12:ncol(Sumario))],"simple",caption = "Resumen Estadistico de Datos Facebook.")
```

\newpage

##  Correlación entre variables:

```{r}
correlation <-round(cor(FB[ ,-c(2,3,7)],use = "complete.obs"),digits = 2)
correlation <-data.frame(correlation)
```

```{r}
knitr::kable(correlation[ ,1:10 ],"simple",caption = "Matriz de Correlación de atributos.")
```

```{r}
knitr::kable(correlation[ ,11:16],"simple",caption = "Matriz de Correlación de atributos.")
```

\newpage

## Realizamos el Análisis Visual Univariante:

##  1.Histogramas:

```{r}
data2<-FB[ ,-c(2,3,7)]
par(mfrow=c(2,4))
for (i in 1:8){
  hist(data2[ ,i], main = names(data2)[i])
}
data2<-FB[ ,-c(2,3,7)]
par(mfrow=c(2,4))
for (i in 9:16){
  hist(data2[ ,i], main = names(data2)[i])
}
```

\newpage

##  2. Grafico de Densidad:

```{r}
data3 <-na.omit(data2)
par(mfrow=c(2,4))
for (i in 1:8){
  plot(density(data3[ ,i]), main=names(data3)[i]) 
}
par(mfrow=c(2,4))
for (i in 9:16){
  plot(density(data3[ ,i]), main=names(data3)[i])} 
```


\newpage

##  3. Boxplot:

```{r}
par(mfrow=c(2,4))
for (i in 1:8){
  boxplot(data2[ ,i], main=names(data2)[i])
}
par(mfrow=c(2,4))
for (i in 9:16){
  boxplot(data2[ ,i], main=names(data2)[i])
}
```

\newpage

## Realizamos el Análisis Visual Bivariante:

##  1.Grafico de Correlacion

```{r}
data2<-FB[ ,-c(2,3,7)]
corr<-cor(data2, use = "complete.obs")
corrplot(corr, method ="circle")
```

Se determino lo mismo que se obtuvo lineas arriba en la parte de correlacion.

##  2.Matriz de Dispersion

```{r}
par(mar=c(1,1,1,1)) 
pairs(FB,cex=0.1)
```

\newpage


# Se determino lo siguiente en esta parte:

  La información no presenta valores NA (solo 4,2,1 _not availabe_ en 3 variables).
  
  Las variables en su mayoría presenta alta asimetría, es decir sesgo, es posible que sea por valores anómalos o posible distribución exponencial de la informacion en algunas variables, en otra puede ser por la naturaleza de las mismas.
  
  Se encontraron algunas variables que tienen una distribución al parecer normal que pueden ser muy útiles al momento del ML.
  
  Se determinó que el objetivo está sesgado.
  
  La mayoría de variables se correlacionan entre sí, solamente **like**, **share** e **TI** se visualizan en la matriz de dispersión con una relación **lineal muy marcada**.


# Feature Selection e Importance - Fase de Preprocesamiento y Tratamiento de Datos

No es necesario poner todo el proceso realizado debido a que es sumamente largo, lo que si es recomendable citar es que aplicando las tecnicas de _Feature Selection_ (Coeficiente de Correlacion, eliminacion de caracteristicas redundantes) e _Feature Importance_ (Decisiontree, RandomForest, Eliminacion recursiva de atrubutos) se determino que las variables **TI, PRLP, PILP, PLPEP** debieron ser eliminadas, además los procesos que generan cambios a mejora en el modelado probando diferentes algoritmos ya sean de **taxonomia lineal, no lineal u ensamblados** son la _estandarizacion_ (`scale y center`) y transformacion `Yeo-Johnson` que dan los mejores resultados.

```{r}
#Feature Importance - Selection
FB <-FB[ ,c(1:12,16:18)]
```


# Evaluacion de Algoritmos:

Debido a que ya se probaron una cantidad grande de algorimos acorde a los valores obtenidos en el proceso de trabajo los mejores resultados se obtienen con los de **taxonomia lineal** entre estos `regresion lineal` y `regresion regularizada` por otro lado los de **ensamblado** especificamente los **boosting** entre estos `gradient boosting machine` y `cubist` son los mejores, por lo cual en esta parte compararemos a todos estos. Para ellos usaremos para tódos la métrica _validacion cruzada con repeticiones_.


```{r}
#Primero definimos:
trainControl01<-trainControl(method = "repeatedcv", number=5, repeats=3)
seed<-7
metric<-"MAE" 
#Creando los modelos para analizar:

#LiR:
set.seed(seed)
fit.lm <- train(PConsume~., data=FB, method="lm",metric=metric,
                preProcess=c("center","scale","YeoJohnson"), trControl=trainControl01)
#RR:
set.seed(seed)
fit.glmnet <- train(PConsume~., data=FB, method="glmnet",
                    metric=metric, preProcess=c("center","scale","YeoJohnson"),
                    trControl=trainControl01)
#GBM:
set.seed(seed)
fit.gbm<-train(PConsume~., data=FB, method="gbm", metric=metric,preProcess=c("center","scale","YeoJohnson"),
             trControl=trainControl01,verbose=FALSE)

#Cubist:
set.seed(seed)
fit.c<-train(PConsume~., data=FB, method="cubist", metric=metric,
             preProcess=c("center","scale","YeoJohnson"),
             trControl=trainControl01)

```

## Sumario de Resultados

```{r}
BestMLResults <- resamples(list(LiR=fit.lm,RR=fit.glmnet, gbm=fit.gbm, cubist=fit.c))
summary(BestMLResults)
dotplot(BestMLResults)
```

# Tunning algorithmo Cubist

Debido a que se realizo la fase de Forecasting anteriormente se tiene como conocimiento previo una idea de los valores de `committees` y `neighbors` para el algoritmo **Cubist**

```{r}
#trainControl01
#metric = "MAE"
set.seed(seed)
grid <- expand.grid(.committees=seq(1,20, by=1),.neighbors=c(6,7,8,9))
tune.cubist <-train(PConsume~., data=FB, method="cubist", metric=metric, preProc =c("scale","center","YeoJohnson"), tuneGrid=grid, trControl=trainControl01)
print(tune.cubist)
plot(tune.cubist)
```


# Fase de Forecasting y Finalizacion

Ahora que sabemos que el mejor modelo para nuestro conjunto de datos Facebook es **Cubist** realizaremos nuevas predicciones sobre datos no etiquetados y guardaremos y cargaremos el modelo para produccion, además en el Tunning obtuvimos `committees = 20` and `neighbors = 9`

```{r}
library(Cubist)
#Preparamos la data FB transformada para entrenamiento
set.seed(seed)
x<-FB[ ,c(1:10,12:15)]
y<-FB[ ,11]
preprocessParams0<-preProcess(x, method=c("scale","center","YeoJohnson"))
transX <- predict(preprocessParams0, x)
# Entrenamiento final del modelo
finalModelCubist<-cubist(x=transX, y=y, committees=20, neighbors=9)
#summary(finalModelCubist)
```


```{r}
#Transformar el conjunto de datos para validacion
set.seed(seed)
validationIndex <- createDataPartition(FB$PConsume, p=0.80, list=FALSE)
validation <- FB[-validationIndex, ]
train <- FB[validationIndex, ]

valX<-validation[ ,c(1:10,12:15)]
trans_valX<-predict(preprocessParams0, valX)
valY <-validation[ ,11]

#Usamos el modelo para predecir
predictions <- predict(finalModelCubist, newdata=trans_valX, neighbors=9)

#Calcular el RMSE, MAE y R-squared:
rmse<-RMSE(predictions, valY)
mae <- MAE(predictions, valY)
r2<-R2(predictions, valY)
print(rmse)
print(mae)
print(r2)
```
## Guardar y Cargar

```{r}
saveRDS(finalModelCubist, "./finalModelCubist.rds")
superModel <- readRDS("./finalModelCubist.rds")
print(superModel)
```


# Conclusiones

* El algoritmo que mejor se ajusta a nuestros datos dando los menores valores de **RMSE**, **MAE** y mejor **R-squared** es el `Cubist`.

* El análisis comprende un todo es decir cada "fase del modelado" (desde la carga de data hasta el grabado del modelo) se debe llevar cuidadosamente entendiendo los resultados  que se obtienen y revisando cada paso dando un tiempo determinado de dedicación.

# Recomendaciones

* Algunos modelos como el `Gradient Boosting Machine` podria dar iguales o mejores resultados que el _Cubist_ esto dependera de la fase Tunning la cual no se realizo para este algoritmo.

* El modelado predictivo del SML es un trabajo de prueba error además del conocimiento de todas las caracteristicas de nuestro data set lo cual recae en un analisis descriptivo inicial riguroso que nos llevara a comprender los datos para así encontrar el mejor modelo y preparar este para su produccion.








