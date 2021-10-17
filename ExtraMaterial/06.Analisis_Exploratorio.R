.libPaths(c("D:/R_Packages",.libPaths()))

library(lattice)
library(ggplot2)
library(caret) #Mas importante:
library(mlbench)
library(AppliedPredictiveModeling)
library(e1071)
library(Rcpp)
library(Amelia)
library(RCurl)
library(ggplot2)

#Análisis de Data Preliminar (Elegir ela variable objetivo)

# muestras de cobre (.csv)
# Parte I: Analisis Numerico Estadistico Basico
# 1.Estructura de la data:
getwd()
muestras_cu<-read.csv(file="BD/muestras_cu.csv", sep=";",header = TRUE)
str(muestras_cu)

#Comentario: Tenemos un dataframe de 6 variables y 2380 instancias (filas).
# existen tres variables espaciales que son X, Y, Z, además de Leyes de Cobre y Oro y el tipo de roca.
# Se observa valores de Ley de Cobre "consecutivos" y leyes de oro con valor "0" y con mayor variación, 
# el tipo de roca esta como entero pero es un factor.

muestras_cu$Tipo.de.roca<-factor(muestras_cu$Tipo.de.roca)
names(muestras_cu)<-c("X", "Y", "Z", "Cu", "Au", "Tipo_Roca")
str(muestras_cu)

#Ahora podemos verificar que existen siete niveles o tipos de roca.

head(muestras_cu)
tail(muestras_cu)

#La información preliminar de los cabezales y fondos nos muestra valores X, Y, Z no dispersos; la variable
# cobre tienen valors que van de 0.12 a 1.14, mientras que la variable Au va de 0 a 19.045, considerar que 
# los mayores valores parecieran estar en el Tipo_Roca 7.


# 2.Valores vacios:
Vacios<-sapply(muestras_cu,function(x)sum(is.na(x)))
# No se tienen valores nulos en la data.

# 3.Sumario Inicial:
summary(muestras_cu)

#Con el sumario podemos notar que los valores de X e Y no tienen grandes variaciones, aunque inician en 
#valores cercanos al cero (debe haber una referencia de los taladros), la profundidad de taladros empieza en 
# 6 metros y termina en 136 metros con una distribución proporcionada. Los valores de cobre inician en 0.120 y 
# terminan en 7.24 (debemos revisar los valores que están por encima del 3er cuartil (ore?)), para el oro
# los valores inician en 0, cumpliendose algo similar a los valores de Cu por encima del 3er cuartil. El
# tipo de roca mayoriatría es 2 los demás son ampliamente diferentes a menor.

# 4. Medidas de Centralización, Posición, Dispersión y Asimetría (exceptuamos la variable Tipo_Roca)

muestras_cu2<-muestras_cu[ ,-6] #Retiramos la variable Tipo_Roca
  
estadistico<-function(x){
    Valores<-round(c(length(x),min(x,na.rm=TRUE),quantile(x,probs=0.25,na.rm=TRUE),
                   median(x,na.rm=TRUE),mean(x,na.rm=TRUE),mean(x,trim=0.10,na.rm=TRUE),
                   quantile(x,probs=0.75,na.rm=TRUE),max(x,na.rm=TRUE),
                   IQR(x,na.rm=TRUE),sd(x,na.rm=TRUE),skewness(x,na.rm=TRUE),
                   CV=(sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE))*100),digits = 2)
  }
Estadisticos <- sapply(muestras_cu2,estadistico)
rownames(Estadisticos) <- 1:nrow(Estadisticos)
Nombres<-c("n","Min","Q1",
           "Median","Mean","Trim 10%",
           "Q3","Max","IQR", "Sd","As","CV")
Sumario<-cbind(Nombres, Estadisticos)
Sumario<-data.frame(Sumario)
print(Sumario)
View(Sumario)

#Al realizar un resumen de las medidas estadísticas principales notamos lo siguiente:
#La variable X ...


# Medidas de asociación #
#Matriz de valores de correlación (caso Pearson - de manera lineal)

correl<-cor(muestras_cu2, method = "pearson")
print(correl)

#

#Plot de la matriz de correlación
library(corrplot)
corrplot(correl, method = "circle", order="hclust")


# Parte II: 
# Análisis Gráfico Univariante

## Boxplot (Verificar los outliers)
par(mfrow=c(1,2))
for(i in 4:5){
  boxplot(muestras_cu2[ ,i],main=names(muestras_cu2)[i])
}

#

## Histogramas
summary(muestras_cu2)
par(mfrow=c(1,2))
for(i in 4:5){
  hist(muestras_cu2[ ,i], main=names(muestras_cu2)[i])
}

#

## Densidad
par(mfrow=c(1,2))
for(i in 4:5){
  plot(density(muestras_cu[ ,i]), main=names(muestras_cu)[i])
}

#

## Variable Cualitativa:

## Barplot (variables categoricas)
for (i in 6){
  counts<-table(muestras_cu[ ,i])
  names <-names(muestras_cu)[i]
  barplot(counts, main=names)
}

# Análisis Gráfico Multivariante:

#Matriz de Disperion (X vs Y)
pairs(muestras_cu2)
colnames(muestras_cu)
#Matriz de dispersion por clases
pairs(Tipo_Roca~.,muestras_cu,col=muestras_cu$Tipo_Roca)

#Matriz de Densidad por Clase
x<-muestras_cu[ ,1:5]
y<-muestras_cu[ ,6]
scale<-list(x=list(relation ="free"),y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scale)

#Boxplot por Clase
x<-copia_cu[ ,1:2]
y<-copia_cu[ ,3]
featurePlot(x=x, y=y, plot="boxplot")

#Usaremos GGPLOT(solo demostracion basica ustedes deben probar mas)
library(plotly)

plot_ly(x=muestras_cu$X, y=muestras_cu$Y, z=muestras_cu$Z, type="scatter3d", mode="markers",
        color=muestras_cu$Cu)

#Esto lo podran desarrollar ustedes con paquetes geoestadisticos (en otro nivel)

b<-ggplot(muestras_cu, aes(x=Cu, y=Au))
b+geom_point()+
  facet_wrap(~Tipo_Roca)
library(ggExtra)
library(ggpubr)
ggscatterhist(muestras_cu,x="Cu",y="Au",color="Tipo_Roca",
              margin.plot = "boxplot",
              ggtheme = theme_bw())

#Ver qqplot:
ggplot(muestras_cu2,aes(sample=Cu))+
  stat_qq()+
  geom_qq_line()
ggplot(muestras_cu2,aes(sample=Au))+
  stat_qq()+
  geom_qq_line()
ggplot(muestras_cu2,aes(sample=Z))+
  stat_qq()+
  geom_qq_line()

#Solo ver Cu:
qplot(sample=Cu,data=muestras_cu,color=Tipo_Roca)
qqPlot(muestras_cu$Cu)
#Identify (buscar)
ggqqplot(muestras_cu,x="Cu",
         color="Tipo_Roca", 
         ggtheme=theme_pubclean())
#Solo ver Au:
qplot(sample=Au,data=muestras_cu,color=Tipo_Roca)
qqPlot(muestras_cu$Au)
#Ver por Tipo de Roca:
#Cu:
qplot(sample=Cu,data=muestras_cu,facets = .~Tipo_Roca)+
  labs(title="Cu por tipo de Roca",
       y="Cu (ppm)")
#Au:
qplot(sample=Au,data=muestras_cu,facets = .~Tipo_Roca)+
  labs(title="Au por tipo de Roca",
       y="Au (ppm)")

#Ver ECDF de Cu:
m<-ggplot(muestras_cu,aes(x=Cu))+
  stat_ecdf(aes(color=Tipo_Roca,linetype=Tipo_Roca),
            geom ="step",size=1.2)

#Library Ridges:
ggplot(muestras_cu,aes(x=Cu, y=Tipo_Roca))+
  geom_density_ridges(aes(fill=Tipo_Roca))

#Graficar Cu acorde a valor:
ggplot(muestras_cu,aes(x=Cu,y=Au))+
  geom_point(aes(size=Z))
#Ver donde se concetra el Cu por Tipo:
ggplot(muestras_cu,aes(x=Cu,y=Au,color=Tipo_Roca))+
  geom_point(size=3.5)+
  geom_rug()

#Correlación Lineal:

ggscatter(muestras_cu,x="Cu",y="Au",
          add="reg.line", conf.int = TRUE,
          add.params = list(fill="lightgray"),
          ggtheme = theme_minimal())+
  stat_cor(method = "pearson",
           label.x =3, label.y=30 )

#Relaciones
b<-ggplot(muestras_cu, aes(x=Cu, y=Au))
b+geom_point(aes(color=Tipo_Roca,shape=Tipo_Roca))+
  stat_ellipse(aes(color=Tipo_Roca),type = "t")

#Color por Cu:
b+geom_point(aes(color=Cu),size=4)+
  scale_color_gradientn(colors = c("#00AFBB","#E7B800","#FC4E07"))+
  theme(legend.position = "top")

#Create a scatter plot
p<-ggplot(muestras_cu,aes(Cu,Au))+
  geom_point(aes(color=Tipo_Roca),size=3, alpha=0.6)+
  scale_color_manual(values = c("brown","red","green","pink","black","gray","blue"))

#Add density distribution as marginal plot:
#First install package ggExtra, then load the package:
library("ggExtra")
ggMarginal(p,type="density")

#Change the marginal plot type
ggMarginal(p,type = "densigram")
ggMarginal(p,type="boxplot")

#For multiple groups in the scatter plot and the marginal plots:
library(ggpubr)
#Grouped Scatter plot with marginal density plots
ggscatterhist(muestras_cu, x="Cu",y="Au",
              color="Tipo_Roca",size=3,alpha=0.6,
              palette = c("brown","red","green","pink","black","gray","blue"),
              margin.params = list(fill="Tipo_Roca",color="black",size=0.2))
#Use box plot as marginal plots:
ggscatterhist(muestras_cu, x="Cu",y="Au",color="Tipo_Roca",
              palette = c("brown","red","green","pink","black","gray","blue"),
              margin.plot = "boxplot",
              ggtheme = theme_bw())

#Polynomial regression. Sow equation and adjusted R2. (descubranlo)
formula2<-Au~poly(Cu,3,raw = TRUE)
p<-ggplot(muestras_cu,aes(Cu,Au,color=Tipo_Roca))+
  geom_point()+
  geom_smooth(aes(fill=Tipo_Roca),method="lm",formula=formula2)+
  stat_poly_eq(
    aes(label=paste(..eq.label.., ..adj.rr.label..,sep="~~~~")),
    formula=formula2,parse=TRUE
  )
ggpar(p, palette = "jco")
