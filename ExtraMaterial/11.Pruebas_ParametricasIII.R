.libPaths(c("D:/R_Packages",.libPaths()))

####Librerias####
library(tidyverse)
library(ggpubr)
library(rstatix)

#### One-way ANOVA ####

#Usaremos la data construida en R denominada PlantGrowth. Que muestra los pesos de las
#plantas obtenidas durante un control y dos diferentes tratamientos.

my_data<-PlantGrowth

#Revision basica:

#Tomamos una muestra aleatoria:
set.seed(1234)
dplyr::sample_n(my_data,10)

#Nota: En la terminologia de R, la columna "group" es llamada factor y las diferentes
#categorias ("ctr","trt1","trt2")son denominadas niveles de factor. **Estos niveles son 
#ordenados alfabeticamente.

#Mostrar los niveles:

levels(my_data$group)

#Si los niveles no están autimaticamente en el orden correcto, reordenar:

my_data$group<-ordered(my_data$group,
                       levels=c("ctrl","trt1","trt2"))

#Computamos el sumario estadistico por grupos - cantidad, media, sd:

library(dplyr)
group_by(my_data, group) %>%
  summarise(
    count=n(),
    mean=mean(weight, na.rm=TRUE),
    sd = sd(weight, na.rm=TRUE)
  )

#Visualizamos la data:

library(ggpubr)
#Realizaremos Box plots:
#Ploteamos peso por grupo y color por grupo

ggboxplot(my_data, x="group",y="weight",
          color="group", palette=c("#00AFBB","#E7B800","#FC5E07"),
          order = c("ctrl","trt1","trt2"),
          ylab="Peso",xlab="Tratamiento")

#Medianas de Plots:
#Plot de peso por grupos y agregamos barras de error (mean_se),
#otros valores pueden ser, mean_cim mean_iqr,.....

ggline(my_data, x="group",y="weight",
       add=c("mean_se","jitter"),
       order=c("ctrl","trt1","trt2"),
       ylab="Peso",xlab="Tratamiento")

#Si deseamos usar los graficos convencionales de R:

# Box plot
boxplot(weight ~ group, data = my_data,
        xlab = "Treatment", ylab = "Weight",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))
# plotmeans
library("gplots")
plotmeans(weight ~ group, data = my_data, frame = FALSE,
          xlab = "Treatment", ylab = "Weight",
          main="Mean Plot with 95% CI") 

#Nosotros deseamos saber si existe una diferencia significativa entre el 
#promedio de pesos entre las plantes en las 3 condiciones experimentales

#La funcion en r `aov()` puede ser usarada para responder esta pregunta.
#La funcion `summary.aov()` es usada para sumarizar el analisis del 
#modelo de varianza.

# Compute el analisis de varianza:

res.aov <- aov(weight ~ group, data = my_data)

# Resumir el analisis

summary(res.aov)

#La salida incluye la columna F valor y Pr(>F) correspondiente
# al p-valor del test. 

#Interpretando el rsultado de One-Way Anova Test

#Como el p-valor es menor que el nivel de significancia 0.05, nosotros
#podemos conluir que existe diferencias significativas entre los
#grupos resaltado con "*" en el modelo del sumario.


#Multiples pares de comparaciones entre las medias de los grupos.

#En un One-Way Anova Test, un significante p-valor indica que algunos de los grupos son
#de medias diferentes, pero nosotros no conocemos cual de grupos es.
#Es posible llevar a cabo una comparación multiple de pares, para determinar
#si la media diferente entre pares de grupoes es estadisticamente significativa.

#Tukey para comparación multiple de pares:

#Como el test de ANOVA es significante, nosotros podemos coutar
# el **Tukey HSD** (Tukey Honest Significant Differences, R function: **TukeyHSD()**) para
#llevar a cabo la comparación multiple de grupos por pares entre las medias de grupos.

#La función `TukeyHD()` toma el ajuste de ANOVA como argumento:

TukeyHSD(res.aov)

#diff: diferencia entre medias de dos grupos.
#lwr, upr: el menor y superior final del punto del intervalo de confianza al 95% (default)
#p adj: p-valor después del ajuste de multile comparacion.
 
#Podemos ver que desde la salida, la unica diferencia entre trt2 y trt2 es signifcante con un ajuste
#del p-valor de 0.012.












