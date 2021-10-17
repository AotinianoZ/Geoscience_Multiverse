.libPaths(c("D:/R_Packages",.libPaths()))

####WILCOXON####
library(plotly)
library(ggplot2)
library("ggpubr")

# Creando la data.:

set.seed(1234)
my_data <- data.frame(
  name = paste0(rep("M_", 10), 1:10),
  weight = round(rnorm(10, 20, 2), 1)
)

# Print the first 10 rows of the data
head(my_data, 10)

# Statistical summaries of weight
summary(my_data$weight)

# Visualizacion:
library(ggpubr)
ggboxplot(my_data$weight, 
          ylab = "Weight (g)", xlab = FALSE,
          ggtheme = theme_minimal())

# Queremos saber si el promedio de peso de ratones difiere
# de 25 g (two-tailed test)

# One-sample wilcoxon test
res <- wilcox.test(my_data$weight, mu = 25)
# Printing the results
res 
# print only the p-value
res$p.value

# El p-valor del test es 0.005793045 el cual es menor
#que el nivel de significancia alfa = 0.05. Podemos rechazar
# la hipótesis nula y concluir que el promedio de pesos
# de los ratones es significantemente diferente de 25g con
# un p-valor = 0.005793045.

# Si queremos testear que la media de pesos de los ratones
# es menor que 25g (one-tailed test):

wilcox.test(my_data$weight, mu = 25,alternative = "less")

# Si queremos testear que la media de pesos de los ratones
# es mayor que 25g (one-tailed test):

wilcox.test(my_data$weight, mu = 25,aternative = "greater")

####U MANN-WHITNEY####

# Data in two numeric vectors
women_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
men_weight <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4) 
# Create a data frame
my_data <- data.frame( 
  group = rep(c("Woman", "Man"), each = 9),
  weight = c(women_weight,  men_weight)
)


#Queremos saber si la mediana de los pesos de mujeres difiere de la mediana de los pesos de hombres.

print(my_data)

# Es posible calcular sumario estadistico (mediana y rango intercuartil (IQR)) por grupos)
# El paquete `dplyr` puede ser usado:

library(dplyr)

#Sumario por grupos:

group_by(my_data, group) %>%
  summarise(
    count = n(),
    median = median(weight, na.rm=TRUE),
    IQR = IQR(weight, na.rm=TRUE)
  )

#Visualizacion de la data usango boxplot:
library(ggplot2)
library(ggpubr)
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "Weight", xlab = "Groups")

#Existe diferencia significativa entre peso de mujeres y hombres:

res <- wilcox.test(women_weight,men_weight)
res

#Este calulo podria dar un mensaje de advertencia, diciendo que "no se 
#puede computar el valor exacto de p con amarres". Esto proviene desde que se asume
#que el Wilcoxon tiene respuesta continua. Se puede suprimir este mensaje
#adicionando el argumento `exact=FALSE`, pero el resultado sera el mismo.


res <- wilcox.test(weight ~ group, data = my_data,
                   exact = FALSE)
res

# Print the p-value only
res$p.value

# El p-valor del test es 0.02712 el cual es menor que el nivel
# de significancia alfa=0.05. Nosotros podemos concluir que
# la mediana del peso es significantemente diferente de la
# mediana del peso con un p-valor = 0.02712.


# Nota:

# Si quieres ver el test si la mediana del peso  de hombres es menor que
# la mediana de hombres:

wilcox.test(weight ~ group, data = my_data,exact = FALSE, alternative = "less")

# Si queremos ver que la mediana del peso de hombres es mayor que la mediana
# de mujeres :

wilcox.test(weight ~ group, data = my_data,exact = FALSE, alternative = "greater")


####KRUSKAL-WALLIS####

# Usaremos una data del R:

my_data <- PlantGrowth

# print the head of the file
head(my_data)

# Show the group levels
levels(my_data$group)

# Los niveles no estan en orden hay que ponerlos en orden:

my_data$group <- ordered(my_data$group,
                         levels = c("ctrl", "trt1", "trt2"))
print(my_data)

# Sumario Estadistico:

library(dplyr)
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE),
    median = median(weight, na.rm = TRUE),
    IQR = IQR(weight, na.rm = TRUE)
  )
# Visualizacion:

# Box plots
# Plot weight by group and color by group
library(plotly)
library(ggplot2)
library("ggpubr")
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Weight", xlab = "Treatment")
ggplotly()

# Mean plots
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(my_data, x = "group", y = "weight", 
       add = c("mean_se", "jitter"), 
       order = c("ctrl", "trt1", "trt2"),
       ylab = "Weight", xlab = "Treatment")

# Queremos saer si existen diferencia significativa entre los promedios
# experimentales de las 3 plantas.

kruskal.test(weight ~ group, data = my_data)

# Interpretacion:
# El p-valor<alpha=0.05, podemos concluir que existe diferencias
# significativas entre el tratamientos de los grupos.

# Desde que la salida del test de Kruskal-Wallis nosotros sabemos que 
# existe diferencia significativa entre grupos, pero nosotros no sabemos
# que par de grupos son diferentes.

# Es posible usar la funcion pairwise.wilcox.test() para calcular
# comparacion entre grupos de niveles con correcciones para testeo multiple.

pairwise.wilcox.test(PlantGrowth$weight, PlantGrowth$group,
                     p.adjust.method = "BH")

# La comparacion por pares muestra que solo trt1 y trt2 son significantemente
# diferenetes (p<0.05)


