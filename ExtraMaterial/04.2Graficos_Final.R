                                    ###Graficos mejorados con Rstudio###

getwd()

#Primero debemos cargar todas las librerias:

library(chron)
library(psych)
library(nortest)
library(ggplot2)
library(ggmap)
library(NADA)
library(MASS)
library(readxl)
library(car)
library(plyr)
library(dplyr)
library(plotly)
library(gridExtra)
library(ggpmisc)
library(ggpubr) 
library(ggrepel)
library(BoutrosLab.plotting.general)
library(ggridges)

#En esta parte trabajaremos con la data mtcars que está por default en el software
mtcars
str(mtcars)

#mtcars
#From datasets v3.6.2
#by R-core R-core@R-project.org
#99.99th
#Percentile

#Motor Trend Car Road Tests
#The data was extracted from the 1974 Motor Trend US magazine,
#and comprises fuel consumption and 10 aspects of automobile 
#design and performance for 32 automobiles (1973--74 models).

#Keywords
#datasets
#Usage
#mtcars
#Note
#Henderson and Velleman (1981) comment in a
#footnote to Table 1: 'Hocking [original transcriber]'s 
#noncrucial coding of the Mazda's rotary engine
#as a straight six-cylinder engine and the Porsche's 
#flat engine as a V engine, as well as the inclusion 
#of the diesel Mercedes 240D, have been retained to enable
#direct comparisons to be made with previous analyses.'

#Format
#A data frame with 32 observations on 11 (numeric) variables.

#[, 1]	mpg	Miles/(US) gallon
#[, 2]	cyl	Number of cylinders
#[, 3]	disp	Displacement (cu.in.)
#[, 4]	hp	Gross horsepower
#[, 5]	drat	Rear axle ratio
#[, 6]	wt	Weight (1000 lbs)
#[, 7]	qsec	1/4 mile time
#[, 8]	vs	Engine (0 = V-shaped, 1 = straight)
#[, 9]	am	Transmission (0 = automatic, 1 = manual)
#[,10]	gear	Number of forward gears

str(mtcars)

#Suponemos que Cylinder no esta en factor, convertimos:
mtcars$cyl = factor(mtcars$cyl)
str(mtcars)

#Funcion qplot - Bidimensional:
qplot(mpg, wt, data=mtcars)
qplot(mpg, wt, data=mtcars, colour=cyl)
qplot(mpg, wt, data=mtcars, size=cyl, colour=mpg)

qplot(mpg, wt, data=mtcars, facets=vs~cyl)

summary(mtcars$vs)
summary(mtcars$cyl)

#ggplot
ggplot(mtcars, aes(sample=mpg))+
  stat_qq()+
  geom_qq_line()
#qplot
a<-qplot(sample=mpg, data=mtcars, shape=cyl) 
a+geom_qq_line()

a+scale_shape_manual(values=c(1,17,19))
a+scale_color_manual(values=c("red","green","blue")) #No tengo creado un color para el grafico
a+scale_fill_manual(values=c("red"))


qplot(sample=mpg, data=mtcars, color=cyl)
qplot(sample=mpg, data=mtcars, color=cyl)+
  scale_color_manual(values = c(rgb(255,0,0,maxColorValue = 255),
                                "#E69F00",
                                "#56B4E9"))

# https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf
# https://rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf

qplot(sample=mpg, data=mtcars, color=cyl)+
  scale_color_brewer(palette="Pastel1")

qplot(sample=mpg, data=mtcars, color=cyl)+
  scale_color_discrete()+theme_classic()

b<-qplot(sample=mpg, data=mtcars, facets=.~vs, colour=cyl)+
  labs(title="Miles por galon acorde al peso",
       y = "Miles US galones")
b+theme_classic()
b+theme(legend.position="top")
b+theme(legend.position="bottom")
b+theme(legend.position="none")

#Usar un 95% de confianza como banda debemos tener
#paquete ggpubr

qqPlot(mtcars$mpg)

ggplot(mtcars, x="mpg",
       color="cyl",
       palette=c("red","blue","green"),
       ggtheme=theme_pubclean())

create.qqplot.fit.confidence.interval(mtcars$mpg,
                                      distribution = qnorm,
                                      conf=0.95,
                                      reference.line.method = "quartiles")
tmp.x <- rnorm(100)
tmp.confidence.interval <- create.qqplot.fit.confidence.interval(tmp.x)
qqnorm(tmp.x)
qqline(tmp.x)
lines(tmp.confidence.interval$z,
      tmp.confidence.interval$upper.pw,
      lty=2,
      col="brown")
lines(tmp.confidence.interval$z,
      tmp.confidence.interval$lower.pw,
      lty=2,
      col="brown")
lines(tmp.confidence.interval$z[tmp.confidence.interval$u],
      tmp.confidence.interval$upper.sim, lty=2, col="blue")
lines(tmp.confidence.interval$z[tmp.confidence.interval$l],
      tmp.confidence.interval$lower.sim, lty=2, col="blue")

#ECDF (Funcion de densidad acumulada empirica) reporta para cualquier numero
#el porcentaje individual que tiene debjao de una tendencia

m<-ggplot(mtcars, aes(x=mpg))+
  stat_ecdf(aes(color=cyl),geom="point",size=1.5)+
  scale_color_manual(values=c("red","blue","green"))+
  labs(y="f(mpg)")

m<-ggplot(mtcars, aes(x=mpg))+
  stat_ecdf(aes(color=cyl,linetype=cyl))+
  scale_color_manual(values=c("red","blue","green"))+
  labs(y="f(mpg)")

#Densidad ridgeline plot:
#paquete ggridges

theme_set(theme_ridges())
str(iris)

ggplot(iris, aes(x=Petal.Length,y=Species))+
  geom_density_ridges(aes(fill=Species))+
  scale_fill_manual(values=c("red","green","blue"))

ggplot(iris, aes(x=Petal.Length,y=Species))+
  geom_density_ridges(aes(fill=Species),scale=4)+
  scale_fill_manual(values=c("red","green","blue"))
View(iris)

#Lincoln_weather
View(lincoln_weather)
colnames(lincoln_weather)

ggplot(lincoln_weather,aes(x=`Mean Temperature [F]`,y=`Month`))+
geom_density_ridges_gradient(aes(fill=..x..),
                             scale=3, size=0.3)+
  scale_fill_gradientn(colours = c("blue","yellow","red"),
                       name = "Temp. [F]")+
  labs(title="Temperaturas de Lincoln NE")

#Para ver mas ejemplos
browseVignettes("ggridges")

# Modern Graphics

#### Scatter plots ####

theme_set(theme_gray())

ggplot(mtcars, aes(x=wt,y=mpg))+
  geom_point()

ggplot(mtcars, aes(x=wt,y=mpg))+
  geom_point(size=4,shape=6)

ggplot(mtcars, aes(x=wt,y=mpg))+
  geom_point(aes(size=qsec))
View(mtcars)


ggplot(mtcars, aes(x=wt,y=mpg))+
  geom_point()+
  geom_text(label=rownames(mtcars))

ggplot(mtcars, aes(x=wt,y=mpg))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)
  
ggplot(mtcars, aes(x=wt,y=mpg))+
  geom_point()+
  geom_smooth(method=lm, se=TRUE)  
  
ggplot(mtcars, aes(x=wt,y=mpg))+
  geom_point()+
  geom_smooth()  

ggplot(mtcars, aes(x=wt,y=mpg))+
  geom_point(shape=18, color="blue")+
  geom_smooth(method = lm, se=TRUE, linetype="dashed",
              color="red",fill="green")


# Multiples grupos

ggplot(mtcars, aes(x=wt, y=mpg, shape=cyl))+
  geom_point(size=3)

ggplot(mtcars, aes(x=wt, y=mpg, shape=cyl, color=cyl))+
  geom_point(size=3.5)

ggplot(mtcars, aes(x=wt, y=mpg, shape=cyl, color=cyl, size=cyl))+
  geom_point()

#Lineas de Regresion:

ggplot(mtcars, aes(x=wt, y=mpg, color=cyl, shape=cyl))+
  geom_point()+
  geom_smooth(method=lm)

ggplot(mtcars, aes(x=wt, y=mpg, color=cyl, shape=cyl))+
  geom_point()+
  geom_smooth(method=lm)

ggplot(mtcars, aes(x=wt, y=mpg, color=cyl, shape=cyl))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

ggplot(mtcars, aes(x=wt, y=mpg, color=cyl, shape=cyl))+
  geom_point()+
  geom_smooth(method=lm)+
  scale_shape_manual(values=c(3,6,17))+
  scale_color_manual(values=c("red","green","blue"))+
  theme(legend.position = "top")

mtcars$vs = factor(mtcars$vs)
mtcars
ggplot(mtcars, aes(x=wt, y=mpg, color=cyl, shape=cyl))+
  geom_point(aes(size=vs))+
  geom_rug()eom_smooth(method=lm)+
  scale_shape_manual(values=c(3,6,17))+
  scale_color_manual(values=c("red","green","blue"))+
  theme(legend.position = "top")


#Podemos generar una alfombra de numeros:

ggplot(mtcars, aes(x=wt,y=mpg))+
  geom_point(size=3.5)+
  geom_rug()

ggplot(mtcars, aes(x=wt,y=mpg, color=cyl))+
  geom_point(aes(shape=vs), size=3.5)+
  geom_rug()

str(faithful)
View(faithful)
?faithful

ggplot(faithful, aes(x=eruptions, y=waiting))+
  geom_point()

ggplot(faithful, aes(x=eruptions, y=waiting))+
  geom_point()+
  geom_rug()

#Scatter Plot con estimador densidad 2D:

sp <- ggplot(faithful, aes(x=eruptions, y=waiting))+
  geom_point()

sp+geom_density2d()

sp+stat_density2d(aes(fill=..level..), geom="polygon")

sp+stat_density2d(aes(fill=..level..), geom="polygon")+
  scale_fill_gradient(low="blue",high="red")
ggplotly()

#Scatter Plot con binds rectangulares

head(diamonds)
str(diamonds)
p<-ggplot(diamonds, aes(x=carat, y=price))
p+geom_bin2d()

p+geom_bin2d(bins=10)
p+geom_bin2d(binwidth=c(1,1000))

#Scatterplot con funcion de densidad marginal:

set.seed(1234)
x<-c(rnorm(n=500, mean=-1), rnorm(n=500, mean=1.5))
y<-c(rnorm(n=500,mean=-1), rnorm(n=500, mean=1.7))
group <- as.factor(rep(c(1,2),each=500))

df<-data.frame(x,y,group)
head(df)

scatterPlot <- ggplot(df, aes(x,y, color=group))+
               geom_point()+
  theme(legend.position = c(0,1),
        legend.justification = c(0,1))

##Marginal density plot of x (top panel):
xdensity<-ggplot(df, aes(x,fill=group))+
  geom_density(alpha=0.5)+
  scale_fill_manual(values=c("#999999","#E69F00"))+
  theme(legend.position = "none")

##Marginal density plot of y (right panel):

ydensity<-ggplot(df, aes(y,fill=group))+
  geom_density(alpha=0.5)+
  scale_fill_manual(values=c("#999999","#E69F00"))+
  theme(legend.position = "none")

#Un formato de plot:

blankPlot<-ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

grid.arrange(xdensity, blankPlot,scatterPlot, ydensity,
             ncol=2, nrow=2, widths=c(4,1.4),
             heights=c(1.4,4))









  






