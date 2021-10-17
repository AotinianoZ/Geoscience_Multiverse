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

#Convertir cyl desde un numérico a un factor variable:

mtcars$cyl=factor(mtcars$cyl)
mtcars$mpg
qplot(sample=mpg, data=mtcars)

qplot(mpg, wt, data = mtcars)
qplot(mpg, wt, data = mtcars, colour = cyl)
qplot(mpg, wt, data = mtcars, size = cyl, colour=mpg)
qplot(mpg, wt, data = mtcars, facets = vs ~ cyl)

ggplot(mtcars,aes(sample=mpg))+
  stat_qq()+
  geom_qq_line()

a<-qplot(sample=mpg,data=mtcars,shape=cyl)

a+scale_shape_manual(values=c(1,17,19))
a+scale_color_manual(values=c("red","green","blue")) #Ojo
a+scale_fill_manual(values=c("red")) #Ojo

qplot(sample=mpg,data=mtcars,color=cyl)
qplot(sample=mpg,data=mtcars,color=cyl)+scale_color_manual(values=c("#999999",
                                                                    "#E69F00",
                                                                    "#56B4E9"))

qplot(sample=mpg,data=mtcars,color=cyl)+scale_color_brewer(palette="Dark2")

qplot(sample=mpg,data=mtcars,color=cyl)+scale_color_grey()+theme_classic()

qplot(sample=mpg,data=mtcars,facets = .~vs)+
  labs(title="Miles por galón \n acorde al peso",
       y="Miles/(US) galón")

b<-qplot(sample=mpg,data=mtcars,color=cyl,shape=cyl)+
  labs(title="Miles por galón \n acorde al peso",
       y="Miles/(US) galón")

b+theme_classic()
b+theme(legend.position="top")
b+theme(legend.position = "bottom")       
b+theme(legend.position = "none")       

#Para usar con 95% de confianza en banda debemos usar:
#instalar ggpubr
install.packages("ggpubr")
install.packages("BoutrosLab.plotting.general")
library(ggplot2)
library(ggpubr)
library(car)

qqPlot(mtcars$mpg)

ggqqplot(mtcars,x="mpg",
         color="cyl", 
         palette=c("red","blue","green"),
         ggtheme=theme_pubclean())

create.qqplot.fit.confidence.interval(mtcars$mpg, 
                                      distribution = qnorm, conf = 0.95,
                                      conf.method = "both",
                                      reference.line.method = "quartiles")
tmp.x <- rnorm(100);
tmp.confidence.interval <- create.qqplot.fit.confidence.interval(tmp.x);
qqnorm(tmp.x);
qqline(tmp.x);
lines(tmp.confidence.interval$z, tmp.confidence.interval$upper.pw, lty = 2, col = "brown");
lines(tmp.confidence.interval$z, tmp.confidence.interval$lower.pw, lty = 2, col = "brown");
lines(tmp.confidence.interval$z[tmp.confidence.interval$u], 
      tmp.confidence.interval$upper.sim, lty = 2, col = "blue");
lines(tmp.confidence.interval$z[tmp.confidence.interval$l], 
      tmp.confidence.interval$lower.sim, lty = 2, col = "blue");
legend(1, -1.5, c("simultaneous", "pointwise"), col = c("blue", "brown"), lty = 2, bty = "n")

#ECDF 
#Veremos también la función de densidad acumulada empírica (ECDF) reporta
#para cualquier número el porcentaje individual que tienen debajo de una 
#tendencia

m<-ggplot(mtcars,aes(x=mpg))+
  stat_ecdf(aes(color=cyl),
            geom ="point",size=1.5)+
  scale_color_manual(values=c("red","blue","green"))+
  labs(y="f(mpg)")

m<-ggplot(mtcars,aes(x=mpg))+
  stat_ecdf(aes(color=cyl,linetype=cyl),
            geom ="step",size=1.2)+
  scale_color_manual(values=c("red","blue","green"))+
  labs(y="f(mpg)")
ggplotly()

#Densidad ridgeline plot:
install.packages("ggridges")
library(ggplot2)
library(ggridges)
theme_set(theme_ridges())
View(iris)
ggplot(iris,aes(x=Sepal.Length, y=Species))+
  geom_density_ridges(aes(fill=Species))+
  scale_fill_manual(values=c("#00AFBB","#E7B800","#FC4E07"))
ggplot(iris,aes(x=Sepal.Length, y=Species))+
  geom_density_ridges(aes(fill=Species),scale=1)+
  scale_fill_manual(values=c("#00AFBB","#E7B800","#FC4E07"))  

#Datase
View(lincoln_weather)
colnames(lincoln_weather)

ggplot(lincoln_weather,aes(x = `Mean Temperature [F]`, y = `Month`)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
  scale_fill_gradientn(colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
    name = "Temp. [F]")+
  labs(title = 'Temperatures in Lincoln NE')

#To see more examples run the next code:
browseVignettes("ggridges")

#Modern Graphics  

####Scatter plots####


#Usaremos la data mtcars para explicar los tipos diferentes de boxplot
View(mtcars)
str(mtcars)
mtcars$cyl<-as.factor(mtcars$cyl)
head(mtcars)

ggplot(mtcars,aes(x=wt,y=mpg))+
  geom_point()

ggplot(mtcars,aes(x=wt,y=mpg))+
  geom_point(size=2,shape=23)

ggplot(mtcars,aes(x=wt,y=mpg))+
  geom_point(aes(size=qsec))

ggplot(mtcars,aes(x=wt,y=mpg))+
  geom_point()+
  geom_text(label=rownames(mtcars))


ggplot(mtcars,aes(x=wt,y=mpg))+
  geom_point()+
  geom_smooth(method = lm)

ggplot(mtcars,aes(x=wt,y=mpg))+
  geom_point()+
  geom_smooth(method = lm,se=FALSE)

ggplot(mtcars,aes(x=wt,y=mpg))+
  geom_point()+
  geom_smooth()

ggplot(mtcars,aes(x=wt,y=mpg))+
  geom_point(shape=18, color="blue")+
  geom_smooth(method = lm, se=FALSE, linetype="dashed",
              color="darkred",fill="blue")

ggplot(mtcars,aes(x=wt,y=mpg))+
  geom_point(shape=18, color="blue")+
  geom_smooth(method = lm, se=TRUE, linetype="dashed",
              color="darkred",fill="blue")

#Working with multiples groups

ggplot(mtcars, aes(x=wt,y=mpg,shape=cyl))+
  geom_point()
ggplot(mtcars, aes(x=wt,y=mpg,shape=cyl,color=cyl))+
  geom_point(size=3.5) 
ggplot(mtcars,aes(x=wt,y=mpg,shape=cyl,color=cyl,size=cyl))+
  geom_point(n=3.5)

#Regression Lines:

ggplot(mtcars,aes(x=wt,y=mpg,color=cyl,shape=cyl))+
  geom_point()+
  geom_smooth(method=lm)

ggplot(mtcars,aes(x=wt,y=mpg,color=cyl,shape=cyl))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE,fullrange=TRUE)
ggplot(mtcars,aes(x=wt,y=mpg,color=cyl,shape=cyl))+
  geom_point()+
  geom_smooth(method=lm,se=TRUE,aes(fill=cyl))

ggplot(mtcars,aes(x=wt,y=mpg,color=cyl,shape=cyl))+
  geom_point()+
  geom_smooth(method = lm,se=FALSE,fullrange=TRUE)+
  scale_shape_manual(values = c(3,6,17))+
  scale_color_manual(values = c("#999999","#E69F00","#56B4E9"))+
  theme(legend.position = "top")

ggplot(mtcars,aes(x=wt,y=mpg,color=cyl,shape=cyl))+
  geom_point(aes(size=cyl))+
  geom_smooth(method = lm, se=FALSE,fullrange=TRUE)+
  scale_shape_manual(values = c(3,6,17))+
  scale_color_manual(values = c("#999999","#E69F00","#56B4E9"))+
  scale_size_manual(values=c(1,3,5))+
  theme(legend.position = "top")

h<-ggplot(mtcars,aes(x=wt,y=mpg,color=cyl,shape=cyl))+
  geom_point()+
  geom_smooth(method=lm,se=FALSE,fullrange=TRUE)+
  theme_classic()
h+scale_color_brewer(palette = "Dark2")  
h+scale_color_grey()  

#Podemos generar alfombra de números

ggplot(mtcars,aes(x=wt,y=mpg))+
  geom_point(size=3.5)+
  geom_rug()

ggplot(mtcars,aes(x=wt,y=mpg,color=cyl))+
  geom_point(size=3.5)+
  geom_rug()

ggplot(faithful,aes(x=eruptions,y=waiting))+
  geom_point()+
  geom_rug()

#Scatter plots with the 2d density estimation

sp<-ggplot(faithful,aes(x=eruptions,y=waiting))+
  geom_point()
sp+geom_density2d()

sp+stat_density2d(aes(fill=..level..), geom="polygon")
sp+stat_density2d(aes(fill=..level..),geom="polygon")+
  scale_fill_gradient(low="blue",high="red")
library(plotly)
ggplotly()

#Scatter plots with rectangular binds

head(diamonds)
p<-ggplot(diamonds,aes(x=carat,y=price))
p+geom_bin2d()
p+geom_bin2d(bins=10)
p+geom_bin2d(binwidth=c(1,1000))


#Scatterplot with marginal density distribution:

set.seed(1234)
x<-c(rnorm(n=500,mean=-1),rnorm(n=500,mean=1.5))
y<-c(rnorm(n=500,mean=-1),rnorm(n=500,mean=1.7))
group<-as.factor(rep(c(1,2),each=500))
df<-data.frame(x,y,group)
head(df)
scatterPlot<-ggplot(df, aes(x,y,color=group))+
  geom_point()+
  theme(legend.position = c(0,1),legend.justification = c(0,1))

#Marginal density plot of x (top panel)
xdensity<-ggplot(df, aes(x, fill=group))+
  geom_density(alpha=0.5)+
  scale_fill_manual(values=c("#999999","#E69F00"))+
  theme(legend.position = "none")

#Marginal density plot of y (right panel)
ydensity<-ggplot(df, aes(y, fill=group))+
  geom_density(alpha=0.5)+
  scale_fill_manual(values=c("#999999","#E69F00"))+
  theme(legend.position = "none")

#Create a blank plot placeholder plot:
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

install.packages("gridExtra")
library(gridExtra)
grid.arrange(xdensity,blankPlot,scatterPlot,ydensity,ncol=2,
             nrow=2,widths=c(4,1.4),heights=c(1.4,4))

#Plot multiple continuous variable:



#Prepare demo data sets.

df<-mtcars
head(df)

b<-ggplot(df, aes(x=wt, y=mpg))
b+geom_point()+
  geom_smooth(method="lm")
b+geom_point()+
  geom_smooth(method = "loess")
#For remove CI you have to use se=FALSE

b+geom_point(shape=18)

#Crete a scatter plot using ggscatter()+stat_cor()

#Recode packages 

devtools::install_github("wilkelab/cowplot")
install.packages("ggpmisc")
library(ggplot2)
library(ggpmisc)
install.packages("backports")
install.packages("broom")
install.packages("ggpubr") 
library(ggpubr) 
df<-mtcars
ggscatter(df,x="wt",y="mpg",
          add="reg.line", conf.int = TRUE,
          add.params = list(fill="lightgray"),
          ggtheme = theme_minimal())+
  stat_cor(method = "pearson",
           label.x =3, label.y=30 )

#Multiple groups

#Change color and shape by groups (cyl)
b+ geom_point(aes(color=cyl,shape=cyl))+
  geom_smooth(aes(color=cyl,fill=cyl),method = "lm")+
  geom_rug(aes(color=cyl))+ #Add marginal rug with geom_rug()
  scale_color_manual(values=c("#00AFBB","#E7B800","#FC4E07"))+
  scale_fill_manual(values=c("#00AFBB","#E7B800","#FC4E07"))
#Remove confidence region (se=FALSE)
#Extend the regression lines: fullrange =TRUE
b+geom_point(aes(color=cyl,shape=cyl))+
  geom_rug(aes(color=cyl))+
  geom_smooth(aes(color=cyl),method = "lm",
              se=FALSE, fullrange=TRUE)+
  scale_color_manual(values=c("#00AFBB","#E7B800","#FC4E07"))+
  ggpubr::stat_cor(aes(color=cyl),label.x = 3)

#Split the plot into multiple panels. Use the function facet_wrap()

b+geom_point(aes(color=cyl,shape=cyl))+
  geom_smooth(aes(color=cyl,fill=cyl),method ="lm",fullrange=TRUE)+
  facet_wrap(~cyl)+
  scale_color_manual(values=c("#00AFBB","#E7B800","#FC4E07"))+
  scale_fill_manual(values=c("#00AFBB","#E7B800","#FC4E07"))+
  theme_bw()
#Add concentration ellipse around groups.
b+geom_point(aes(color=cyl,shape=cyl))+
  stat_ellipse(aes(color=cyl),type = "t")+
  scale_color_manual(values=c("#00AFBB","#E7B800","#FC4E07"))

#Convex hull of group:

b+geom_point(aes(color=cyl,shape=cyl))+
  stat_chull(aes(color=cyl, fill=cyl),alpha=0.1, geom = "polygon")+
  scale_color_manual(values=c("#00AFBB","#E7B800","#FC4E07"))+
  scale_fill_manual(values=c("#00AFBB","#E7B800","#FC4E07"))

#Add mean point and confidence ellipses
b+geom_point(aes(color=cyl,shape=cyl))+
  stat_conf_ellipse(aes(color=cyl,fill=cyl),
                    alpha=0.1,geom = "polygon")+
  stat_mean(aes(color=cyl,shape=cyl),size=2)+
  scale_color_manual(values=c("#00AFBB","#E7B800","#FC4E07"))+
  scale_fill_manual(values=c("#00AFBB","#E7B800","#FC4E07"))

#Add group mean points and stars
ggscatter(df,x="wt",y="mpg",
          color="cyl",palette = "npg",
          shape="cyl",ellipse = TRUE,
          mean.point = TRUE, star.plot = TRUE,
          ggtheme = theme_minimal())

#Change the ellipse to convex
ggscatter(df,x="wt",y="mpg",
          color="cyl",palette = "npg",
          shape="cyl",ellipse = TRUE, 
          ellipse.type = "convex",
          ggtheme = theme_minimal())

#Add text to the plot:

.labs<-rownames(df)
b+geom_point(aes(color=cyl))+
  geom_text_repel(aes(label=.labs,color=cyl),size=3)+
  scale_color_manual(values=c("#00AFBB","#E7B800","#FC4E07"))

#Draw a rectangle underneath the text, making easier to read.
b+geom_point(aes(color=cyl))+
  geom_label_repel(aes(label=.labs, color=cyl),size=3)+
  scale_color_manual(values=c("#00AFBB","#E7B800","#FC4E07"))

#BUBBLE CHART (point size controlled by continuous variable):
b+geom_point(aes(color=cyl,size=qsec),alpha=0.5)+
  scale_color_manual(values=c("#00AFBB","#E7B800","#FC4E07"))+
  scale_size(range = c(0.5,12))+ #Adjust the range of points size
  theme(legend.position = "top")

#Color by a continuous variable
b+geom_point(aes(color=mpg),size=4)+
  scale_color_gradientn(colors = c("#00AFBB","#E7B800","#FC4E07"))+
  theme(legend.position = "top")

#Add marginal density plot: (ggMarginal())

#Create a scatter plot
str(iris)
p<-ggplot(iris,aes(Sepal.Length,Sepal.Width))+
  geom_point(aes(color=Species),size=3, alpha=0.6)+
  scale_color_manual(values = c("#00AFBB","#E7B800","#FC4E07"))
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
ggscatterhist(iris, x="Sepal.Length",y="Sepal.Width",
              color="Species",size=3,alpha=0.6,
              palette = c("#00AFBB","#E7B800","#FC4E07"),
              margin.params = list(fill="Species",color="black",size=0.2))
#Use box plot as marginal plots:
ggscatterhist(iris,x="Sepal.Length",y="Sepal.Width",color="Species",
              palette = c("#00AFBB","#E7B800","#FC4E07"),
              margin.plot = "boxplot",
              ggtheme = theme_bw())

#Exist some alternatives that are quite different instead to use an scatter plot:
#Rectangular, Hexagonal binning and 2d density estimation


str(diamonds)
View(diamonds)
?diamonds
#Rectangular binning:
ggplot(diamonds,aes(x=carat,y=price))+
  geom_bin2d(bins=20,color="white")+
  scale_fill_gradient(low="#00AFBB",high="#FC4E07")+
  theme_minimal()
#Hexagonal binning:
ggplot(diamonds,aes(x=carat,y=price))+
  geom_hex(bins=20,color="white")+
  scale_fill_gradient(low="#00AFBB",high="#FC4E07")+
  theme_minimal()

#Create a scatter plot with 2d density estimation:
#Add 2d density estimation
sp<-ggplot(iris,aes(x=Sepal.Length, Sepal.Width))+
  geom_point(color="lightgray")
sp+geom_density_2d()

#Use different geometry and change the gradient color:
sp+stat_density_2d(aes(fill=..level..),geom="polygon")+
  scale_fill_gradientn(colors=c("#FFEDA0","#FEB24C","#F03B20"))

#Zoom in a scatter plot:
#install "ggforce"
library(ggforce)
ggplot(iris,aes(Petal.Length,Petal.Width,colour=Species))+
  geom_point()+
  ggpubr::color_palette("jco")+
  facet_zoom(x=Species =="virginica")+
  theme_bw()
#If we want to add some restrictions:
ggplot(iris,aes(x=Petal.Length,y=Petal.Width,colour=Species))+
  geom_point()+
  ggpubr::color_palette("jco")+
  facet_zoom(x=Petal.Length <1.5)+
  theme_bw()

#Add trend lines and equations:
#Load packages and set theme
library(ggpubr)
library(ggpmisc)
theme_set(
  theme_bw()+
    theme(legend.position = "top")
)

#Scatter plot
p<-ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+
  geom_point(aes(color=Species),size=4,alpha=0.5)+
  scale_color_manual(values=c("#00AFBB","#E7B800","#FC4E07"))+
  scale_fill_manual(values=c("#00AFBB","#E7B800","#FC4E07"))+
  facet_wrap(~Species)

#Add regression line, correlation coefficient and equations of the fitted line.

formula<-y~x
p+
  stat_smooth(aes(color=Species,fill=Species),method="lm")+
  stat_cor(aes(color=Species),label.y = 4.3)+
  stat_poly_eq(
    aes(color=Species,label=..eq.label..),
    formula = formula, label.y=4.1,parse=TRUE)

#Fit polynomial equation:
set.seed(1234)
x<-1:100
y<-(x+x^2+x^3)+rnorm(length(x),mean=0,sd=mean(x^3)/4)
my.data<-data.frame(x,y,group=c("A","B"),
                    y2=y*c(0.5,2), block=c("a","a","b","b"))

#Polynomial regression. Sow equation and adjusted R2.
formula2<-y~poly(x,3,raw = TRUE)
p<-ggplot(my.data,aes(x,y2,color=group))+
  geom_point()+
  geom_smooth(aes(fill=group),method="lm",formula=formula2)+
  stat_poly_eq(
    aes(label=paste(..eq.label.., ..adj.rr.label..,sep="~~~~")),
    formula=formula2,parse=TRUE
  )
ggpar(p, palette = "jco")

####Otra Parte####

#Paquetes Extras:
  # plotly,tibble,ggridges,dplyr,ggrepel,gridExtra
  
  #Estructura de Análisis de Información
  #Cargar la información y damos una revisión general de las variables, considerar
  #de manera importante los valores ausentes o valores mal escritos. Después vamos a 
  #realizar un análsis descriptivo visual (summary(nombrededata)),luego vamos a ver
  #un grupo de variables para analizar de las cuales realizaremos un resumen estadístico, boxplot
  #un histograma, gráficos extras y curva de densidad real y ajustada y finalmente un qqplot y el análisis
  #de normalidad
  
  #Analizaremos primero el archivo "ToothGrowth":
View(ToothGrowth)

#One of the standard learning data sets included in R is the "ToothGrowth" 
#data set. The tooth growth data set is the length of the odontoblasts (teeth)
#in each of 10 guinea pigs at three Vitamin C dosage levels (0.5, 1, and 2 mg) 
#with two delivery methods (orange juice or ascorbic acid).
#The file contains 60 observations of 3 variables

#len : Tooth length
#supp : Supplement type (VC or OJ)
#dose : Dose in milligrams
View(ToothGrowth)

getwd()
setwd("D:/9. Dictar_R/Clase_04")
getwd()
BD_01<-ToothGrowth
View(BD_01)
str(BD_01)
colnames(BD_01)
dim(BD_01)
head(BD_01)
tail(BD_01)

#Analices de los valores nulos o errores de escritura:

sum(is.na(BD_01$len)) #Variable len no tiene nulos
sum(is.na(BD_01$supp)) #Variable supp no tiene nulos
sum(is.na(BD_01$dose)) #Variable dose no tiene nulos

sapply(BD_01, function(x) sum(is.na(x)))


BD_01 <- BD_01[!is.na(BD_01$len),]
BD_01 <- BD_01[!is.na(BD_01$supp),]
BD_01 <- BD_01[!is.na(BD_01$dose),]

BD_01 <- na.omit(BD_01)

####Resumen de la información####

summary(BD_01)

g <- ggplot(ToothGrowth, aes(x= dose, y= len)) +
  geom_point(aes(color=supp))
print(g)

a <- ToothGrowth %>% group_by(supp,dose) %>% summarize(lenmean=mean(len), lensd=sd(len), count = n())
print(a)
b<- ToothGrowth %>% group_by(supp) %>% summarize(lenmean=mean(len), lensd=sd(len), count = n())
print(b)
c <- ToothGrowth %>% group_by(dose) %>% summarize(lenmean=mean(len), lensd=sd(len), count = n())
print(c)

#Si debemos sectar la información genere un subset de lowdose, middose, higdose correspondiente a
# 0.5 , 1.0 y 2.0 respectivamente.

lowdose <- ToothGrowth[ToothGrowth$dose==0.5, ]
middose <- ToothGrowth[ToothGrowth$dose==1.0, ]
highdose <- ToothGrowth[ToothGrowth$dose==2.0, ]

#Si debemos estudiarlo de acuerdo a supp OJ de 0.5 a 1.0 , OJ de 1.0 a 2.0, VC de 0.5 a 1.0 y VC de
#1.0 a 2.0 cuales serían los subsets:

OJlowtomid <- filter(ToothGrowth, dose < 2, supp=="OJ")
OJmidtohigh <- filter(ToothGrowth, dose > 0.5, supp=="OJ")
VClowtomid <- filter(ToothGrowth, dose < 2, supp=="VC")
VCmidtohigh <- filter(ToothGrowth, dose > 0.5, supp=="VC")

#Luego de revisar la información analizaremos de la forma realizada en la clase pasada:
#Calcular n, min, Q1,Me,X,trim(x),Q3,max,RIC,MAD,Sd,As,K y CV
x=BD_01$len
Valores=c(length(x),min(x),quantile(x,probs=0.25),median(x),mean(x),
          mean(x,trim=0.025),quantile(x,probs=0.75),max(x),
          IQR(x),mad(x),sd(x),skew(x),kurtosi(x),CV=(sd(x)/mean(x))*100)
Nombres=c("n","Mínimo","Q1","Mediana","Media","Media Corta al 5%",
          "Q3","Máximo","IQR","MAD","Sd","As","K","CV")
Len=data.frame(Valores,Nombres)
write.csv(Len,file = "Len.csv")

####Boxplot de Multiples Variables####

BD_01$dose=factor(BD_01$dose) #Importante al ver la estructura solo estaba puesto como numeric!

ggplot(BD_01, aes(x=dose,y=len, fill=supp))+
  geom_boxplot()+
  theme_gray()
ggplot(BD_01, aes(x=dose,y=len, color=dose))+
  geom_boxplot()+
  theme_gray()
ggplot(BD_01, aes(x=dose, y=len, fill=dose))+
  geom_boxplot()+
  labs(title = "Boxplot of lenght per dose", x="Dose (mg/l)",y="Length")

#Asociarlo a un gráfico de violin (presenta la probabilidad de densidad de Kernell de la data a diferente
#valores)

violin<-ggplot(BD_01, aes(x=dose,y=len, fill=dose))+
  geom_violin(trim = FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title = "Boxplot of lenght per dose", x="Dose (mg/l)",y="Length")+
  scale_fill_brewer(palette = "Blues")+
  theme_gray()

#Ahora lo veremos como un plot de puntos con medidas de resumen estadístico (dotplot):
library()

dotplot<-ggplot(BD_01, aes(x=dose,y=len, fill=dose))+
  geom_dotplot(binaxis = "y",stackdir = "center")+
  labs(title = "Boxplot of lenght per dose", x="Dose (mg/l)",y="Length")+
  scale_fill_brewer(palette = "Blues")+
  theme_gray()

#Produciendo sumario estadístico personalizado:
data_summary<-function(x){
  m<-mean(x)
  ymin<-m-sd(x)
  ymax<-m+sd(x)
  return(c(y=m, ymin=ymin, ymax=ymax))
}
dotplot+stat_summary(fun.data = data_summary, color="red")

####Stripchart ("gráfico de valores")####

p<-ggplot(BD_01, aes(x=dose, y=len, color=supp, shape=supp))+
  geom_jitter(position = position_dodge(0.8),size=2)

q<-ggplot(BD_01, aes(x=dose, y=len, color=supp))+
  geom_boxplot(color="black")+
  geom_jitter(position = position_dodge(0.8),size=2)
q2<-ggplot(BD_01, aes(x=dose, y=len, color=supp))+
  geom_boxplot(position = position_dodge(0.8))+
  geom_jitter(position = position_dodge(0.8),size=2)

r<-ggplot(BD_01,aes(x=dose,y=len, color=dose, shape=dose))+
  geom_jitter(position=position_jitter(0.2), size=2)+
  labs(title = "Boxplot of lenght per dose", x="Dose (mg/l)",y="Length")+
  scale_color_brewer(palette = "Dark2")+
  theme_minimal()

data_summary<-function(x){
  m<-mean(x)
  ymin<-m-sd(x)
  ymax<-m+sd(x)
  return(c(y=m, ymin=ymin, ymax=ymax))
}
r+stat_summary(fun.data = data_summary, color="red")

####Gráfico de Barras####
df<-data.frame(supp=rep(c("VC","OJ"),each=3),
               dose=rep(c("D0.5","D1","D2"),2),
               len=c(6.8,15,33,4.2,10,29.5))

ggplot(data=df, aes(x=dose,y=len,fill=supp))+
  geom_bar(stat="identity")

ggplot(data=df, aes(x=dose,y=len,fill=supp))+
  geom_bar(stat="identity", positin= position_dodge())

m<-ggplot(data=df, aes(x=dose,y=len,fill=supp))+
  geom_bar(stat="identity", color="black",position= position_dodge())+
  theme_minimal()
m+scale_fill_manual(values = c("#999999","#E69F00"))
m+scale_fill_brewer(palette = "Blues")

#Agregando etiquetas:
ggplot(data=df,aes(x=dose,y=len,fill=supp))+
  geom_bar(stat="identity",position = position_dodge())+
  geom_text(aes(label=len),vjust=1.6,color="white",
            position=position_dodge(0.9),size=3.5)+
  scale_fill_brewer(palette = "Paired")+
  theme_minimal()
#Para agregar etiquetas a un diagrama de barras estancado necesitamos 3 pasos:
#1.Ordenar la data por dose y supp: el paquete plyr es usado.
#2. Calcular la suma acumulada de la variable len para cada dose.
#3. Crear el plot.

library(plyr)
df_sorted<-arrange(df,dose,supp)
head(df_sorted)
df_cumsum<-ddply(df_sorted,"dose",
                 transform,label_ypos=cumsum(len))
head(df_cumsum)
ggplot(data=df_cumsum,aes(x=dose,y=len,fill=supp))+
  geom_bar(stat="identity")+
  geom_text(aes(y=label_ypos,label=len), vjust=1.6,
            color="white",size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()

####Gráficos Densidad####
#Primero generamos el dataframe:

set.seed(1234)
df2<-data.frame(
  sex=factor(rep(c("F","M"),each=200)),
  weight=round(c(rnorm(200,mean=55,sd=5),
                 rnorm(200,mean=65,sd=5)),digits = 2
  ))
head(df2)

ggplot(df2,aes(x=weight))+
  geom_density()
ggplot(df2,aes(x=weight))+
  geom_density()+
  geom_vline(aes(xintercept=mean(weight)),color="blue",linetype="dashed",size=1)
ggplot(df2,aes(x=weight))+
  geom_density(color="darkblue",fill="black")

#Trabajaremos con grupos:
library(plyr)
mu<-ddply(df2,"sex",summarise,grp.mean=mean(weight))
head(mu)
plot<-ggplot(df2,aes(x=weight,color=sex))+
  geom_density()+
  geom_vline(data=mu,aes(xintercept=grp.mean,color=sex),
             linetype="dashed")
plot+scale_color_manual(values = c("#999999","#E69F00","#56B4E9"))
plot+scale_color_brewer(palette = "Dark2")
plot+scale_color_grey()+theme_classic()

ggplot(df2,aes(x=weight,fill=sex))+
  geom_density()
plot2<-ggplot(df2,aes(x=weight,fill=sex))+
  geom_density(alpha=0.4)

geom_vline(data=mu,aes(xintercept=grp.mean,color=sex),linetype="dashed")

plot2+scale_fill_manual(values =c("#999999","#E69F00","#56B4E9"))+theme_classic()
plot3<-plot2+scale_color_brewer(palette = "Dark2")
plot2+scale_color_grey()+theme_classic()

plot3+theme(legend.position = "top")
plot3+theme(legend.position="bottom")
plot3+theme(legend.position="none")

#Combinando histogramas y curvas de densidad:
ggplot(df2,aes(x=weight))+
  geom_histogram(aes(y=..density..),binwidth = 1,colour="black",fill="gray")+
  geom_density(alpha=0.2,fill="#FF6666")

ggplot(df2,aes(x=weight,color=sex,fill=sex))+
  geom_histogram(aes(y=..density..),alpha=0.5,position = "identity")+
  geom_density(alpha=0.2)

#Una aplicación importante para dividir data rápidamente es dividirlo en varios paneles (usando
#facets)

ggplot(df2,aes(x=weight))+
  geom_density()+facet_grid(sex~.)+
  geom_vline(data=mu,aes(xintercept=grp.mean, color="red"),linetype="dashed")

#Podemos poner los gráficos más bonitos:
ggplot(df2,aes(x=weight,fill=sex))+
  geom_density(fill="gray")+
  geom_vline(aes(xintercept=mean(weight)),color="blue",linetype="dashed")+
  labs(title="Weight density curve",x="Weight(kg)",y="Density")+
  theme_classic()
####Generación de Histogramas####

##Caso 1 de análisis data creada df3
set.seed(1234)
df3<-data.frame(
  sex=factor(rep(c("F","M"),each=200)),
  weight=round(c(rnorm(200,mean=55,sd=5),
                 rnorm(200,mean=65,sd=5)),digits = 2
  ))
head(df3)

ggplot(df3,aes(x=weight))+geom_histogram()
ggplot(df3,aes(x=weight))+geom_histogram(binwidth = 1)
p<-ggplot(df3,aes(x=weight))+geom_histogram(color="black",fill="white")
p+geom_vline(aes(xintercept=mean(weight)),
             color="blue",linetype="dashed",size=1)
ggplot(df3,aes(x=weight))+
  geom_histogram(aes(y=..density..),color="black",fill="white",linetype=
                   "dashed")+
  geom_density(alpha=0.2,fill="#FF6666")
#Lo haremos ahora por grupo según sexo:
library(dplyr)
mu<-ddply(df3,"sex",summarise,grp.mean=mean(weight))
head(mu)
ggplot(df3,aes(x=weight,color=sex))+
  geom_histogram(fill="white")
ggplot(df3,aes(x=weight,color=sex))+
  geom_histogram(fill="white",alpha=0.5,position = "identity")
library(plotly)
ggplotly()  #SORPRENDENTE!!!!!!

ggplot(df3,aes(x=weight,color=sex))+
  geom_histogram(fill="white",position="dodge",binwidth = 1)+
  geom_vline(data=mu,aes(xintercept=grp.mean, color="sex"),
             linetype="dashed")+
  theme(legend.position = "top")+
  scale_color_manual(values=c("#999999","#E69F00","#56B4E9"))
#Se puede usar:
scale_color_brewer(palette = "Dark2")
scale_color_grey()+theme_classic()

ggplot(df3,aes(x=weight,fill=sex,color=sex))+
  geom_histogram(position = "identity", alpha=0.5)+
  geom_vline(data=mu,aes(xintercept=grp.mean, color="sex"),
             linetype="dashed")
#Usando facetas:
ggplot(df3,aes(x=weight))+
  geom_histogram(color="black",fill="white")+
  facet_grid(sex~.)+
  geom_vline(data=mu,aes(xintercept=grp.mean,color="red"),
             linetype="dashed")
ggplotly()
ggplot(df3,aes(x=weight,fill=sex))+
  geom_histogram(fill="white",color="black")+
  geom_vline(aes(xintercept=mean(weight)),color="blue",
             linetype="dashed")+
  labs(title="Histograma de Peso",x="Weight(kg)",y="Count")+
  theme_classic()
ggplot(df3,aes(x=weight,color=sex,fill=sex))+
  geom_histogram(position="identity",alpha=0.5)+
  geom_vline(data=mu,aes(xintercept=grp.mean,color=sex),
             linetype="dashed")+
  scale_color_manual(values = c("#999999","#E69F00","#56B4E9"))+
  scale_fill_manual(values = c("#999999","#E69F00","#56B4E9"))+
  labs(title="Histograma de Peso",x="Weight(kg)",y="Count")+
  theme_classic()

ggplot(df3,aes(x=weight,color=sex,fill=sex))+
  geom_histogram(aes(y=..density..),position="identity",alpha=0.5)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu,aes(xintercept=grp.mean,color=sex),
             linetype="dashed")+
  scale_color_manual(values = c("#999999","#E69F00","#56B4E9"))+
  scale_fill_manual(values = c("#999999","#E69F00","#56B4E9"))+
  labs(title="Histograma de Peso",x="Weight(kg)",y="Densidad")+
  theme_classic()
ggplotly()          




####Plot Individual Mi Código####

#Para visualizar los gráficos de manera conjunta modifique el layout de la siguiente forma:
dev.off() # Desactivamos todas las ventanas gráficas o dispositivos
x11() # Abrimos el primer dispositivo
layout(matrix(c(1:2), ncol=2, byrow=FALSE))
layout.show(2) # Muestra las 6 particiones que escogí
y<-df3
z<-df3$weight
annotation <- data.frame(
  x = c(0.55,1.10,0.65,1.32,0.65,0.90,1.10),
  y = c(median(z),min(z),quantile(z, prob = c(0.25)),
        median(z),quantile(z, prob = c(0.75)),((quantile(z, prob = c(0.75))+max(z))/2),max(z)),
  label = c("RIC","Concentración Mínima =","Q1(25%) =","Mediana(50%) =","Q3(75%) =","Atípicos",
            "Concentración Máxima =")
)
annotation2<-data.frame(
  x=c(0.55,0.50),
  y=c(min(z),min(z)/2),
  label=c("*Boxplot con valores obtenidos en campo","")
)
ggplot(y, aes(x=factor("sex"), y=weight))+
  geom_boxplot(outlier.colour="steelblue",outlier.shape =19,outlier.size = 4,
               notch=FALSE,width=0.5, fill="gray")+
  stat_summary(fun=mean, geom="point",shape=23, fill= "black",size=4)+
  labs(title="Peso de Mujeres",x="Mujeres",y= "Peso (kg) ",outer=TRUE)+
  stat_summary(geom="text", fun.y =quantile,
               aes(label=sprintf("%.4f", ..y..)),
               position=position_nudge(x=c(0.21,-0.29,0.40,-0.29,0.21)), size=3)+
  geom_text(data=annotation, aes( x=x, y=y, label=label),
            color="black", 
            size=3 , angle=0, fontface="bold" )+
  geom_text(data=annotation2, aes( x=x, y=y, label=label),
            color="black", 
            size=3 , angle=0, fontface="bold" )+
  annotate("pointrange", x = 0.60, y = median(z), ymin = quantile(z, prob = c(0.25)), ymax = quantile(z, prob = c(0.75)),
           colour = "black", size = 0.5)+
  annotate("pointrange", x = 0.95, y = ((quantile(z, prob = c(0.75))+max(z))/2), ymin = quantile(z, prob = c(0.85)), ymax = max(z),
           colour = "steelblue", size = 0.5)+
  theme_gray()