# Probabilidad_Variable_Aleatoria_y_Distribuciones #
.libPaths(c("D:/R_Packages",.libPaths()))

#VALORES DISCRETOS Y CONTINUOS:

#spike plot:
# Muestra las probabilidades para cada valor en el rango de X como
# un "spike", enfatizando lo discreto de la distrubción.

k=0:4
p=c(1,2,3,2,1)/9
plot(k, p, type="h",xlab="k",ylab="probability",
     ylim=c(0,max(p)))
points(k,p,pch=16,cex=2)

#sample() generar valores aleatorios:
k=0:2
p=c(1,2,1)/4
sample(k,size=1,replace=FALSE,prob=p) #simula que todo tiene 
#la misma probabilidad de ocurrencia.
sample(k,size=1,prob=p)

#lanzar una moneda 10 veces, cara=1 y sello=0
sample(0:1, size=10, replace=TRUE) #i.i.d sample
sample (1:6,size=10, replace=TRUE) #lanzar un 
#dado 10 veces.
#suma de lanzar el dado 10 veces
sample(1:6,size=10,replace=TRUE)+ 
  sample(1:6,size=10,replace=TRUE)
#Asumir que 10000 personas se les pregunta
#si les gusta comer pollo o no y 6200 dicen si.
#Luego si elegimos una muestra de 10:
sample(rep(0:1,c(3200,6800)),size=10,replace=TRUE)
sample(0:1, size=10, replace=T, prob=c(1-.62,.62))

#Familia de distribuciones:
# d: retorna la p.d.f de la distribucion.
# p.d.f: probability density function

# p: retorna la c.d.f de la distribucion.
# c.d.f:cummulative density function

# q: retorna los cuantiles.
# r: retorna una muestra aleatoria desde la distribucion.

#Cada familia de distribución tiene sus parámetros pero
# las funciones son similares en funcionamiento.

dunif(x=1, min=0, max=3)
punif(q=2,min=0,max=3)
qunif(p=1/2,min=0,max=3)
runif(n=1,min=0,max=3)
runif(n=10,min=0,max=10)

#Multiples cuantiles:
ps<-seq(0,2,by=0.2)
names(ps)=as.character(seq(0,200,by=20))
ps
qunif(ps, min=0,max=1)

#Ejemplo de distribución Binomial:

#Lanzar una moneda 10 veces. X:numero de caras.
#Si la moneda es justa. #X tienen una distribución binomial
# Binomial(10, 1/2)
# La probabilidad que X=5
choose(10,5)*(1/2)^5*(1/2)^(10-5)
# usando d function
dbinom(5,size=10, prob=1/2)
#La probabilidad de obtener 6 o menos caras
sum(dbinom(0:6,size=10,prob=1/2))

#Spike plot producido por binomial:
heights=dbinom(0:10, size=10,prob=1/2)
plot(0:10,heights,
     type="h",
     main="Spike plot de X",xlab="k",ylab="p.d.f")
points(0:10,heights,pch=16,cex=0.5)

#De una poblacion las personas que eligen geologia se les 
#considera como si y se codifica con 1 al resto con 0.
#El analisis corresponde a una Binomial(n,p) de una v.a.d
#donde n es el número de muestras.

#Si la población favorable que eligió si es 62%,calcule de una
#Muestra de 100 que tan probable es que respondan 60% o menos.
pbinom(60, size=100, prob=0.62)

#Distribucion Exponencial: Exponential(lambda)
res=rexp(50,rate=1/5)
#boxplot:
par(fig=c(0,1,0,0.35))
boxplot(res, horizontal=TRUE,bty="n",
        xlab="muestra exponencial")
#histograma
par(fig=c(0,1,0.25,1),new=TRUE)
#guardar los valores,
#luego encontrar el mas grande y setearlo
#como maximo:
tmp.hist=hist(res,plot=FALSE)
tmp.edens=density(res)
tmp.dens =dexp(0, rate=1/5)
y.max=max(tmp.hist$density,tmp.edens$y,
          tmp.dens)
hist(res,ylim=c(0,y.max),prob=TRUE,
     main="", col=gray(0.9))
curve(dexp(x,rate = 1/5),lwd=2,add=TRUE)
rug(res)

#Distribucion Normal:
#Normal(µ,σ)
pnorm(q=1.5,mean=0, sd=1)
pnorm(q=4.75, mean=0,sd=1/2)
qnorm(c(0.25,0.50,0.75)) #Areas importantes
#basadas en Z-scores
#qnorm especificamos el area que deseamos
#cuanta area es no mayor a una desviacion estandar
#desde la media:
pnorm(1)-pnorm(-1)
#dos desviaciones estandar:
1-2*pnorm(-2)
#tres desviaciones estandar:
diff(pnorm(c(-3,3)))

#LogNormal: log(x)

res=rlnorm(n=50, meanlog=0,sdlog=1)
#boxplot:
par(fig=c(0,1,0,0.35))
boxplot(res, horizontal=TRUE, bty="n",xlab="lognormal")
#histogram:
par(fig=c(0,1,0.25,1),new=TRUE)
tmp.hist=hist(res, plot=FALSE)
tmp.edens=density(res)
tmp.dens = dexp(0,rate=1/5)
ymax =max(tmp.hist$density,tmp.edens$y,
          tmp.dens)
#hist:
par(fig=c(0,1,0.25,1),new=TRUE)
hist(res,ylim=c(0,ymax),prob=TRUE,
     main="",col=gray(0.9))
curve(dlnorm(x,meanlog=0,sdlog=1),
      add=TRUE)
rug(res)

#t-distribution, F-distribution and
#chi-distrbution














