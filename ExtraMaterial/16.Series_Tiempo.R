.libPaths(c("D:/R_Packages",.libPaths()))


#### Ejemplo1 ####
library(quantmod)
help(quantmod)

getSymbols("FB")
str(FB)
head(FB)
tail(FB)
plot(FB$FB.Open)
chartSeries(FB$FB.Open)

#Calculo de los retornos

# r[t] = log(P[t]) - log(P[t-1])
# r[t] = log(P[t]/P[t-1])

r = diff(log(FB[,1])) #vector de retornos
chartSeries(r)

summary(r)
View(r)
colnames(r)
summary(coredata(r))

r[which.min(r)] # dia en que se obtuvo el precio minimo
hist(r, breaks=100)
quantile(r, probs=0.01, na.rm=TRUE)


#### Ejemplo2 ####
# "Introductory time series with R" Cowperwait,Metcalfe (2009)
# "Time series analysis with R" Mcleod,Mahdi (2012)
# "Time series analysis with applications"  Cryer (2008)
# Instalar : TSA , tseries

library(TSA)
library(tseries)

data("rwalk")
plot(rwalk, ylab="Paseo Aleatorio", type="o")
r_walk <- diff(log(as.xts(rwalk)))
str(rwalk)
class(rwalk)
colnames(rwalk)

data(bev)
help(bev)
plot(bev, ylab="Trigo",type="o")
r_bev<-diff(log(bev))
plot(r_bev)
str(bev)

data("oilfilters")
help("oilfilters")
r_oilf<-diff(log(oilfilters))
plot(oilfilters, ylab="Filtros de aceite",type="o")
plot(r_oilf)

# Analisis cbe

CBE<- read.table("cbe.dat", header =TRUE)
head(CBE)

#Informacion mensual, capturada desde 1958
Elec.ts <- ts(CBE[ ,3], star = 1958, freq = 12)
Beer.ts <- ts(CBE[ ,2], star = 1958, freq = 12)
Choc.ts <- ts(CBE[ ,1], star = 1958, freq = 12)
plot(Elec.ts)
plot(Beer.ts)
plot(Choc.ts)
plot(cbind(Elec.ts, Beer.ts, Choc.ts))

# Descomponer: ts = tendencial + estacional + errror

# Aritmetica:

plot(decompose(Elec.ts)$random)

# Descomposicion Multiplicativa:

Elec.decom_M<-decompose(Elec.ts, type="mult")
plot(Elec.decom_M$random)













