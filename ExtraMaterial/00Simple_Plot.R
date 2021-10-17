####Simple Plots en R####

#AirPassenger (Data de Tiempo):
#Se recomienda revisar la data AirPassengers antes de trabajar el plot.
dir()
View(AirPassengers)
head(AirPassengers)    
colnames(AirPassengers)     
str(AirPassengers)
summary(AirPassengers)

plot(AirPassengers, col ="blue", type = "o", pch=18, lty=4, axes=FALSE,ann=FALSE)
rangex <- range(0,AirPassengers)
axis(1, tick = TRUE)
axis(2, las=1, at = 100:rangex[2])
title(main="Vuelos desde 1949 a 1961", col.main="black", font.main=12, cex.main=4)
title(xlab="Años",col.lab="black")
title(ylab="Total de vuelos", col.lab="black")
legend(1,rangex[2],c("vuelos"),col="blue", cex=0.8, pch=18, lty=4)

# CO2 verificar información:

dir()
data("CO2")
View(CO2)
head(CO2)    
colnames(CO2)     
str(CO2)
pairs(CO2)
summary(CO2)

# Boxplots:
boxplot(CO2$uptake~CO2$Plant, notch=FALSE,horizontal=TRUE,axes=FALSE,ann=FALSE,col=heat.colors(12))
range_d <- range(0, CO2$uptake)
axis(2, at=1:12, lab=c("Qn1","Qn2","Qn3","Qc1","Qc2","Qc3","Mn3","Mn2","Mn1","Mc2","Mc3","Mc1"),cex=0.1)
axis(1, tick = TRUE)
title(main="Boxplot según Tipo de Planta", sub="WEBONSCIENCE", col.main="black", font.main=4, cex=1,lty=3)
title(ylab="Tipo de Planta", col.lab="black",font.lab=4,cex=1.5,lty=4)
title(xlab="Toma de CO2", col.lab="blue", font.lab=4,cex=4,lty=4)

# Plot Detallado:
plot(CO2$conc,CO2$uptake,
     main = "Grafico de Concentracion Vs. Toma de CO2",
     sub= "Data Historica",
     xlab = "Concentracion de CO2 (mg/l)",
     ylab = "Toma de CO2",
     cex.lab=1.2,
     cex.axis=1.2,
     cex.main=1.2,
     cex.sub=1,
     type = "p",
     col="steelblue",
     lwd=2,
     pch=2,
     bg="red",
     cex=1)
#Generamos textos en el gr?fico:
text(x=CO2$conc[6:7], y=CO2$uptake[6:7], labels=rownames(CO2[6:7,]), pos=4, col="red")
text(x=CO2$conc[c(8,9,10)], y=CO2$uptake[c(8,9,10)],
     labels=c("Point 1", "Point 2", "Point 3"), pos=4, col="blue",xpd=TRUE)

mtext(c("Lower", "Higher"), side=1, line=3, at=c(250, 900), col=c("blue", "red"))
mtext("Another label", side=4, line=1, adj=0, col="green2") # Rotated y axis label
mtext("Another \nlabel", side=4, line=1, adj=1, col="green2", las=1) # Horizontal label

#Realizando an?lisis de ciertos valores de la visualizaci?n:
id <- identify(CO2$conc,CO2$uptake,labels = CO2$Type,pos=TRUE,font=2)
analisis <- CO2[id$ind, ]

#Agregar l?neas horizontales y verticales:
abline(v=400, col="grey",lwd=2,lty=1)
text(x=(CO2$conc=400),y=(CO2$uptake=25),labels="       Boundary")
abline(h=mean(CO2$uptake), col="pink",lwd=2, lty=2)
abline(v=mean(CO2$conc),col="green",lwd=2, lty=2)

#Dibujar flechas:
arrows(x0=40, y0=-1, x1=CO2$conc[1], y1=CO2$uptake[1], col="blue", lwd=2)
arrows(x0=40, y0=-1, x1=CO2$conc[1]-2, y1=CO2$uptake[1]+0.02, col="blue", lwd=2)
arrows(x0=200, y0=40, x1=CO2$conc[10], y1=CO2$uptake[10], col="red", lwd=2)

#Generando Ubicaci?n:
locator(3,type = "p")
a1<-locator(2)
a2<-locator(2)
a3<-locator(2)
# Create a matrix of the coordinates:
co.x <- cbind(a1$x, a2$x, a3$x)
co.y <- cbind(a1$y, a2$y, a3$y)
#Dibujando las lineas generadas con flechas:
arrows(x0=co.x[1,], y0=co.y[1,], x1=co.x[2,], y1=co.y[2,], 
       col=c("red", "green", "blue"), lwd=2, xpd=TRUE)
arrows(x0=c(400, 750), y0=-2.65, x1=c(350, 800), y1=-2.65, 
       col=c("blue", "red"), length=0.15, lwd=3, xpd=TRUE)
#Generando Ubicacion(locator(n>=3))
m<-locator(3) 
r<-locator(4)
#Dibujando los pol?gonos:
polygon(m,border = "red",lwt=3,lty=3)
polygon(r,border = "red",lwt=3, lty=2)
search()



