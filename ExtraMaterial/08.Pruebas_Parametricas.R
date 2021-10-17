.libPaths(c("D:/R_Packages",.libPaths()))
####librerias####
library(plot3D)
library(chron)
library(psych)
library(nortest)
library(ggplot2)
library(ggmap)
library(NADA)
library(MASS)
library(readxl)
library(plotly)
library(tibble)
library(ggridges)
library(dplyr)
library(ggrepel)
library(gridExtra)
library(ggpubr)
library(car)
library(plyr)
library(ggpmisc)

library(caret)
library(mlbench)
library(AppliedPredictiveModeling)
library(e1071)
library(lattice)
library(Rcpp)
library(corrplot)
library(Amelia)
library(RCurl)
library(tidyverse)
library(klaR)
library(rpart)
library(randomForest)
library(glmnet)
library(kernlab)
library(learnr)
library(C50)
library(caretEnsemble)
library(profvis)
library(Cubist)


library(knitr)
library(knitLatex)
library(tinytex)
library(rmarkdown)

#### Goodnest of fit (Chi-square) ####

#Tenemos la siguiente entrada:

#Example1: Case of equal proportions

valores<-c(1:6)
observed<-c(57,46,68,52,72,65)
uniform<-rep(1/6,6)

test<-chisq.test(observed,p=uniform)

par(mfrow=c(1,2))
plot(valores,observed, ylim=c(0,200))
plot(valores,uniform, ylim=c(0,6))

test
test$statistic
test$parameter
test$p.value
test$method
test$data.name
test$observed
test$expected
test$residuals
test$stdres

expected.count = sum(observed)*uniform
chi2 = sum((observed- expected.count)^2/ expected.count)
chi2
pchisq(chi2,
       df=5,
       lower.tail=FALSE) 

#Critic value calculation:
qchisq(.96, df=5)

#Graphing:
valores_name <-c("uno","dos","tres","cuatro","cinco","seis")
observed <- c(57,46,68,52,72,65)
expected <- rep(1/6,6)

total = sum(observed)
observed.prop = observed / total
observed.prop


Input <- ("
Value     Numero_1 Numero_2 Numero_3 Numero_4 Numero_5 Numero_6
Observed  0.1583333 0.1277778 0.1888889 0.1444444 0.2000000 0.1805556
Expected  0.1666667 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667  
")

Matriz <- as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))
par(mfrow=c(1,1))
barplot(Matriz,
        beside=TRUE,
        legend=TRUE,
        ylim=c(0, 0.3),
        xlab="Dice Value",
        ylab="Foraging Proportion")

#Ejemplo 4:

### Creating frequency table
data <- data.frame(c(0:5), c(50,77,81,48,31,13))
names(data) <- c('n', 'frequency' )
print(data)

## Make it look neat
library(kableExtra)
library(knitr)
options(knitr.table.format = "html") 
kable(data) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left")

## Fit poisson distribution to the data
## This is what dpois() does - available with R already to save your time
poisson <- function(x, lambda) {
  probfn <- exp(-lambda) * (lambda ^ x) / factorial(x)
  return(probfn)
}
## Prepare some stats
attach(data)
N <- sum(frequency)
RF <- frequency/N
DF <- data.frame(data, round(RF, 5))
MEAN <- sum(RF * DF$n)
VAR <- (sum(DF$n^2*DF$frequency) - N*MEAN^2)/(N-1) # else use (MEAN+MEAN^2/R)
DISP <- VAR/MEAN # over dispersion
THETA <- 1/DISP
R <- MEAN*THETA/(1-THETA) # MEAN = R(1-THETA)/THETA
cbind(MEAN,VAR,DISP,THETA,R)

x = n
(E_poi = round(N * dpois(x, lambda=MEAN),5))

par(mfrow=c(1,1))
barplot(matrix(c(frequency,E_poi),nr=2, byrow = TRUE), beside=T, 
        col=c("aquamarine3","coral"), 
        names.arg=x)
legend("topright", c("Observed","Expected Poisson"), pch=15, 
       col=c("aquamarine3","coral"), 
       bty="n")

CT = chisq.test(rbind(frequency,E_poi)); CT
C = CT$statistic; C
pVal = CT$p.value
RES = frequency - E_poi
par(mfrow=c(1,3))
plot(x,RES)
SR <- sum(RES)
MSE <- mean(RES^2)
SUMMARY <- matrix(c(SR, C, pVal, MSE), byrow = TRUE)
colnames(SUMMARY) <- c("Poisson Fit")
rownames(SUMMARY) <- c("sum of residuals", "Chi-square Statistic", "p-value", "MSE")
SUMMARY

### Combining categories
(Actual <- c(50,77,81,sum(48+31+13)))

(Expected <- c(E_poi[1], E_poi[2],E_poi[3], sum(E_poi[4],E_poi[5],E_poi[6])))

(combnd <- chisq.test(expChiSq <- rbind(Actual,Expected)))


#Code: https://rpubs.com/vidhya36/292819


