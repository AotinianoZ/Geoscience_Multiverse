    #Chapter II (Programming with R)
#Packages of ML:
.libPaths(c("D:/R_Packages", .libPaths()))
install.packages("caret",dependencies=c("Depends","Suggests"))
library(caret)
library(help="caret")

#Comands to load dataset:
data(iris)
library(help="datasets")

#Package "mlbench"
#Colección de problemas de referencia de machine learning.
#URI: https://cran.r-project.org/web/packages/mlbench/index.html

install.packages("mlbench")
library(mlbench)
library(help="mlbench")

#Package "Applied Predictive Modeling"
install.packages("AppliedPredictiveModelling", dependencies=c("Depends","Suggests"))
library(AppliedPredictiveModeling)
library(help="AppliedPredictiveModeling")

#Conjuntos de datos mlbench:
data(BostonHousing)
?BostonHousing
head(BostonHousing)
summary(BostonHousing)

data(BreastCancer)
head(BreastCancer)
summary(BreastCancer)

data(Glass)
head(Glass)
summary(Glass)

data(Ionosphere)
head(Ionosphere)
summary(Ionosphere)

data(PimaIndiansDiabetes)
head(PimaIndiansDiabetes)
summary(PimaIndiansDiabetes)

data(Sonar)
head(Sonar)
summary(Sonar)

data(Soybean)
head(Soybean)
summary(Soybean)
colnames(Soybean)



#Conjunto de datos Applied Predictive Modeling:
data(abalone)
str(abalone)
head(abalone)
summary(abalone)




