                                          #Introduction

.libPaths(c("D:/R_Packages", .libPaths()))
data(iris)
summary(iris)
proc.time()
#Operator of assignation and show values
a<-23 #integer
b<-2.3 #double
c<-TRUE #boolean
d<-"Hola Mundo" #string (is neccesary convert to factor)
#Creating a vector:
v<-c(98,151,121)
v
v[1:2]
r<-c(1:10)
r[1:5]
v<-c(1,2,4,5)
v[6]<-7
v
#List:
a<-list(aa=1, bb=2, cc=3)
a$aa
a$dd=5
a
#Matrix:
data<-c(1,2,3,4,5,6)
headling<-list(NULL,c("a","b","c"))
m<-matrix(data,nrow=2, ncol=3, byrow = TRUE, dimnames = headling)
m
m[1, ]
m[ ,1]
#Dataframe:
years<-c(1985,1990,2020)
scores<-c(34,44,83)
df<-data.frame(years,scores)
df
df[ ,1]
df[1 ,]
df$years
#Control Structures (if-then-else)
a<-66
if(a>55){
  print("a is more than 55")
}else{
  print("A is less than or equal to 55")
}
#For
mylist<-c(55,66,77,88,99)
for (value in mylist){
  print(value)
}
#While
a<-100
while(a<500){
  a<-a+100
print(a)
  }
#Prefined Functions:
numbers<-c(1,2,3,4,5,6)
mean(numbers)
?mean       #help code
help(mean)  #help code
args(mean)  #kwow arguments
example(mean) #examples of the function
mysum<-function(a,b,c){
  sum<-a+b+c
  return(sum)
}
mysum(1,2,3)

#Packages of ML:

install.packages("caret")
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

data(BostonHousing)
head(BostonHousing)
str(BostonHousing)

data(BreastCancer)
head(BreastCancer)
str(BreastCancer)

data(Glass)
head(Glass)
str(Glass)

data(Ionosphere)
head(Ionosphere)
str(Ionosphere)

data(PimaIndiansDiabetes)
head(PimaIndiansDiabetes)
str(PimaIndiansDiabetes)

data(Sonar)
head(Sonar)
str(Sonar)

data(Soybean)
head(Soybean)
str(Soybean)







