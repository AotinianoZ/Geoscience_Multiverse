                                        ####Técnicas de Modelamiento####
.libPaths(c("D:/R_Packages", .libPaths()))

library(knitr)
library(knitLatex)
library(learnr)
library(tinytex)
library(rmarkdown)

library(ggplot2)
library(lattice)
library(caret) #paquete necesario para metricas
library(mlbench)
library(AppliedPredictiveModeling)
library(e1071)
library(rpart)
library(corrplot)
library(Amelia)
library(RCurl)
library(MASS)
library(klaR)
library(randomForest)
library(glmnet)
library(kernlab)
                                        


              ####Feature Selection####
#Coeficiente de Correlacion

library(clusterGeneration)
S <-genPositiveDefMat("unifcorrmat", dim=15)
library(mnormt)
n<-5000
X <-rmnorm(n, varcov = S$Sigma)
Y <-rbinom(n, size =1, prob =0.3)
data <-data.frame(Y,X)
cor(data, data$Y)

#Eliminar por Correlacion

set.seed(7)
data(PimaIndiansDiabetes)
correlationMatriz <- cor(PimaIndiansDiabetes[ ,1:8 ])

print(correlationMatriz)
highlyCorrelated <- findCorrelation(correlationMatriz, cutoff = 0.5)
highlyCorrelatedNames <-findCorrelation(correlationMatriz, cutoff=0.4, names=TRUE)
print(highlyCorrelatedNames)

#Basados en Modelo de Regresion:
data(PimaIndiansDiabetes)
data_lm<-as.data.frame(PimaIndiansDiabetes)
fit_glm <- glm(diabetes~., data_lm, family ="binomial")
summary(fit_glm)

#Basadas en conocimiento#

#"VARIMP()" en Decision Tree
varImp(fit_glm)

# varImp() en Learning Vector Quantization (LVQ)

set.seed(7)
data(PimaIndiansDiabetes)
control<-trainControl(method="repeatedcv", number =10, repeats=3)
model<-train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale",trControl=control)
importance<-varImp(model,scale = FALSE)
plot(importance)

# Random Forest
library(randomForest)
View(PimaIndiansDiabetes)
fit_rf<-randomForest(diabetes~., data=data_lm)
importance(fit_rf)
varImp(fit_rf)
varImpPlot(fit_rf)

#Eliminacion Recursiva de Atributos (RFE)

set.seed(7)
data(PimaIndiansDiabetes)
control<-rfeControl(functions = rfFuncs, method="cv", number = 10)
results<-rfe(PimaIndiansDiabetes[ ,1:8], PimaIndiansDiabetes[ ,9], sizes=c(1:8),
             rfeControl = control)
print(results)
predictors(results)
plot(results, type=c("g","o"))

#### Modelos de Taxonomic Lineal ####

## Regresion Lineal

#sin caret:

data("BostonHousing")
str(BostonHousing)
?BostonHousing
summary(BostonHousing)

fit<-lm(medv~. , BostonHousing)
print(fit)
summary(fit)
predictions<-predict(fit, BostonHousing)
mse<-mean((BostonHousing$medv-predictions)^2)
print(mse)

#con caret:
data("BostonHousing")
set.seed(7)
trainControl01<-trainControl(method="cv", number=5)
fit.lm<-train(medv~., data=BostonHousing, method="lm",
              metric="RMSE", preProcess=c("center","scale"),
              trControl=trainControl01)
print(fit.lm)

# Regresion Logistica

# sin caret

data("PimaIndiansDiabetes")
str(PimaIndiansDiabetes)
summary(PimaIndiansDiabetes)

fit<-glm(diabetes~., data=PimaIndiansDiabetes, 
         family=binomial(link="logit"))
print(fit)
summary(fit)
probabilities<-predict(fit, PimaIndiansDiabetes[ ,1:8], type="response")
predictions<-ifelse(probabilities>0.5, "pos","neg")
table(predictions, PimaIndiansDiabetes$diabetes)

#con caret:

set.seed(7)
trainControl02<-trainControl(method="cv", number=5)
fit.glm<-train(diabetes~., data=PimaIndiansDiabetes, method="glm",
               metric="Accuracy", preProcess=c("center","scale"),
               trControl=trainControl02)
print(fit.glm)
summary(fit.glm)

# Linear Discriminant Analysis (LDA)

#sin caret:
data("PimaIndiansDiabetes")
fit<-lda(diabetes~., data=PimaIndiansDiabetes)
print(fit)
predictions<-predict(fit, PimaIndiansDiabetes[ ,1:8])$class
table(predictions, PimaIndiansDiabetes$diabetes)

#con caret:
set.seed(7)
trainControl03<-trainControl(method="cv",number=5)
fit.lda<-train(diabetes~. , data=PimaIndiansDiabetes, method="lda",
               metric="Accuracy", preProcess=c("center","scale"),
               trControl=trainControl03)
print(fit.lda)
# plot(fit.lda)


# Regularized Regression

#Problema de Clasificación
library(glmnet)
data(PimaIndiansDiabetes)
x <- as.matrix(PimaIndiansDiabetes[ ,1:8])
y <- as.matrix(PimaIndiansDiabetes[ ,9])
fit <- glmnet(x, y, family = "binomial", alpha=0.5, lambda=0.001) 
print(fit)
predictions<-predict(fit, x, type ="class")
table(predictions, PimaIndiansDiabetes$diabetes)

#Problema de Regresion
data(BostonHousing)
BostonHousing$chas <-as.numeric(as.character(BostonHousing$chas))
x <-as.matrix(BostonHousing[ ,1:13])
y <- as.matrix((BostonHousing[ ,14]))
fit<-glmnet(x, y, family = "gaussian", alpha =0.5, lambda = 0.001)
print(fit)  
predictions <- predict(fit, x, y, type="link")
View(predictions)
mse <- mean((y-predictions)^2)
print(mse)

#Regresion:

data("BostonHousing")
set.seed(7)
trainControl04<-trainControl(method="cv",number=5)
fit.glmnet<-train(medv~. , data=BostonHousing, method="glmnet",
                  metric="RMSE", preProcess=c("center","scale"),
                  trControl=trainControl04)
print(fit.glmnet)
plot(fit.glmnet)

#Clasificacion:

data("PimaIndiansDiabetes")
set.seed(7)
trainControl05<-trainControl(method="cv",number=5)
fit.glmnet<-train(diabetes~. , data=PimaIndiansDiabetes, method="glmnet",
                  metric="Accuracy", preProcess=c("center","scale"),
                  trControl=trainControl05)
print(fit.glmnet)
plot(fit.glmnet)

#### Modelos de Taxonomia No Lineal ####

#K-NN

#clasificacion

data(PimaIndiansDiabetes)
fit<-knn3(diabetes~., data=PimaIndiansDiabetes)
print(fit)
predictions <-predict(fit, PimaIndiansDiabetes[ ,1:8], type="class")
table(predictions, PimaIndiansDiabetes$diabetes)

#regresion
data(BostonHousing)
BostonHousing$chas<-as.numeric(as.character(BostonHousing$chas))
x <- as.matrix(BostonHousing[ ,1:13])
y <- as.matrix(BostonHousing[ ,14])
fit<-knnreg(x, y, k=3)
print(fit)
predictions<-predict(fit, x)
mse <-mean((BostonHousing$medv-predictions)^2)
print(mse)

#con caret clasificacion:

data(PimaIndiansDiabetes)
set.seed(7)
trainControl06<-trainControl(method="cv", number=5)
fit.knn<-train(diabetes~., data=PimaIndiansDiabetes, method="knn",
               metric="Accuracy", preProcess=c("center","scale"),
               trControl=trainControl06)
print(fit.knn)
plot(fit.knn)

#con caret en regresion:

data(BostonHousing)
set.seed(7)
trainControl07<-trainControl(method="cv", number=5)
fit.knn<-train(medv~., data=BostonHousing, method="knn",metric="RMSE",
               preProcess=c("center","scale"), trControl=trainControl07)
print(fit.knn)
plot(fit.knn)

#Naive Bayes

#sin caret
data(PimaIndiansDiabetes)
fit<-naiveBayes(diabetes~., data=PimaIndiansDiabetes)
print(fit)
predictions <- predict(fit, PimaIndiansDiabetes[ ,1:8])
table(predictions, PimaIndiansDiabetes$diabetes)
#con caret:

data("PimaIndiansDiabetes")
set.seed(7)
trainControl08<-trainControl(method="cv", number=5)
fit.nb<-train(diabetes~., data=PimaIndiansDiabetes, method="nb",
              metric="Accuracy", preProcess=c("center","scale"),
              trControl=trainControl08)
print(fit.nb)
plot(fit.nb)

#Super Vector Machine
#instalar DRR y kernlab ademas de LIBSVM:
library(kernlab)

#clasificacion sin caret:
data(PimaIndiansDiabetes)
fit <- ksvm(diabetes~.,data=PimaIndiansDiabetes, kernel="rbfdot")
print(fit)
predictions<-predict(fit, PimaIndiansDiabetes[ ,1:8], type="response")
table(predictions, PimaIndiansDiabetes$diabetes)

#regresion sin caret:
data(BostonHousing)
fit<-ksvm(medv~., data=BostonHousing, kernel="rbfdot")
print(fit)
predictions <- predict(fit,BostonHousing)
mse<-mean((BostonHousing$medv-predictions)^2)
print(mse)

#clasificacion con caret

data(PimaIndiansDiabetes)
set.seed(7)
trainControl09 <-trainControl(method="cv", number=5)
fit.svmRadial <- train(diabetes~., data=PimaIndiansDiabetes, method="svmRadial",
                       metric="Accuracy", trControl =trainControl09)
print(fit.svmRadial)
plot(fit.svmRadial)

#regresion con caret

data(BostonHousing)
set.seed(7)
trainControl10<-trainControl(method="cv", number=5)
fit.svmRadial<-train(medv~., data=BostonHousing, method="svmRadial",metric="RMSE",
                     trControl=trainControl10)
print(fit.svmRadial)

#Decision Tree (CART)
#instalar rpart.
library(rpart)

#clasifiacion sin caret
data(PimaIndiansDiabetes)
fit <-rpart(diabetes~., data=PimaIndiansDiabetes)
print(fit)
predictions<-predict(fit, PimaIndiansDiabetes[ ,1:8], type="class")
table(predictions, PimaIndiansDiabetes$diabetes)

#regresion sin caret

data(BostonHousing)
fit <- rpart(medv~., data=BostonHousing, control=rpart.control(minsplit = 5))
print(fit)
predictions <-predict(fit, BostonHousing[ ,1:13])
mse<-mean((BostonHousing$medv-predictions)^2)
print(mse)

#clasificacion con caret
data(PimaIndiansDiabetes)
set.seed(7)
trainControl11<-trainControl(method="cv", number=5)
fit.rpart <-train(diabetes~., data=PimaIndiansDiabetes, method="rpart", metric="Accuracy",
                  trControl=trainControl11)
print(fit.rpart)

#regresion con caret
data(BostonHousing)
set.seed(7)
trainControl12<-trainControl(method="cv", number=5)
fit.rpart <- train(medv~., data=BostonHousing, method="rpart", metric="RMSE",
                   trControl=trainControl12)
print(fit.rpart)

##### Rendimiento de Algoritmos ####

#Prepara el entrenamiento con kfold y repeticiones:
trainControl001<-trainControl(method="repeatedcv", number=10, repeats=3)

#CART
set.seed(7)
fit.cart<-train(diabetes~., data=PimaIndiansDiabetes, method="rpart",
                trControl=trainControl001)
#LDA
set.seed(7)
fit.lda<-train(diabetes~., data=PimaIndiansDiabetes, method="lda",
                trControl=trainControl001)
#SVM
set.seed(7)
fit.svmRadial<-train(diabetes~., data=PimaIndiansDiabetes, method="svmRadial",
                trControl=trainControl001)

#KNN
set.seed(7)
fit.knn<-train(diabetes~., data=PimaIndiansDiabetes, method="knn",
                trControl=trainControl001)

#Random Forest
set.seed(7)
fit.rf<-train(diabetes~., data=PimaIndiansDiabetes, method="rf",
                trControl=trainControl001)
#Recoger resultados

results<-resamples(list(CART=fit.cart, LDA=fit.lda, SVM=fit.svmRadial,
                        KNN=fit.knn, RF=fit.rf))

#Comparar los modelos:

#Tabla resumen:
summary(results)
#Boxplots:
scales<-list(x=list(relaton="free"), y=list(relation="free"))
bwplot(results, scales=scales)
#Density Plot
scales<-list(x=list(relation="free"), y=list(relation="free"))
densityplot(results, scales=scales, pch="l")
#Dot Plot:
scales<-list(x=list(relation="free"), y=list(relation="free"))
dotplot(results, scales=scales)
#Pararell Plot:
parallelplot(results)

#Scatterplot Matrix
splom(results)
#Pairwise xyPlots
xyplot(results, models = c("LDA","SVM"))

#Test de Significancia estadisticas
diffs<-diff(results)
summary(diffs)
View(diffs)

#Cargar conjunto de datos
data(Sonar)                                          
dataset<-Sonar
str(Sonar)
?Sonar
x<-dataset[ ,1:60]
y<-dataset[ ,61]
#Probar Algoritmo (mtry (clas. sqrt(x)/ reg. x/3) y ntree)

#Modelo Linea Base
trainControl01<- trainControl( method="repeatedcv", number=10, repeats=3)
seed<-7
metric<-"Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(x)) #Por ser un problema de reg.
tuneGrid1 <- expand.grid(.mtry=mtry) #Hacer busqueda de hiperparametros
rfDefault <- train(Class~., data=dataset, method="rf", metric = metric,
                   tuneGrid = tuneGrid1, trControl=trainControl01 )
print(rfDefault)


#Random Search (Prueba mediante metodos estocasticos)
trainControl01<- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
rfRandom <- train(Class~.,data=dataset, method="rf",metric=metric, tuneLength=15,
                  trControl=trainControl01)
print(rfRandom)
plot(rfRandom)

#Grid Search
trainControl01<- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(seed)
tuneGrid2<-expand.grid(.mtry=c(1:15))
rfGrid <- train(Class~.,data=dataset, method="rf",metric=metric, tuneGrid=tuneGrid2,
                trControl=trainControl01)
print(rfGrid)
plot(rfGrid)

#Usando Herramientas de algoritmo:

#Fase tunning con RF()
set.seed(seed)
bestmtry <- tuneRF(x,y, stepFactor = 1.5, improve = 0.05, ntree=500)
print(bestmtry)

#Usando tu propio algoritmo:

#Tunning Manual
trainControl01<- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tuneGrid3 <-expand.grid(.mtry=sqrt(ncol(x)))
modellist<-list()
for(ntree in c(1000,1500,2000,2500)){
  set.seed(seed)
  fit <-train(Class~., data=dataset, method="rf", metric=metric,
              tuneGrid=tuneGrid3, trControl=trainControl01, ntree=ntree)
  key<-toString(ntree)
  modellist[[key]]<-fit
}
results<-resamples(modellist)
summary(results)
dotplot(results)

#Caret Extendido
customRF<-list(type="Classification", library="randomForest", loop=NULL)
customRF$parameters<-data.frame(parameter=c("mtry","ntree"),
                                class=rep("numeric",2),
                                label=c("mtry","ntree"))
customRF$grid<-function(x,y,len=NULL,search="grid"){}
customRF$fit <-function(x,y,wts,param,lev,last,weights,classProbas,...){
  randomForest(x,y, mmtry = param$mtry, ntree=param$ntree)
}
customRF$predict<-function(modelFit,newdata, preProc=NULL, submodels=NULL)
  predict(modelFit, newdata)
customRF$prob<-function(modelFit, newdata, preProc=NULL, submodels=NULL)
  preduct(modelFit, newdata,type="prob")
customRF$sort<-function(x) x[order(x[,1]), ]
customRF$levels<-function(x) x$classes

#entrenamos el modelo 
install.packages("profvis")
library(profvis)
profvis({
  trainControl01<-trainControl(method="repeatedcv", number=10, repeats=3)
  tuneGrid4<-expand.grid(.mtry=c(1:15), .ntree=c(1000,1500,2000,25000))
  set.seed(seed)
  custom <-train(Class~., data=dataset, method=customRF, metric=metric,
                 tuneGrid=tuneGrid4, trControl=trainControl01)
})
print(custom)
plot(custom)

####Algoritmos Ensamblados####

#Busqueda Random
library(mlbench)
library(caret)
library(caretEnsemble)

#Cargamos los datos
data(Ionosphere)
datasets<-Ionosphere
dataset <- Ionosphere
View(Ionosphere)
dataset <- dataset[ ,-2]
dataset$V1<-as.numeric(as.character(dataset$V1))
head(dataset)

#Algoritmos Boosting
install.packages("C50", repos = "http://R-Forge.R-project.org")
library(caretEnsemble)
library(C50)
trainControl0001<-trainControl(method="repeatedcv", number=10, repeats=3)
seed<-7
metric<-"Accuracy" #problema de clasificacion
#####C5.0######
set.seed(seed)
fit.c50<-train(Class~., data=dataset, method="C5.0", metric=metric, trControl=trainControl0001)
#####Stochastic Gradient Boosting####
set.seed(seed)
fit.gbm<-train(Class~., data=dataset, method="gbm", metric=metric, trControl=trainControl0001,
               verbose=FALSE)

#Summary of Results
boostingResults <- resamples(list(c5.0=fit.c50, gbm=fit.gbm))
summary(boostingResults)
dotplot(boostingResults)


#Algoritmos Bagging

trainControl0002<-trainControl(method="repeatedcv", number=10, repeats=3)
seed<-7
metric<-"Accuracy"

#####Bagged CART#####
set.seed(seed)
fit.treebag <- train(Class~., data=dataset, method="treebag", metric=metric,
                     trControl=trainControl0002)
#####Random Forest#####
set.seed(seed)
fit.rf<- train(Class~., data=dataset, method="rf", metric=metric, trControl=trainControl0002)

#Summary of Results
baggingResults<-resamples(list(treebag=fit.treebag, rf = fit.rf))
summary(baggingResults)
dotplot(baggingResults)


#Algoritmos Stacking

trainControl0003<-trainControl(method="repeatedcv", number=10, repeats=3, 
                               savePredictions=TRUE, classProbs=TRUE)
algorithmList<-c("lda", "rpart", "glm", "knn", "svmRadial")
set.seed(seed)
models<-caretList(Class~., data=dataset, trControl=trainControl0003,
                  methodList=algorithmList)
results<-resamples(models)
summary(results)
dotplot(results)
#Mostrar correlacion de modelos
modelCor(results)
splom(results)

#Stacking usando GLM
stackControl01<-trainControl(method="repeatedcv", number=10, repeats=3, 
                             savePredictions=TRUE, classProb=TRUE)
set.seed(seed)
stack.glm<-caretStack(models, method="glm", metric="Accuracy", trControl=stackControl01)
print(stack.glm)

#Stacking usando RF
set.seed(seed)
stack.rf<-caretStack(models, method="rf", metric="Accuracy", trControl=stackControl01)
print(stack.rf)


####Fase de Forecasting####

#Usaremos el LDA como candidato a produccion (dividiremos 20% y 80%)
library(caret)
library(mlbench)
data(PimaIndiansDiabetes)
set.seed(9)
validationIndex <- createDataPartition(PimaIndiansDiabetes$diabetes, p=0.80, list=FALSE)
View(validationIndex)

validation <- PimaIndiansDiabetes[-validationIndex, ]
train <- PimaIndiansDiabetes[validationIndex, ]
set.seed(9)
trainCotrol001<-trainControl(method="cv", number=10)
fit.lda <- train(diabetes~., data= train, method="lda", metric="Accuracy",
                 trControl=trainControl001)
print(fit.lda)
print(fit.lda$finalModel)

#Para realizar predicciones en el 20% de los datos y corroborar la robustez (fase Forecasting)
set.seed(9)
predictions <- predict(fit.lda, newdata=validation)
confusionMatrix(predictions, validation$diabetes)

#### Modelo independiente - Random Forest
library(caret)
library(mlbench)
library(randomForest)
data(Sonar)
set.seed(7)
## Crear el 20%/80% entrenamiento/validacion
validationIndex <- createDataPartition(Sonar$Class, p=0.80, list=FALSE)
validation <- Sonar[-validationIndex, ]
trainning <-Sonar[-validationIndex,]
## Entrenar y mostrar el resultado
set.seed(7)
trainControl001<-trainControl(method="repeatedcv", number=10, repeats=3)
fit.rf <-train(Class~., data=trainning, method="rf", metric="Accuracy",
               trControl = trainControl001)
print(fit.rf)

## Matriz de confusioon
finalModel <- randomForest(Class~., trainning, mtry=2, ntree=2000)
finalPredictions <- predict(finalModel, validation[ ,1:60])
confusionMatrix(finalPredictions, validation$Class)


##### Guardar y Cargar un Modelo####

#1 Cargar librerias
library(caret)
library(mlbench)
library(randomForest)
data("Sonar")
View(Sonar)
set.seed(7)

#2 Dividir en entrenamiento y validacion
validationIndex <- createDataPartition(Sonar$Class, p=0.80, list=FALSE)
validation <- Sonar[-validationIndex, ]
training <- Sonar[validationIndex, ]

#3 Crear nuestro modelo
set.seed(7)
finalModel <- randomForest(Class~., training, mtry=2, ntree=2000)
summary(finalModel)
finalModel$importance
#4 Guardar y Cargar
saveRDS(finalModel, "./finalModel.rds")
superModel <- readRDS("./finalModel.rds")
print(superModel)

##### Hacer Predicciones en datos no etiquetados
finalPredictions <- predict(superModel, validation[ , 1:60])
confusionMatrix(finalPredictions, validation$Class)









                               