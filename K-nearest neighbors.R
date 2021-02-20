library(class)
library(ISLR)
''' Primero debemos crear matrices para las observaciones de entrenamiento
y prueba '''
train = (Smarket$Year < 2005)
x_train = cbind(Smarket$Lag1, Smarket$Lag2)[train,]
x_test = cbind(Smarket$Lag1, Smarket$Lag2)[!train,]
y_train = Smarket$Direction[train]

KNN = knn(x_train, x_test, y_train, k = 1)

table(KNN, Smarket$Direction[!train]) #Creamos la matrix de confusion para ver la precision del modelo


