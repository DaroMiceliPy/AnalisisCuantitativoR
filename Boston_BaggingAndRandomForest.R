library(randomForest)
library(MASS)

train = sample(1:nrow(Boston), nrow(Boston)/2)
bagging_boston = randomForest(medv ~., Boston[train,], mtry = 13)
bagging_boston

predicciones = predict(bagging_boston, Boston[-train,])
y_real = Boston[-train, "medv"]
mean((y_real - predicciones)^2)

#Vemos como disminuyo el error cuadratico medio con bagging
#Para ver la desviacion media hacemos
sqrt(mean((y_real - predicciones)^2))

plot(predicciones, y_real)

#Ahora realizamos un random forest pero solamente considerando una cierta
#Cantida de caracteristicas por cada split

random_forest = randomForest(medv~., Boston[train,], mtry = sqrt(13))

predicciones_forest = predict(random_forest, Boston[-train,], importance = TRUE)
mean((y_real - predicciones)^2)

''' Obtuvimos el mismo error cuadratico medio que usando Bagging con forest '''

''' Con esto vemos la importancia de cada predictor ''' 
importance(random_forest)


