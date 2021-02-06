library(gbm)
library(MASS)
train = sample(1:nrow(Boston), nrow(Boston)/2)
boosting = gbm(formula = medv ~ ., Boston[train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)

''' Vamos a ver la influencia de cada caracteristica. Vemos
que el status socioeconomico y la cantidad de cuartos 
son las variables mas importantes'''
summary(boosting)

''' Ahora vamos a ver como se relacionan las variables caracteristicas con el precio de la vivienda '''
plot(boosting,i="rm")
plot(boosting,i="lstat")

''' Ahora vamos a calcular el error cuadratico medio '''

y_real_boosting = Boston[-train, "medv"]
y_predicciones = predict(boosting, Boston[-train,], n.trees = 5000)

mean((y_real_boosting - y_predicciones)^2)

''' Si queremos configurar un boosting con un lambda o penalizacion diferente hacemos '''

boosting2 = gbm(formula = medv ~., Boston[train,], n.trees = 5000, distribution = "gaussian", interaction.depth = 4, shrinkage = 0.2)
