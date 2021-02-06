library(ISLR)


entrenamiento = sample(392,196)

regresion1 = lm(mpg~horsepower, data = Auto, subset = entrenamiento)

predicciones = predict(regresion1, Auto[-entrenamiento,])
mean((Auto[-entrenamiento, "mpg"] - predicciones)^2)

regresion2 = lm(mpg~horsepower+I(horsepower^2), Auto, subset = entrenamiento)
predicciones2 = predict(regresion2, Auto[-entrenamiento,])
mean((Auto[-entrenamiento, "mpg"] - predicciones2)^2)

''' Vemos como el error del test disminuyo al añadir
un grado mas de polinomio en el modelo. Vamos a ver un polinomio
de grado 3'''

regresion3 = lm(mpg ~ horsepower + I(horsepower^2) + I(horsepower^3), Auto, subset = entrenamiento)
predicciones3 = predict(regresion3, Auto[-entrenamiento,])
mean((Auto[-entrenamiento, "mpg"] - predicciones3)^2)

''' Vemos como el error del test (error cuadratico medio)
aumento ligeramente. Eso quiere decir que a medida que aumentamos
el grado del polinomio disminuimos el error del test hasta cierto
punto, pero luego empieza a subir al seguir aumentado el grado
del polinomio '''

summary(regresion1)
summary(regresion2)
summary(regresion3)


''' En todos los casos vemos que el error del entrenamiento disminuyo.
Por ello a la hora de elegir modelos predictivos es mas importante
fijarnos en el error cudratico medio del test y no del entrenamiento '''

