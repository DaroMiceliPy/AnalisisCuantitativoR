library(pls)
library(ISLR)
''' Primero hacemos data cleaning '''
data = data.frame(Hitters)
data = data[!is.na(data$Salary),]

samp = sample(1:nrow(data), nrow(data)/2)
Y_test = data$Salary
Y_test = Y_test[(-samp)]

''' Ahora corremos el modelo con el set de entrenamiento '''
pls.fit1 = plsr(Salary~., data = data, subset = samp, scale = TRUE, validation = "CV")
summary(pls.fit1)

''' Miramos donde obtenemos el menor error de testeo '''
validationplot(pls.fit1)

''' Aparentemente obtenemos el menor error cuando el numero
de componentes es igual a 11 '''

pls.fit2 = plsr(Salary~., data = data, ncomp = 11, sacale = TRUE, validation = "CV")
summary(pls.fit2)

predicciones = predict(pls.fit2, data[-samp,])
predicciones = data.frame(predicciones)

mean((Y_test - predicciones$Salary.11.comps)^2) #Obtenemos el error cuadratico medio del modelo


