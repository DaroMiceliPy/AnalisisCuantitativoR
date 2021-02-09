library(pls)
library(ISLR)
library(d)

data = data.frame(Hitters)
data = data[!is.na(data$Salary),]

samp = sample(1:nrow(data), nrow(data)/2)
Y = data$Salary

pcr.fit = pcr(Salary ~., data = data, scale = TRUE, validation = "CV")

''' Con scale = TRUE le indicamos que estandarice las variables. '''
summary(pcr.fit)

''' Ahora vamos a graficar el error cuadratico medio por cada
numero de componentes elegido '''
validationplot(pcr.fit, val.type = "RMSEP")

''' Vemos como el error cuadratico medio disminuye a medida
que crecen el numero de componentes pero no de manera significativa
y clara '''

pcr.fit2 = pcr(Salary~., data = data, subset = samp, scale = TRUE, validation = "CV")

validationplot(pcr.fit2)
''' Vemos que el numero de componentes donde se minimiza
MSE es 5. Por lo tanto calculamos las predicciones
para hacer el MSE del test'''
test = (-samp)
Y_test = Y[test]

pcr.final = pcr(Salary ~ ., data = data, ncomp = 5)
predicciones = data.frame(predict(pcr.final, data[-samp,]))
mean((Y_test - predicciones$Salary.5.comps)^2)

pcr.final = pcr(Salary ~ ., data = data, ncomp = 5)

summary(pcr.final)
