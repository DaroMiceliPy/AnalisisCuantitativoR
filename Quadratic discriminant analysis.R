library("MASS")
library("ISLR")
library("car")
data(Smarket)

leveneTest(Smarket$Lag1, Smarket$Lag2) #Las varianzas son iguales, no rechazo hipotesis nula
train = (Smarket$Year < 2005)
x_test = Smarket[!train,]
y_test = Smarket$Direction[!train]
QDA = qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
QDA #Nos da la probabilidades a priori y el valor promedio de cada predictor en cada clase
QDA.predict = predict(QDA, x_test)

table(QDA.predict$class, y_test) #Podemos ver la prediccion del modelo
