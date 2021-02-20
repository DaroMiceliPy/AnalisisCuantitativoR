library("MASS")
library("ISLR")
data(Smarket)
train = (Smarket$Year < 2005)
x_test = Smarket[!train,]
y_test = Smarket$Direction[!train]
LDA = lda(Direction ~ Lag1+Lag2, data = Smarket, subset = train)
LDA #Con esto vemos las probabiidades a priori y las medias de cada grupo. Asi como tambien los coeficientes del modelo
LDA.predict = predict(LDA, x_test)

''' Ahora vamos a armar la matrix de confusion '''
table(LDA.predict$class, y_test)
max(LDA.predict$posterior) #Con esto sacamos la probabilidad a posteriori mas alta de que el mercado se encuentre al alza


