library("ISLR")
names(Smarket) #Nos tira los nombres de las columnas
View(Smarket)
data(Smarket)

dim(Smarket) #Nos dice la cantidad de filas y columnas

summary(Smarket) #Hace un resumen de estadisticas descriptivas
cor(Smarket[,-9]) #Armamos una matrix de correlacion entre cada columna omitiendo la variable cualitativa

hist(Smarket$Lag1, col = "red") #Trazamos un histogramas de retornos diarios
shapiro.test(Smarket$Lag1) #Hay una evidencia fuerte contra la normalidad de los residuos
plot(Smarket$Volume) #Ploteamos la evolucion del volumen para el indice S&P

''' Vamos a correr una regresion logistica para predecir el valor de
direction, en funcion de lag1 a lag5, y Volume '''
exists(is.null(Smarket))
glm.fit = glm(Smarket$Direction ~ Smarket$Lag1+Smarket$Lag2+Smarket$Lag3+Smarket$Lag4+Smarket$Lag5+Smarket$Volume, family = binomial)

summary(glm.fit)

''' No hay muchas evidencias para decir que los retornos de dias anteriores
puedan explicar de alguna manera que el valor del indice cierre al alza o 
la baja '''
''' Ahora vamos a tratar de predecir las probabilidades con las variables
de entrenamiento '''
probs = predict(glm.fit, type = "response")
probs #Estas probabilidades indican la probabilidad de que el indice se encuentre al alza
contrasts(Smarket$Direction)
''' Ahora vamos a transformar las probabilidades, para que nos diga directamente
si el indice sera al alza cuando la probabilidad predicha supere al 0.5, de lo
contrario seria a la baja '''
probs[probs > 0.5] = "Up"
probs[probs <= 0.5] = "Down"
probs

table(probs, Smarket$Direction) #Entonces creamos una matrix de confusion para saber la precision del modelo

''' Aunque esto no nos dice nada acerca de la verdadera precision del modelo
puesto que solo estamos utilizando las observaciones de entrenamiento '''
''' Vamos a utilizar como observaciones de entrenamientos, las observaciones
entre 2001 y 2004 '''
vector = (Smarket$Year < 2005)
''' Creamos las observaciones para el test '''
x_test = Smarket[!train,]
y_test = Smarket$Direction[!train]

glm.fit = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial, subset = train)
glm.predict = predict(glm.fit, x_test, type = "response") #Generamos las predicciones

glm.predict[glm.predict > 0.5] = "Up"
glm.predict[glm.predict <= 0.5] = "Down"
table(glm.predict, y_test) #Ahora creamos la matrix de confusion
''' Esto quiere decir que no podemos predecir el comportamiento del indice
a partir del volumen y los retornos pasados '''

''' Pero podemos mejorar el modelo a partir de reducir el numero de predictores. Esto 
lo tomamos en cuenta por los p valores arrojados cuando entrenamos el primer
modelo con todos los predictores'''

glm.fit = glm(Direction ~ Lag1+Lag2, data = Smarket, family = binomial)
glm.predict = predict(glm.fit, x_test, type = "response")
glm.predict[glm.predict > 0.5] = "Up"
glm.predict[glm.predict <= 0.5] = "Down"
table(glm.predict, y_test)

''' Podemos ver en la nueva matrix de confusion como mejoro la precision del modelo '''


