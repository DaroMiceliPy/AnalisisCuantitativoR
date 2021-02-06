library("tree")
library("ISLR")

data("Carseats")
High = as.factor(ifelse(Carseats$Sales <= 8, "No", "Yes"))
new_data = data.frame(Carseats, High)
tree.carseats =tree(High???.-Sales , new_data)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0, cex = 0.5)

write.csv(new_data, "C:\\Users\\darioylauri\\Desktop\\treeR.csv")

tree.carseats #Esto nos da las ramas del arbol y los valores de cada nodo terminal

train = sample(1:nrow(new_data), 200)
new_data.test = new_data[-train,]
High.test = High[-train]

''' Creamos el arbol 2, con todas las variables
excepto Sales, en el segundo argumento marcamos el 
dataset, y con el subset decimos que nos entrene exactamente
esas filas que obtuvimos con la funcion sample '''
tree2 = tree(High ~ .-Sales, new_data, subset = train)
''' Una vez obtenido el segundo arbol
entrenado con la muestra seleccionada, vamos a predecir
los valores que nos arroja usando ese mismo modelo,
pero con las caracteristicas o tambien llamadas
variables explicativas de la variable new_data.test '''
pred = predict(tree2, new_data.test, type = "class")
''' Una vez obtenidas las predicciones vamos a compararlas
con los valores reales, que habiamos guardado en la variable
High.test '''
table(pred, High.test)
''' Calculamos entonces el ratio de predicciones correctas'''
correct_predictions = (94 + 53)/200

''' Ahora vamos a poder el arbol de entrenamiento '''

''' Hacemos el podado del arbol indicando que queremos 9 nodos solamente '''
prune.carseats = prune.misclass(tree2, best = 9)
plot(prune.carseats)
text(prune.carseats)

''' Ahora vamos a testear el arbol podado '''
prune.test = predict(prune.carseats, new_data.test, type = "class")
table2 = table(prune.test, High.test)

correct_predictions2 = (88+55)/200

''' Podemos ver como se mejoro el tamaño del arbol y su interpretabilidad y su nivel predictivo'''

