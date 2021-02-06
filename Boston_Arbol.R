library("tree")
library("MASS")

muestra_entrenamiento = sample(1:nrow(Boston), nrow(Boston)/2)


tree1 = tree(medv???.,Boston[muestra_entrenamiento,])
plot(tree1)
text(tree1)
tree1 #Con esto vemos analiticamente mejor las cosas. 
summary(tree1)



cv.boston =cv.tree(tree1)
plot(cv.boston$size, cv.boston$dev, type = "b") #Vemos como baja la desviacion por cada numero de nodos terminales, entonces nos conviene el podado del arbol

podado = prune.tree(tree1, best = 5)
plot(podado)
text(podado)

#Primero realizamos las predicciones con el modelo podado y el conjunto de testeo
y_predicciones = predict(podado, Boston[-muestra_entrenamiento,])
#Luego guardamos los valores reales en y_real_test
y_real_test = Boston[-muestra_entrenamiento, "medv"]
#Ploteamos para ver los valores reales del test en funcion de y_predicciones
plot(y_predicciones, y_real_test)
#Vemos ademas, el desvio obtenido por cada prediccion media.

sqrt(mean((y_real_test-y_predicciones)^2))

