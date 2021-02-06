library(tree)
library(gbm)

data = read.csv("C:\\Users\\darioylauri\\Documents\\Practices in python\\Machine learning\\CEOSAL2.csv")

''' Vamos a predecir si el salario de un ceo va a superar
los 1000 dolares. Usamos el dataset de los años 90s '''

train = sample(1:nrow(data), 118)

tree1 = tree(salary ~ age + college + grad + comten + ceoten + sales + profits + mktval + profmarg, data[train,])

plot(tree1)
text(tree1)
summary(tree1)
tree1

''' Ahora vamos a ver el error cuadratico medio '''

y_real = data[-train, "salary"]

y_predicciones = predict(tree1, data[-train,])
mean((y_real - y_predicciones)^2)

''' Ahora vamos a ver si el podado del arbol nos ayuda a 
disminuir el error cuadratico medio '''

''' Ahora vemos si podemos bajar el desvio modificando el tamaño del arbol '''
cross = cv.tree(tree1)
plot(cross$size, cross$dev)
''' Vemos que podemos hacerlo de manera muy practica, elegimos por lo tanto que el
podado tenga 6 nodos terminales '''
podado = prune.tree(tree1, best = 6)
plot(podado)
text(podado)

y_predicciones_prune = predict(podado, data[-train,])

mean((y_real - y_predicciones_prune)^2)

''' Vemos como el error cuadratico medio disminuyo '''


