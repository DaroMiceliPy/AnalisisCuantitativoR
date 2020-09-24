#Vamos a ver la funcion par
objeto = par()
par(col.lab = "red") #Le ponemos de color rojo a las etiquetas
hist(mtcars$mpg)
#Tambien podemos
hist(mtcars$mpg, col.lab="red")
#Los simbolos para graficar van desde el 1 al 25 dandole valor al pch
plot(c(1, 2, 3), c(1, 2, 3), pch=21)
#lwd es el ancho del grosor de una linea al graficar
text(3, 3, "Hola mundo, fuente por omision") #Grafica letras

#Para graficar el color de cada dato de iris por un color diferente podemos usar
plot(iris, col=iris$Species, pch=21)
legend(x = 4.5, y = 7, legend = levels(iris$Species), col = c(1:3), pch = 16)
#Esto usamos para ponerle nombres a los puntos
