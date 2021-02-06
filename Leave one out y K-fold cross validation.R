''' Leave one out cross validation '''

library(ISLR)
library(boot)
''' Otra forma de hacer regresion lineal es con
la funcion glm, pero sin pasar ningun valor en el argumento
family, de forma tal que la funcion interprete que queremos
correr una regresion lineal '''

regresion1 = glm(mpg~horsepower, data = Auto)

cv.err = cv.glm(Auto, regresion1)
cv.err
cv.err$delta #Nos da los resultado de LOOCV

''' Entonces podemos generar los resultados de LOOCV para
cada modelo con distintos grados de polinomio '''

LOOCV.polinomios = rep(1, 6)
for (i in 1:6) {
  regresion = glm(mpg~poly(horsepower,i), data = Auto)
  cv.err2 = cv.glm(Auto, regresion)
  LOOCV.polinomios[i] = cv.err2$delta[1]
}
LOOCV.polinomios
grados = c(1, 2, 3, 4, 5, 6)
plot(grados, LOOCV.polinomios)

''' Podemos ver que el error cuadratico medio disminuye significativamente
del polinomio de grado 1 al polinomio de grado 2, pero luego,
si incrementamos el valor de los grados de los polinomios
con otros modelos, no obtenemos un error cuadratico medio
significativamente mas bajo. Por lo tanto nos quedamos con el
modelo cuadratico'''

''' Para hacer un cross validation por medio de k-fold
usamos la misma funcion cv.glm, ahora extenderemos la prueba
10 modelos, pricisamente con modelos de polinomios del 1 al 10 '''

LOOCV.polinomios = rep(0, 10)
for (k in 1:10) {
  regresion = glm(mpg~poly(horsepower, k), data = Auto)
  cv.err = cv.glm(Auto, regresion, K = 10)
  LOOCV.polinomios[k] = cv.err$delta[1]
}
LOOCV.polinomios

grados = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
plot(grados, LOOCV.polinomios)

''' Vemos que tampoco podemos bajar el error del testeo utilizando
mas grados de polinomio. Esta es otra evidencia mas fuerte para usar
el modelo cuadratico '''

