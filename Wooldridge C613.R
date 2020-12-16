library("wooldridge")
data("meap00_01")

modelo = lm(meap00_01$math4 ~ meap00_01$lexppp + meap00_01$lenroll + meap00_01$lunch)
summary(modelo)

"La variable explicativa lexppp no es estadisticamente
significativa"
predict(modelo) #Obtenemos las predicciones
newdata = data.frame(meap00_01$exppp, meap00_01$lenroll, meap00_01$lunch)
predict(modelo, newdata, interval="confidence", level=0.95)

"Con la ultima linea obtenemos los intervalos de confianza
de las predicciones"

sort(resid(modelo), TRUE) #Buscamos el residual con mayor valor positivo
meap00_01[1176,2] #Es el bcode de la escuela con mayor residual positivo
''' Creemos que tiene mayor valor positivo ese residual,
debido a que hay cosas positivas que impactan en el porcentaje de
aprobacion de matematicas en 4 grado, que no se estan
teniendo en cuenta. Asi mismo creemos que es la escuela
que entrega mayor valor agregado. '''


modelo2 = lm(meap00_01$math4 ~ meap00_01$lexppp + I(meap00_01$lexppp^2) + meap00_01$lenroll + I(meap00_01$lenroll^2) + meap00_01$lunch + I(meap00_01$lunch^2))
summary(modelo2)
''' No dejariamos en el modelo los terminos
cuadraticos, debido a que no son significativos, y no
tiene sentido dejarlos, ya que no pareciera haber razones
para mantener el termino cuadratico, por ejemplo,
en la variable del logaritmo del gasto por alumno.
Si pareciera tener sentido en la variable del logarimot
del numero de alumnos, pero este no es significativo. '''

modelo3 = lm(scale(meap00_01$math4) ~ scale(meap00_01$lexppp) + scale(meap00_01$lenroll) + scale(meap00_01$lunch))
summary(modelo3)
''' En terminos de desviaciones estandar, el logaritmo
del gasto por alumno pareciera tener mayor efecto en la tasa
de aprobacion, aunque esta variable explicativa no sea muy
significativa. '''
