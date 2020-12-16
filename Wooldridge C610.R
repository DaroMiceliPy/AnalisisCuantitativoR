library(wooldridge)
data("bwght2")

model_1 = lm(log(bwght2$bwght) ~ bwght2$npvis + bwght2$npvissq)
summary(model_1)
''' El termino cuadratico es significativo '''

''' Vamos a buscar el maximo utilizando derivadas '''
maxim = (-coefficients(model_1)[2])/(2*coefficients(model_1)[3])
print("El maximo es:")
maxim
data = bwght2[bwght2$npvis >= 22,]$npvis
print("Las mujeres que superan las 22 visitas prenatales son:")
length(data[!is.na(data)])
''' No es razonable que el peso al nacer disminuya con mas de 22 visitas
prenatales.Solo son 21 mujeres las que superan las 22 visitas
prenatales, por lo tanto, se podria ignorar este hecho '''

model_2 = lm(log(bwght2$bwght) ~ bwght2$npvis + bwght2$npvissq + bwght2$fage + I(bwght2$fage^2))
maxm = (-coefficients(model_2)[4])/(2*coefficients(model_2)[5])

percentage = bwght2[bwght2$fage > maxm,]$fage
percentage = 100*(length(percentage[!is.na(percentage)])/1832)
''' La proporcion de mujeres, de la muestra que tienen una edad
mayor a la optima es: '''
percentage

summary(model_2)
''' Creemos que la edad de la madre no explica de manera significativa
el peso del niño al nacer, pero si lo hace el numero de visitas
prenatales, debido a la atencion medica recibida '''

''' Seria mejor utilizar bwght en su nivel original, aunque
utilizar log(bwght) no seria impedimento, debido a que esta
variable no presenta valores negativos '''
