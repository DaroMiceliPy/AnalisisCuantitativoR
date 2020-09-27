library("readxl")
dataset = read_excel("Table 9_4.xls")
dataset = dataset[4:35, ]
colnames(dataset) = c("FRIG", "DUR","D2", "D3", "D4")
View(dataset)
plot(dataset["FRIG"], type="l")  #Esta serie de tiempo no esta desestacionalizada
modelo = lm(FRIG ~ D2 + D3 + D4, data = dataset)
summary(modelo)
"Vemos, que hay un efecto estacional en el segundo, y tercer trimestre, pero no asi en el cuarto"
"Debido a que la pendiente de D4 no es estadisticamente significativa"
"A continuacion graficamos la serie de tiempo desestacionalizada"
"Donde los valores de las abcisas indican, los trimestres desde 1978 a 1985"

plot(summary(modelo)$residuals, type="l", col ="blue")


