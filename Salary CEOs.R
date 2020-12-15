library(wooldridge)
data("ceosal1")

modelo = lm(log(ceosal1$salary) ~ log(ceosal1$sales) + ceosal1$roe + ceosal1$ros)

values = ceosal1$ros < 0

rosneg = replace(values, c(12, 18, 23, 27, 31, 46, 55, 59, 65, 66, 67, 68, 69, 73, 79, 85, 101, 114, 120, 123, 165, 175), rep(1, 22))
newdata = data.frame(ceosal1, rosneg)

modelo2 = lm(log(newdata$salary) ~ log(newdata$sales) + newdata$roe + newdata$rosneg)
summary(modelo2)

''' The coefficient rosneg is not significative.
Means the semi elasticity of salary with respect to rosneg '''

Semi_elasticity = 100*exp(coefficients(modelo2)[4] - 1)
