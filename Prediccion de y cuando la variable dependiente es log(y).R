library(wooldridge)
library(normtest)
data(ceosal2)
model = lm(log(ceosal2$salary) ~ log(ceosal2$sales) + log(ceosal2$mktval) + ceosal2$ceoten, data = ceosal2)
summary(model)
''' Vamos a realizar la estimacion de salary cuando 
sales = 5000, mktval = 10000, y ceoten = 10 '''
jb.norm.test(resid(model))
#Afirmamos la normalidad de los residuos entonces podemos hacer
betas = coefficients(model)
prediccion = betas[1] + betas[2]*log(5000) + betas[3]*log(10000) + betas[4]*10

StdErr = (sqrt(deviance(model)/df.residual(model)))**2

y = exp(StdErr/2)*exp(prediccion)
print("El valor estimado en este paso para sales = 5000,
      mktval = 10000, y ceoten = 10, es igual a")
print(str(y))
