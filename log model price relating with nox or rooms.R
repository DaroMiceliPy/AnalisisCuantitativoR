library(wooldridge)
data("hprice2")
modelo = lm(log(hprice2$price) ~ log(hprice2$nox) + hprice2$rooms)
summary(modelo)
''' Where the -0,71767 is the elasticity of price with respect to nox '''
''' The next example show how amount is the percent change of price
with respect to rooms if deltarooms = 1 '''
percentchange = 100 * (exp(coef(modelo)[3]*1) - 1)
percentchange
