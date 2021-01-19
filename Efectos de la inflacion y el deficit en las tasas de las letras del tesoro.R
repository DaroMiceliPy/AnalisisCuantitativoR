library(wooldridge)
data("intdef")

model = lm(intdef$i3 ~ intdef$inf + intdef$def)
summary(model)

''' Las variables explicativas de tasa de inflacion y de deficit fisal son muy
significativas '''

