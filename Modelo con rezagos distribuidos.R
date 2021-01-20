library(wooldridge)
data("fertil3")

model = lm(fertil3$gfr ~ fertil3$pe + fertil3$pe_1 + fertil3$pe_2)
summary(model)

Propension_impacto = model$coefficients[1]
Propension_largo_plazo = sum(model$coefficients) - model$coefficients[1]
