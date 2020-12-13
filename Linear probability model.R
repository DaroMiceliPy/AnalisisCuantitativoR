library(wooldridge)
data("k401ksubs")
dataOne = k401ksubs[k401ksubs$e401k == 1,]


Percentage = length(dataOne$e401k)/length(k401ksubs$e401k)

modelo = lm(k401ksubs$e401k ~ k401ksubs$inc + k401ksubs$age + I(k401ksubs$age^2) + I(k401ksubs$inc^2) + k401ksubs$male)
summary(modelo)

adjustMod = c(predict(modelo))
any(adjustMod > 1)
any(adjustMod < 0)

True = adjustMod >= 0.5
False = adjustMod < 0.5
Prediction = length(True[True == TRUE])

PercentagePredictionTrue = length(True[True == TRUE])/3637
PercentagePredictionFalse = length(False[False == FALSE])/5638

pie(c(PercentagePredictionTrue, 1 - PercentagePredictionTrue), labels = c("good", "bad"),col = c("deepskyblue", "red"), main = "Percentages of good predictions")

modelo2 = lm(k401ksubs$e401k ~ k401ksubs$inc + k401ksubs$age + I(k401ksubs$age^2) + I(k401ksubs$inc^2) + k401ksubs$male + k401ksubs$pira)
summary(modelo2)
