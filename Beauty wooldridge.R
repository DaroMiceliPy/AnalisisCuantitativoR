library(wooldridge)
data("beauty")

male_abavg = length(beauty[beauty$abvavg == 1 & beauty$female == 0, ]$female)
male = length(beauty[beauty$female == 0,]$female)
percentage_male = 100*(male_abavg/male)

female_abavg = length(beauty[beauty$abvavg == 1 & beauty$female == 0,]$female)
female = length(beauty[beauty$female == 1,]$female)
percentage_female = 100*(female_abavg/female)

lpm = lm(beauty$abvavg ~ beauty$female)
summary(lpm)
pvalueRigth = 0.14/2

dataMan = beauty[beauty$female == 0,]
modelMan = lm(log(dataMan$wage) ~ dataMan$belavg + dataMan$abvavg)
summary(modelMan)

dataWomen = beauty[beauty$female == 1,]
modelWomen = lm(log(dataWomen$wage) ~ dataWomen$belavg + dataWomen$abvavg)
summary(modelWomen)

''' Ho: B1 = 0 means that the coefficient B1, does not
has effect in log(wage), and the H1: B1 < 0, means
that the coefficient B1 has an effect negative in
log(wage)''' 

''' there are not evidence that the Women with a
beauty above the mean has a wage beyond to the mean '''

modelMan1 = lm(log(dataMan$wage) ~ dataMan$belavg + dataMan$abvavg + dataMan$educ + dataMan$exper + I(dataMan$exper^2) + dataMan$union + dataMan$goodhlth + dataMan$black + dataMan$married + dataMan$south + dataMan$bigcity + dataMan$smllcity + dataMan$service)
summary(modelMan1)

modelWomen1 = lm(log(dataWomen$wage) ~ dataWomen$belavg + dataWomen$abvavg + dataWomen$educ + dataWomen$exper + I(dataWomen$exper^2) + dataWomen$union + dataWomen$goodhlth + dataWomen$black + dataWomen$married + dataWomen$south + dataWomen$bigcity + dataWomen$smllcity + dataWomen$service)
summary(modelWomen1)
