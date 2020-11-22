library(wooldridge) #We load de wooldridge library
data("hprice2") #Loading the dataset
View(hprice2) #We look the data
modelo = lm(scale(price) ~ 0 + scale(crime) + scale(nox) + scale(rooms) + scale(dist) + scale(stratio), data = hprice2)
#With 0, we mark that the the intercept has dropped
summary(modelo)
''' The meaning of this model, is in the slopes.
For example  when rooms has a desviation by 1 to the mean, then
the median house price, presents a desviation by 0,51389 to the mean '''
