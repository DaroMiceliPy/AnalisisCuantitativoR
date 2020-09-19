data = data.frame(año = c(1970:1995), saving = c(61, 68.6, 63.6, 89.6, 97.6, 104.4, 96.4, 92.5, 112.6, 130.1,
                                                 161.8, 199.1, 205.5, 167, 235.7, 206.2, 196.5, 168.4, 189.1,
                                                 187.8, 208.7, 246.4, 272.6, 214.4, 189.4, 249.3),
                  income = c(727.1, 790.2, 855.3, 965, 1054.2, 1159.2, 1273, 1041.4, 1580.1, 1769.5, 1973.3, 2200.2, 2347.3, 2522.4,
                              2810, 3002, 3187.6, 3363.1, 3640.8, 3894.5, 4166.8, 4343.7, 4613.7, 4790.2, 5021.7, 5320.8))

model1 = lm(saving ~ income, data = data) #The model since 1970 to 1995
model2 = lm(saving ~ income, data = data[1:12, ])#The model since 1970 to 1981
model3 = lm(saving ~ income, data = data[13:26, ])#The model since 1982 to 1995
#We can see how the the betas in each model are differents
#We will to try out the suppose of the chow proof
plot(saving ~ income, data = data[1:12, ], main="model2")
abline(model2, col="blue")
plot(saving ~ income, data = data[13:26, ], main="model3")
abline(model3, col="red") #We have some differences 


resid2 = summary(model2)$residuals
resid3 = summary(model3)$residuals
hist(resid2, freq = FALSE, main = "histogram of residuals: model 2", col = "orange")
mean2 = mean(resid2)
sd2 = sd(resid2)
values = rnorm(10, mean = mean2, sd = sd2)
lims = c(mean2 - 3*sd2, mean2 + 3*sd2)
function2 = function(x) { dnorm(x, mean = mean2, sd = sd2)}
function2(values)
curve(function2, xlim = lims, add = TRUE, col = "red", lwd = 2)

ajb.norm.test(resid2) #The p value is 0.8355, and we can to say that the residuals errors follows a normal distribution


hist(resid3, freq = FALSE, main = "histogram of residuals: model 3", col = "yellow")
mean3 = mean(resid3)
sd3 = sd(resid3)
values = rnorm(10, mean = mean3, sd = sd3)
lims = c(mean3 - 3*sd3, mean3 + 3*sd3)
function3 = function(x) { dnorm(x, mean = mean3, sd = sd3)}
function3(values)
curve(function3, xlim = lims, add = TRUE, col = "blue", lwd = 2)

ajb.norm.test(resid3)#The p value is 0.531, and we can to say that the residuals errors follows a normal distribution

#And now, we can to do the proof of chow
SigmaSquareModel2 = (summary(model2)$sigma)**2
SigmaSquareModel3 = (summary(model3)$sigma)**2

pvalueF = 1 - pf(SigmaSquareModel3/SigmaSquareModel2, 14 - 2, 12 - 2)
pvalueF #The p value is very small. We can say that there is a change structural in 1982


