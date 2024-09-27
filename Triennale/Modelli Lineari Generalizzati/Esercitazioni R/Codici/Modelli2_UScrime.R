data("UScrime")
View(UScrime)
?UScrime
a<- round(cor(UScrime), 3)
View(a)

attach(UScrime)
fit1 <- lm(y ~Po1)
summary(fit1)

#per scegliere le variabili da mettere nel modello utilizziamo il comando add1

fit.tot <- lm(y ~., data = UScrime)

add1(fit1, scope = fit.tot, data = UScrime, test = "F")

fit2 <- update(fit1,.~. + Ineq)
?update
summary(fit2)
?UScrime

add1(fit2, scope = fit.tot, data = UScrime, test = "F")

fit2 <- update(fit2, .~. + Ed)
summary (fit2)

add1(fit2, scope = fit.tot, data = UScrime, test = "F")

fit2 <- update(fit2, .~. + M)
summary(fit2)

add1(fit2, scope = fit.tot, data = UScrime, test = "F")

fit2 <- update(fit2, .~. + Prob)
summary(fit2)

add1(fit2, scope = fit.tot, data = UScrime, test = "F")

fit2 <- update(fit2, .~. + U2)

add1(fit2, scope = fit.tot, data = UScrime, test = "F")

summary(fit2)

