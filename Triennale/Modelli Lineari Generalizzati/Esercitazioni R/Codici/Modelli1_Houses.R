length(Houses.dat)
width(Houses.dat)
attach(Houses.dat)
pch.list <- rep(0, 100)
pch.list[new == "1"] <- 3
pch.list[new == "0"] <- 1
pch.list
col.list <- rep(0, 100)
col.list[new == "1"] <- "blue"
col.list[new == "0"] <- "red"
col.list


fit3 <- lm(price ~ ., data = Houses.dat[,-1]) #tutte le variabili
summary(fit3)

fit2 <- lm(price ~ taxes + size + new, data = Houses.dat)
summary(fit2)

#procedura Sepwise: come capire quante variabili usare
#passo 1: aggiungo variabile più correlata
#passo due: la più correlata al netto della prima e così via
#si può fare anche al contrario
#la mista: a ogni passo introduco o rimuovo variabili 

a <- step(fit3, direction = "both")
a
summary(a)

fit.prova <- lm(price ~ new)
summary(fit.prova)
