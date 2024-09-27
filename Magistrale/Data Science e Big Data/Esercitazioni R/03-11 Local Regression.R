### POLINOMIAL REGRESSION ###

library(forecast)
library(splines)
library(ISLR)
library(earth)


# Dataset. ----------------------------------------------------------------


data(Wage) #salario di 3000 uomini
attach(Wage)
head(Wage)
detach(Wage)


# Regressione Polinomiale -------------------------------------------------


#regressione polinomiale

fit.poly<-lm(wage~poly(age,4),data=Wage)
summary(fit.poly)

agelims<-range(age)
age.grid<-seq(from=agelims[1], to=agelims[2])

#obiettivo= previsione sulla variabile dipendente per ogni valore della variabile esplicativa age

prev<-predict(fit.poly, newdata=list(age=age.grid), se=TRUE)
prev

plot(age,wage,xlim=agelims, cex=0.4, col="grey")
lines(age.grid,prev$fit, lwd=2, col="red")

#i punti sopra nel grafico non sono outliers ma si comportano diversamente

se.bands<-cbind(prev$fit+2*prev$se.fit, prev$fit-2*prev$se.fit)
se.bands

matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

#lines(se.bands[,1],lwd=1, col="orange")
#lines(se.bands[,2],lwd=1, col="orange")



# Local Regression --------------------------------------------------------


### LOCAL REGRESSION ###  #span=h

fit.loc<-loess(wage~age,data=Wage, span=0.8)
summary(fit.loc)
fit.loc

plot(age,wage,xlim=agelims, cex=0.4, col="grey")
lines(age.grid,prev$fit, lwd=2, col="red")

lines(age.grid, predict(fit.loc,data.frame(age=age.grid)),lwd=2, col="green")

fit.loc2<-loess(wage~age,data=Wage, span=0.2)
summary(fit.loc2)
fit.loc2

lines(age.grid, predict(fit.loc2,data.frame(age=age.grid)),lwd=2, col="blue")

fit.loc3<-loess(wage~age,data=Wage, span=0.05)
summary(fit.loc3)
fit.loc3

lines(age.grid, predict(fit.loc3,data.frame(age=age.grid)),lwd=2, col="yellow")

#span = 0.5
#plinomial grado6
#campione test 20%
#ptrediction su test


n<-nrow(Wage)
indice<-sample(1:nrow(Wage), 0.8*nrow(Wage))
indice

train<-Wage[indice,]
dim(train)
dim(Wage)

test<-Wage[-indice,]
dim(test)

fit.poly2<-lm(wage~poly(age,6),data=train)
modelsumm2<-summary(fit.poly2)

fit.loc4<-loess(wage~age,data=train, span=0.5)
modelsumm1<-summary(fit.loc4)
fit.loc4
prev1<-predict(fit.poly2, newdata=test, se=TRUE)
prev1

prev2<-predict(fit.loc4,data.frame(test), se=TRUE)
prev2


y.test<-wage[-indice]
mse1<-(mean(y.test - prev1$fit)^2)
mse1

mse2<-(mean(y.test - prev2$fit)^2)
mse2

#meglio modello1 quindi plinomiale



# basic spline ------------------------------------------------------------------
fit.spline <- lm(wage ~ bs(age, knots = c(20, 30, 50, 70)), data = Wage)
summary(fit.spline)

pred <- predict(fit.spline, newdata = list(age = age.grid), se = T)
plot(age, wage, xlim = agelims, col = "black")
lines(age.grid, pred$fit, lwd = 2, col = "red")
abline(v = 20, col = "blue")
abline(v = 30, col = "blue")
abline(v = 50, col = "blue")
abline(v = 70, col = "blue")

x11()
plot(age, wage, xlim = agelims, col = "black")
lines(age.grid, pred$fit, lwd = 2, col = "red")
abline(v = 25, col = "blue")
abline(v = 40, col = "blue")
abline(v = 60, col = "blue")

##scelta dei nodi
fit.spline2 <- lm(wage ~ bs(age, df = 17), data = Wage)
pred2 <- predict(fit.spline2, newdata = list(age = age.grid), se = T)
summary(fit.spline2)
knots <- attr(bs(age, df = 4), "knots")
knots
##la differenza tra df e nodi è sempre 3
x11()
plot(age, wage, xlim = agelims, col = "black")
lines(age.grid, pred2$fit, lwd = 2, col = "red")
abline(v = 37, col = "blue")
abline(v = 48, col = "blue")


# natural spline ----------------------------------------------------------

fit.natspline <- lm(wage ~ ns(age, df = 5), data = Wage)
pred3 <- predict(fit.natspline, newdata = list(age = age.grid), se = T)

plot(age, wage, xlim = agelims, col = "black")
lines(age.grid, pred3$fit, lwd = 2, col = "red")
#in natural spline df - knots = 1
#in natural spline rispetto a basic spline, ha più vincoli sulle code



par(mfrow = c(1, 2))
attr(ns(age, df = 5), "knots")


# smoothing spline --------------------------------------------------------

fit.smooth <- smooth.spline(age, wage, df = 15)

plot(age, wage, xlim = agelims, col = "black")
lines(fit.smooth, col = "red", lwd = 2)
fit.smooth$lambda


smooth2 <- smooth.spline(age, wage, cv = T)





# MARS --------------------------------------------------------------------

data(trees)
View(trees)
?trees

mod_mars <- earth(Volume ~ ., data = trees)
plotmo(mod_mars)
summary(mod_mars)
summary(mod_mars, digits = 4, style = "pmax")
#i coefficienti si valutano rispetto ad a
plot(mod_mars)

marsprev <- predict(mod_mars)

predict(mod_mars, data.frame(Girth = 15, Height = 70))
plot(trees[, 3])
points(marsprev, col = "red")

girthnew <- seq(4, 20, by = 2)
heightnew <- seq(50, 90, by = 5)
predict(mod_mars, list(girthnew, heightnew))

mod_mars2 <- earth(Volume ~ ., data = trees, degree = 2)
plotmo(mod_mars2)




