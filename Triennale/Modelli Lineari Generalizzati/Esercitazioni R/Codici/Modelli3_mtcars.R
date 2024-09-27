data("mtcars")
View(mtcars)
?mtcars
correlazioni <- round(cor(mtcars), 3)
View(correlazione)
attach(mtcars)
fit1 <- lm(mpg ~ ., data = mtcars)
summary(fit1)
step(fit1, direction = "backward")
fit1 <- lm(mpg ~ wt + qsec + am)
summary(fit1)
table(am, cyl)

#analisi dei residui

res <- fit1$residuals
plot(res)
abline(h = 0)
round(mean(res), 6)

#hp meida=0
test.residui.media <- t.test(res)
test.residui.media

#hp di normalità
qqnorm(scale(res))
abline(0, 1)
remove(qqline)
test.norm <- shapiro.test(res)
test.norm
?shapiro.test

#verifica omoschedasticità, test breusch-pagan: H_0 varianze tutte uguali 
mod <- formula(fit1)
testbp <- bptest(mod, data = mtcars)
testbp
?bptest

#verifica autocorrelazione, test darwin-watson: H_0 autocorrelazioni nulle

testdw <- dwtest(mod, data = mtcars)
testdw

#intervallo di confidenza
confint(fit1)
summary(fit1)

#punti leverage e anomali
par(mfrow = c(3,2))
for(i in 1:6) plot(fit1, which = i)
x11()
plot(fit1, which = 6)

cook <- cooks.distance(fit1)
x11()
plot(cook)

identify(1 : length(cook), cook, rownames (mtcars))
