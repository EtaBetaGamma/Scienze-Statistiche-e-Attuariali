data("Stroke")
View(Stroke)
?Stroke
data("Stroke1")
View(Stroke1)
week <- 1:8
plot(week, rep(100, 8), type = "n", ylab = "abilità funzionale", ylim = c(5, 120))

for(i in 1:8) lines(week, Stroke[i, 3:10], lty = 1, col = 1)

for(i in 9:16) lines(week, Stroke[i, 3:10], lty = 2, col = 2)

for(i in 17:24) lines(week, Stroke[i, 3:10], lty = 3, col = 3)

mediaA <- apply(Stroke[Stroke$Group == "A", 3:10], 2, mean)
mediaB <- apply(Stroke[Stroke$Group == "B", 3:10], 2, mean)
mediaC <- apply(Stroke[Stroke$Group == "C", 3:10], 2, mean)

lines(week, mediaA, lty = 1, col = 1)
lines(week, mediaB, lty = 1, col = 2)
lines(week, mediaC, lty = 1, col = 3)

round(cor(Stroke[, 3:10]), 3)

caso <- NULL
for(i in 1:24) caso <- c(caso, rep(i, 8))
caso

gruppo <- c(rep("A", 64), rep("B", 64), rep("C", 64))
settimana <- rep(1:8, 24)
gruppo

y <- as.vector(t(Stroke[, 3:10]))
strokefin <- data.frame(caso, gruppo, settimana, y)
View(strokefin)

#tutto fatto per modificare il dataset e renderlo più leggibile

#modello

?gee

#modello incorrelato
mod1 <- gee(y ~ gruppo + settimana + gruppo:settimana, id = caso, family = gaussian, corstr = "independence" , data = strokefin)

summary(mod1)

mod2 <- lm(y ~ gruppo + settimana + gruppo:settimana, id = caso, family = gaussian, corstr = "independence" , data = strokefin)

summary(mod2)

#modello equicorrelato
mod3 <- gee(y ~ gruppo + settimana + gruppo:settimana, id = caso, family = gaussian, corstr = "exchangeable" , data = strokefin)
summary(mod3)

#modello autoregressivo
mod4 <- gee(y ~ gruppo + settimana + gruppo:settimana, id = caso, family = gaussian, corstr = "AR-M" , data = strokefin)
summary(mod4)

#significatività
beta <- mod4$coefficients[5:6]
cov <- mod4$robust.variance[5:6, 5:6]
waldtest <- t(beta) %*% solve(cov) %*% beta
pchisq(waldtest, 2, lower.tail = F)

#modello a effetti fissi e casuali
modlme <- lme(y ~ gruppo * settimana, random = ~ 1|caso, data = strokefin)
#modello a intercetta random
summary(modlme)
?lme
randomeffect <- ranef(modlme)

modlme.1 <- lme(y ~ gruppo * settimana, random = ~ 1|caso, correlation = corAR1(form = ~ 1|caso), data = strokefin)
summary(modlme.1)

#modello a effetti fissi e casuali
modlme <- lme(y ~ gruppo * settimana, random = ~ 1|caso, data = strokefin)
#modello a intercetta random
summary(modlme)
?lme
randomeffect <- ranef(modlme)

#modello con intercetta e  coefficiente random
modlme.2 <- lme(y ~ gruppo * settimana, random = ~ 1 + gruppo|caso,  data = strokefin)
summary(modlme.2)

randomeffect1 <- ranef(modlme.2)
randomeffect1

predict(modlme.2)

anova(modlme, modlme.2)
