data("Puromycin")
?Puromycin
attach(Puromycin)
?glm

fit <- glm(state ~ conc + rate, family = "binomial")
summary(fit)
(exp(fit$coefficients)-1)*100

#conc=1 e rate=100

previsione <- predict(fit, newdata = data.frame("conc" = 1, "rate" = 100), type = "response")

previsione

######previsione generalizzato

fit2 <- glm(state ~ rate, family = "binomial")

summary(fit2)

previsioni1 <- predict(fit2, newdata = Puromycin, type = "response")
previsioni1

pred <- ifelse(previsioni1 > 0.5, "untreaded", "treaded")
pred

matrice_confusione <- table(Puromycin$state, pred)
matrice_confusione

levelgood <- sum(diag(matrice_confusione))/sum(matrice_confusione)
levelgood

mean(pred == Puromycin$state)

curvaroc <- roc(Puromycin$state ~ previsioni1)
plot(curvaroc)

auc(curvaroc)
