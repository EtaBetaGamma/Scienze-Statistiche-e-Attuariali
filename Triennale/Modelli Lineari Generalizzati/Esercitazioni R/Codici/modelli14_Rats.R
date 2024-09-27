data(Rats)
View(Rats)
?Rats
Rats$placebo <- ifelse(Rats$group == 1, 1, 0)

#logistica

logr <- glm(s/n ~ placebo + h, weights = n, family = binomial, data = Rats)
summary(logr)

residui <- resid(logr, type = "pearson")
chi2 <- sum(residui^2)
chi2

pchisq(chi2, logr$df.residual, lower.tail = F)
#se pvalue grande no overd, se piccolo si

quasir <- glm(s/n ~ placebo + h, weights = n, family = quasi(link = "logit", variance = "mu(1-mu)"), data = Rats)
summary(quasir)
