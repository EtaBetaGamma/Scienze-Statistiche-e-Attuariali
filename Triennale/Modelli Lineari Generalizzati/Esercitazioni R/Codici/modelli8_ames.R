mod.ames <- glm(batteri ~ logquinolinaplus10, family = poisson, data = ames)
summary(mod.ames)
a <- ames$logquinolinaplus10

with(ames, plot(logquinolinaplus10, batteri))
curve(predict(mod.ames, data.frame(logquinolinaplus10 = x), type = "response"), add = T, col = 1, lwd = 2)

mod.ames1 <- glm(batteri ~ quinolina, family = poisson, data = ames)
summary(mod.ames1)

x11()
with(ames, plot(quinolina, batteri))
curve(predict(mod.ames1, data.frame(quinolina = x), type = "response"), add = T, col = 1, lwd = 2)

#intervalli confidenza
prev <- predict(mod.ames, se = T, type = "response")
inf <- prev$fit - qnorm(0.975) * prev$se.fit
sup <- prev$fit + qnorm(0.975) * prev$se.fit
matlines (ames$logquinolinaplus10, cbind(inf, sup), type = "l", lty = rep(2, 2), lwd = rep(2, 2), col = rep(2, 2))



