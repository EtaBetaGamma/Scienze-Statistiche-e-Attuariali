data("Ants")
View(Ants)
?Ants

Ants$Bread <- as.factor(Ants$Bread)
Ants$Filling <- as.factor(Ants$Filling)
Ants$Butter <- as.factor(Ants$Butter)
pois1 <- glm(Ant_count ~ Bread + Butter + Filling, data = Ants, family = "poisson")
summary(pois1)
?glm

pois2 <- glm(Ant_count ~ Bread + Butter * Filling, data = Ants, family = "poisson")
summary(pois2)

#verifica overdispersion

x2 <- sum(residuals(pois2, type = "pearson")^2)
x2
pchisq(x2, 39, lower.tail = F)
?pchisq
#39 sono i gradi di libertà deli residui del modello (ce lo da in outpu)

phistim <- x2/39
phistim

modquasi <- glm(Ant_count ~ Bread + Butter * Filling, data = Ants, family = quasi (link = "log", variance = "mu"))
summary(modquasi)



