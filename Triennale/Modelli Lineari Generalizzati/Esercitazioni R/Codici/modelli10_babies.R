babies$survival <- factor(babies$survival)
babies$clinic <- factor(babies$clinic)
babies$prebirth <- factor(babies$prebirth)

babies.tab <- ftable(xtabs(counts~clinic + prebirth + survival, data = babies))
View(babies.tab)
addmargins(babies.tab)

babies.CPS <- glm(counts ~ clinic * prebirth * survival, family = poisson, data = babies)
summary(babies.CPS)

drop1(babies.CPS, test = "Chisq")

babies.ao <- update(babies.CPS, . ~ . - clinic:prebirth:survival)
summary(babies.ao)

drop1(babies.ao, test = "Chisq")


babies.ao2 <- update(babies.ao, . ~ . - prebirth:survival)
summary(babies.ao2)

babies.tab


drop1(babies.ao2, test = "Chisq")

babies.fit <- ftable(xtabs(babies.ao2$fitted.values ~ survival + prebirth + clinic , data = babies))
babies.fit

oddratio <- (293*3)/(4*176)
oddratio
odd2 <- (292.632353*2.632353)/( 4.367647 * 176.367647)
odd2
#se oddratio è 1, non c'è differenza sostazniale tra le quote
#la relazione tra due variabili vale indipendentemente dalla terza 

((292.632353 + 23.012552)/(4.367647 + 1.987448))/((176.367647 + 196.987448)/(2.632353 + 17.012552))

babies.tab <- ftable(xtabs(counts~clinic + prebirth + survival, data = babies))
babies.tab

babies.add <- glm(counts ~ survival + prebirth + clinic, data = babies)
summary(babies.add)

add1(babies.add, scope = ~survival*prebirth*clinic, test = "Rao")


babies.add1 <- update(babies.add, .~. + prebirth:clinic)
add1(babies.add1, ~.^2,  test = "Rao")
babies.add2 <- update(babies.add1, .~. + survival:clinic)
add1(babies.add2, ~.^2, test = "Rao")

#scompone in due la tabella a 3
xtabs(babies.add2$fitted ~ clinic + survival + prebirth, data = babies)

#legame con logit

survavalbis <- babies$survival-1

survavalbis

babieslog <- glm(survavalbis ~ factor(clinic) + factor(prebirth), family = binomial, weights = counts, data = babies)

summary(babieslog)

babieslog2 <- glm(survavalbis ~ factor(clinic), family = binomial, weights = counts, data = babies)
babieslog2
summary(babieslog2)



