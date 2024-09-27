titanic.tab <- ftable(xtabs(Count ~ ., data = titanic))
titanic.tab
attach(titanic)

#iniziamo da un modello cosidetto saturo
titanic.sat <- glm(Count ~ Survived * Class * Age * Sex, family = poisson)
summary(titanic.sat)

#il coefficiente è, al netto delle altre variabili, 
#è la quota di quella modalità rispetto al totale

#andiamo ad eliminare le variabili non significative
drop1(titanic.sat, test = "Chisq")

titanic.sat1 <- glm(Count ~ Survived * Class * Sex, family = poisson)
summary(titanic.sat1)

drop1(titanic.sat1, test = "Chisq")

