fish1 <- read.csv("https://stats.oarc.ucla.edu/stat/data/fish.csv")
View(fish)
attach(fish)
#variabile di conteggio -> poisson
#però tanti zeri quindi zip
table(fish$count)
fish$camper <- as.factor(fish$camper)
modzip <- zeroinfl(count ~ camper + child + persons, dist = "poisson")
summary(modzip)
#nello zero infl, c'è prob che zero vs non zero

#modificare il modello camper + child|persons

modzip1 <- zeroinfl(count ~ factor(camper) + child | persons, dist = "poisson", data = fish1)
summary(modzip1)

#valutare l'utilizzo di un'altro modello e interpretare i risultati
#si fa uno zinb

#per il migliore previsione con child=2, camper=1, persons=4
previsionezip <- predict(modzip1, data.frame(camper = 1, child = 2, persons = 4), data = fish1)

detach(fish)
fish1 <- read.csv("https://stats.oarc.ucla.edu/stat/data/fish.csv")
View(fish1)
##exp(variabile nel conteggio), valore atteso del conteggio è uguale alla quota al livello n-1 * l'exp
