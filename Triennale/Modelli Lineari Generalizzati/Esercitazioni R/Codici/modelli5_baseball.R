library(BradleyTerry2)
data("baseball")
View(baseball)

model1<- BTm(cbind(home.wins, away.wins), home.team, away.team, data = baseball,id = "team")

summary(Model1)

###effetto casa

baseball$home.team<- data.frame(team = baseball$home.team, at.home = 1)
baseball$away.team<- data.frame(team = baseball$away.team, at.home = 0)


modell2<- update(model1, formula = ~ team+at.home)
summary(baseballModel2)

BTabilities(modell2)

bs.qv <- qvcalc(BTabilities(modell2))
bs.qv
plot(bs.qv, levelNames = c("Bal", "Bos", "Cle", "Det", "Mil", "NY", "Tor"))


