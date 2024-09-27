data("citations")
View(citations)
head(citations)
cit.sf <- countsToBinomial(citations)
names(cit.sf)[1:2] <- c("j1", "j2")
View(cit.sf)
citmodel <- BTm(cbind(win1, win2), j1, j2, id = "journal", data = cit.sf)
summary(citmodel)


##pareggio
ties <- 5 + 0*citations
ties[2, 1] <- ties[1, 2] <- 9
ties

modelpari <- vglm(Brat(citations, ties) ~ 1, bratt(refgp = 1), trace = T)
summary(modelpari)
