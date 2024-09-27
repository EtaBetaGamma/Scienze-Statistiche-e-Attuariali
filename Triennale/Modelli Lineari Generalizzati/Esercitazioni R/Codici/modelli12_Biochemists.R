data("Biochemists")
View(Biochemists)
?Biochemists

attach(Biochemists)
table(Biochemists$art)

mean(Biochemists$art)
var(Biochemists$art)

hist(art)

plot(art ~ fem)
plot(art ~ mar)
plot(art ~ ment)
abline(lm(art ~ ment), col = 3)

mod.lin <- lm(art ~ ment)
summary(mod.lin)

marfem <- mar:fem
marfem
plot(art ~ marfem)

pois1 <- glm(art ~ ., family = poisson, data = Biochemists)
summary(pois1)

step(pois1)

pois2 <- update(pois1, .~. - phd)
summary(pois2)

add1(pois2, .~(.)^2, test = "Chisq")

#frequenze relative
oss <- prop.table(table(Biochemists$art))[1:8]
oss

#creiamo frequenze teoriche
pois.exp <- sapply(0:7, function(x) mean(dpois(x, fitted(pois2))))
pois.exp

cbind(oss, pois.exp)
difff <- sum(oss - pois.exp)
difff
#negativa binomiale

mod.bn <- glm.nb(art ~. , data = Biochemists)
summary(mod.bn)

mod.bn2 <- update(mod.bn, .~. - phd)
summary(mod.bn2)

step(mod.bn2)

mod.bn3 <- update(mod.bn2, .~. - mar)
summary(mod.bn3)

add1(mod.bn2, .~(.)^2, test = "Chisq")

nb.exp <- sapply(0:7, function(x) mean(dnbinom(x, m = fitted(mod.bn2), size = mod.bn2$theta)))

ab <- cbind(oss, pois.exp, nb.exp)
tot <- colSums(ab)
tot

n <- nrow(Biochemists)

chiquadroPois <- n*((sum((ab[,1]-ab[,2])^2/ab[,2])) + (tot[1] - tot[2])^2/(1 - tot[2]))
chiquadroPois

pchisq(chiquadroPois, 6, lower.tail = F)

chiquadronb <- n*((sum((ab[,1]-ab[,3])^2/ab[,3])) + (tot[1] - tot[3])^2/(1 - tot[3]))
pchisq(chiquadronb, 6, lower.tail = F)
#modello ZIP

mod.zip <- zeroinfl(art ~ ., data = Biochemists)
summary(mod.zip)

zip2 <- zeroinfl(art ~ fem + kid5 + ment |ment)
summary(zip2)

?zeroinfl

#lrt test
trv <- 2*(mod.zip$loglik - zip2$loglik)
pchisq(trv, 6, lower.tail = F)

#previsioni

coefficienti <- coef(zip2)
x <- model.matrix(zip2)
x
p <- ncol(x)

mustim <- as.vector(exp(x %*% coefficienti[1:p]))

cphistim <- plogis(coefficienti[p+1] + coefficienti[p+2] * Biochemists$ment)
phistim <- 1 - cphistim
zip.exp <- sapply(0:7, function(x) mean(phistim * dpois(x, mustim)))
zip.exp[1] <- zip.exp + mean(cphistim)

ab <- cbind(oss, pois.exp, nb.exp, zip.exp)

tot <- colSums(ab)
tot

n <- nrow(Biochemists)

chiquadrozip <- n*((sum((ab[,1]-ab[,4])^2/ab[,4])) + (tot[1] - tot[4])^2/(1 - tot[4]))
chiquadrozip

#zinb
zinb <- zeroinfl(art ~ fem + kid5 + ment | ment, data = Biochemists, dist = "negbin" )

summary(zinb)

coezinb <- coef(zinb)
x1 <- model.matrix(zinb)
p1 <- ncol(x1)
mustim2 <- as.vector(exp(x1 %*% coezinb[1:p1]))
cphistim2 <- plogis(coezinb[p1+1] + coezinb[p1 + 2] * Biochemists$ment)
phistim1 <- 1 - cphistim2
thetastim1 <- zinb$theta
zinbexp <- sapply(0:7, function(x) mean(phistim1 * dnbinom(x, m = mustim2, size = thetastim1)))
zinbexp[1] <- zinbexp[1] + mean(cphistim2)

ab <- cbind(oss, pois.exp, nb.exp, zip.exp, zinbexp)
ab
chiquadrozinb <- n*((sum((ab[,1]-ab[,5])^2/ab[,5])) + (tot[1] - tot[5])^2/(1 - tot[5]))
chiquadrozinb

#hurdle, modello troncato

mod_hurdle <- hurdle(art ~ ., data = Biochemists, dist = "poisson", zero.dist = "binomial")
summary(mod_hurdle)

mod_hurdle2 <- hurdle(art ~ fem + kid5 + ment | kid5 + ment + mar, data = Biochemists, dist = "p", zero.dist = "b")
summary(mod_hurdle2)
?hurdle

hurdle.count <- sapply(1:8, function(x) sum(predict(mod_hurdle2, type = "p")[,x]))

hurdle.count
table(Biochemists$art)


hurdle.exp <- hurdle.count/sum(hurdle.count)
ab <- cbind(oss, pois.exp, nb.exp, zip.exp, zinbexp, hurdle.count)
ab
chiquadroh <- ((sum((n*ab[,1]-ab[,6])^2/ab[,6])))
chiquadroh

