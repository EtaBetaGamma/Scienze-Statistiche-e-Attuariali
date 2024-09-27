model1 <- polr (factor (apply) ~., data = dati.logit)
brant(model1)

#comulative
fit.prop <- vglm(apply ~ ., data = dati.logit, family = cumulative(parallel = F), maxit = 50)
summary(fit.prop)


dati.logit$apply <- factor (dati.logit$apply, levels = c("unlikely", "somewhat", "likely", "very likely"), ordered = TRUE)
summary(dati.logit)

fit.prop1 <- vglm(apply ~ pared + public, data = dati.logit, family = cumulative(parallel = F, reverse = T), maxit = 50)
summary(fit.prop1)


head(dati.logit)

####Adjacent
fit.adj <- vglm(apply ~., data = dati.logit, family = acat(parallel = F, reverse = T), maxit = 50)
summary (fit.adj)

###continuative ratio
fit.cont <- vglm(apply ~., data = dati.logit, family = sratio(parallel = F, reverse = T), maxit = 50)
summary(fit.cont)

lrtest(fit.prop, fit.cont)
