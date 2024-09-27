drug.table <- ftable(xtabs(count ~ a + c + m, data = drug))
drug.table
View(drug.table)
drug$a <- factor(drug$a)
drug$c <- factor(drug$c)
drug$m <- factor(drug$m)

drug1 <- glm(count ~ a * c * m, family = poisson, data = drug)
summary(drug1)

drop1(drug1, test = "Chisq")

drug2 <- update(drug1, . ~ . - a:c:m)
summary(drug2)

drop1(drug2, test = "Chisq")

drug3 <- glm(count ~ a + c + m, family = poisson, data = drug)
summary(drug3)

#prop di chi beve nel gruppo di chi fuma è exp(...) rispetto a chi beve e non fuma

drug.fit<-ftable(xtabs(drug2$fitted~a+c+m,data=drug))

drug.fit

