L<-c(551, 651, 832, 375, 715, 868, 271, 630, 491, 372, 645, 441, 895, 458, 642, 
     492, 543, 842, 905, 542, 522, 122, 657, 170, 738, 371, 735, 749, 495, 716,
     952, 417)

D<-c(8, 4, 17, 9, 14,  8, 5, 7, 7, 7, 6, 8, 28, 4, 10, 4, 8, 9, 23, 9, 6, 1, 9,
     4, 9, 14, 17, 10, 7, 3, 9, 2)

modP <- glm(D ~ L, family = poisson)

summary(modP)

previsione <- predict (modP, newdata = data.frame("L" = 50))
previsione
?predict
previsione * exp(modP$coefficients)
