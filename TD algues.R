load("donnee.RData")
head(algue)
summary(algue)
#freq : variable a expliquer qui correspond a la frequence de presence de l'algue
#on attache pour eviter d'ecrire algue$nomvariable
attach(algue)

#question 1: qqplot pour comparer les quantiles mais on commence avec un histogramme
hist(mxPH)
#ressemble bien à la répartition d'une fonction normale on peut aussi utiliser qqnorm
#test de normalité, on fait un komogorov smirnov plus général que chappiro
ks.test(mxPH,"pnorm",mean(mxPH),sd(mxPH))
#on accepte Ho, c'est bien normal
qqnorm(mxPH)
qqline(mxPH)
#si les points sont répartis sur la ligne,il y a une repartion normale
shapiro.test(mxPH)
#avec la p value assez grande, on accepte Ho
shapiro.test(Chla)
#elle ne suit pas une loi normale
 
#on test la moyenne d'une normale =8
t.test(mxPH,mu=8.07)
#la p value est trop petite, on rejette la moyenne, mais avec l'intervalle de confiance on peut prendre une estimation
#il donne directement l'intervalle de confiance qu'on peut calculer nous meme comme ceci
n=length(mxPH)
s=sd(mxPH)
E=qt(0.995,df=n-1)*s/sqrt(n)
#d'ou l'intervalle 
mean(mxPH)+c(-E,E)

#question 2
cor(algue[4:12])
#on regarde la colonne de freq, on choissit  donc aPO4 et PO4
plot(PO4,oPO4)
abline(lm(PO4~oPO4))
#ils ont l'air assez corrélés malheuresement, on vérifie par un modèle linéaire
summary(lm(oPO4~PO4))
#le r2 vaut 0.83, les variables sont trop corrélées pour prendre les deux en tant que variable explicative

plot(NH4,NO3)
summary(lm(NH4~NO3))
cor(NH4,NO3)
cor(oPO4,PO4)
#on trouve pas la meme chose entre r2 et la corrélation parce qu'il faut enlever l'intercept

chisq.test(table(size,speed))
#on rejette Ho, elles sont donc dépendantes ou corrélées

#question 3
model<-lm(freq~.,algue)
summary(model)
#modelfinal<-step(model)
#summary(modelfinal)
modelbis<-lm(freq~season+NH4+PO4)
summary(modelbis)
#3.6
model2<-step(model,direction="both") 
#il peut enlever une variable mais la remettre après si elle redevient éloquente
summary(model2)
#maintenant on regarde ce qu'il a enlevé
model2$anova
boxplot(freq~season)
#on peut tester la valeur des moyennes
anova(lm(freq~season))
#les valeurs sont donc les mêmes :)
anova(lm(freq~size))

interaction.plot(size,speed,freq)
#si la vitesse est élevée, la freq est assez faible, plus eleve si la vitesse baisse
