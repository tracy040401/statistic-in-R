#################################################################
############  TP NOTE INFO 3A STATISTIQUES 2022-2023 ############
#################################################################

###### EXERCICE 1 : EVOLUTION DU TAUX DE MORTALITE INFANTILE EN FRANCE DE 1950 à 2016 ######

# Question 1 : Lecture du fichier

setwd("C:/Users/andre/OneDrive/Documents/S6/Stats/TP_note")
mortalite <- read.csv2("MortaliteInfantile.csv", header = T)

attach(mortalite)
head(mortalite)


# Question 2 : Représentation graphique du taux de mortalité infantile en fonction de l'année

plot(mortalite, pch = 19, xlim = c(1950, 2023))


# Question 3a : Ajustement d'un modèle de régression linéaire simple

mod <- lm(Taux~Annee, data = mortalite) 

summary(mod)
# la p-value étant quasiment nulle, on peut affirmer que l'année a un impact sur le taux de mortalité, au risque de 5%

# Question 3b : Tracé de la droite de régression

abline(mod, col="red")

# Question 3c : Prédiction du taux de mortalité infantile de 2017 à 2023

pred <- predict(mod, newdata = data.frame(Annee = seq(2017, 2023, 1)),interval = "prediction")
pred[,1]
                   

# Question 3d : Graphe des résidus

resume <- summary(mod)
plot(x = mod$fitted.values,y = mod$residuals, xlab="^y",ylab="Residuals", pch=19, col="orange", title("Graphe de résidus"))
abline(h=0)
abline(h=-2*resume$sigma,lty=2, col = "red")
abline(h=2*resume$sigma, lty=2, col = "blue")

# plot(mod) ?? marche pas ?? 
# les hypothèses ne sont pas vérifiées : les erreurs ne sont pas réparties de façon indépendantes, ni de variance constante (homosédasdicité), et ne semblent pas suivrent une loi normale


# Question 4a : Nouveau modèle de régression sur le taux de mortalité infantile

mortalite$inv_annee <- 1/Annee
mortalite$inv_taux <- 1/Taux


#plot(Taux, inv_annee)
mod2 <- lm(inv_taux~inv_annee, data = mortalite)
summary(mod2)

# Question 4b : Tracé de la droite ou de la courbe d'ajustement du second modèle


plot(y = mortalite$Taux, x = mortalite$Annee, pch = 19, xlim = c(1950, 2023))
abline(mod, col="red")
pred2 <- predict(mod2)
pred2 <- 1/pred2
lines(Annee, pred2, col = "blue")

# Question 4c : Prédiction du taux de mortalité infantile de 2017 à 2023


seq(1/2017, 1/2023)


pred2 <- predict(mod2, newdata = data.frame(inv_annee = c(1/2017, 1/2018, 1/2019, 1/2020, 1/2021, 1/2022, 1/2023)),interval = "prediction")
pred2[,1]

# Question 5a : Construction de la matrice X pour le modèle linéaire par morceaux

X <- matrix(nrow = 67, ncol = 3)
X[,1] <- rep(1,67)

X[,2] <- Annee
X[,3] <- 1980 - Annee
X

# Question 5b : Estimation des paramètres du modèle linéaire

Y <- cbind(Taux)
beta <- solve(t(X)%*%X)%*%t(X)%*%Y
beta

# Question 5c : Tracé de la courbe d'ajustement

# Question 5d : Calcul du coefficient de détermination

# Question 6a : Ajustement du modèle exponentiel


mod3 <- lm(log(Taux)~log(Annee), data = mortalite)

# Question 6b : Tracé de la courbe d'ajustement du modèle exponentiel

plot(y = mortalite$Taux, x = mortalite$Annee, pch = 19, xlim = c(1950, 2023))
abline(mod, col="red")
pred2 <- predict(mod2)
pred2 <- 1/pred2
lines(Annee, pred2, col = "blue")

pred3 <- predict(mod3)
pred3 <- exp(pred3)
lines(Annee, pred3, col = "green")

# Question 6c : Prédiction du taux de mortalité infantile de 2017 à 2023

pred3 <- predict(mod3, newdata = data.frame(Annee = seq(2017, 2023, 1)),interval = "prediction")
exp(pred3)




# Question 6d : Intervalle de confiance à 95% sur la prédiction
IC <- exp(predict(mod3, newdata = data.frame(Annee = Annee),interval = "prediction"))

lines(Annee, IC[,2], lty = 2, col = "green")
lines(Annee, IC[,3], lty = 2, col = "green")

########## EXERCICE 2 : TORREFACTION DU CAFE VERT ##########

# Question 1 : Lecture des données
setwd("C:/Users/andre/OneDrive/Documents/S6/Stats/TP_note")
cafe <- read.table("cafe.txt", header = T)

attach(cafe)
head(cafe)

# Question 2a: Estimation des paramètres du modèle d'ANOVA
origine <- as.factor(origine)

mod <- lm(perte~origine)

summary(mod)

# Question 2b : Effet de l'origine du café?

anova(mod)
# non l'orgine du café n'a pas d'effet sur la perte en eau puisque que la p-value est de 79,6% >> 5%


# Question 3a: Estimation des paramètres de la régression multiple

modele1 <- lm(perte~lumin+xa+xb+xgn)
summary(modele1)

# Question 3b : Effets des variables

# seul xb a un effet significatif sur les pertes en eau (p-value<<5%)

# Question 3c : Test sur un coefficient

# TODO

# Question 4 : Corrélation entre les variables explicatives

cor(cafe[,-1])

# xb - lumin
# lumin - xy
# lumin - xgn

# Question 5a : Sélection de variables

modele2 <- step(modele1, direction = "backward")

# perte ~ lumin + xa + xb

# Question 5b: Effets des variable dans le modèle 2
summary(modele2)

# toutes les variables présentes : lumin, xa, xb 
# il doit choisir ces cafés en choississant ceux qui ont un très faible xa, puis un faible lumin et enfin un faible xb
# dans l'ordre d'importance 

# Question 6a : Graphiques
cafe$Tlumin <- 1/lumin
cafe$Txy <- 1/xy
cafe$Txgn <- 1/xgn

attach(cafe)

plot(cafe)


# on voit que la relation est linéaire avec les 5 variables de son modèle et perte
# et que c'est inversement proportionnel avec les variables lumin, xy et xgn 


# Question 6b: Création des nouvelles variables dans le tableau de données

# déjà fait 

# Question 6c : Ajustement du modèle de régression multiple 

modele3 <- lm(perte~xa+xb+lumin+xy+Txy+Txgn+Tlumin)

# Question 6d : Sélection de variables

modele4<-step(modele3, direction = "backward")
# perte ~ xa + xb + lumin + Txy + Txgn + Tlumin

# Question 7: Qualité de l'ajustement

AIC1 <- extractAIC(modele1)
AIC1
summary(modele1)$adj.r.squared

AIC2 <- extractAIC(modele2)
AIC2
summary(modele2)$adj.r.squared

AIC3 <- extractAIC(modele3)
AIC3
summary(modele3)$adj.r.squared

AIC4 <- extractAIC(modele4)
AIC4
summary(modele4)$adj.r.squared

# meilleur AIC : modele 4 (plus faible)
# meilleur R² : modele 4 (plus proche de 1)

# Question 8 : Qualité prédictive du modèle
press=function(model)
{
  h=influence(model)$hat
  e=influence(model)$wt.res
  n=length(e)
  sum((e/(1-h))^2)/n
}

press(model = modele1)
press(model = modele2)
press(model = modele3)
press(model = modele4)

# meilleur modèle 3 et 4 


# Question 9 : Nouveau modèle ANCOVA

modele5 <- lm(perte~origine+Tlumin, data=cafe)

extractAIC(modele5)
summary(modele5)$adj.r.squared



# meilleur AIC et meilleur R² donc ElRingo devrait choisir le modèle 5 

detach(cafe)

###### EXERCICE 3 : DEGUSTATION DE COMPOTES ######

# Question 1 : Installation et chargement de packages
library(emmeans)

# Question 2 : Lecture du fichier

notes<-read.csv2("compote.csv")

attach(notes)
head(notes)

# Question 3 : Transformation de variables en facteurs

notes$juge <- as.factor(notes$juge)
compote <- as.factor(compote)

# Question 4 : Représentation de la distribution de la variable saveur de pomme crue en fonction de la compote
pommes_by_comp <- by(S.pom.crue, compote, summary)
boxplot(pommes_by_comp, main = "Boîte à moustache - Compotes", ylab = "Valeurs", xlab= "Types de Compotes")
abline(h=mean(S.pom.crue), col="red", lwd=3)

# on peut voir que delisse semble avoir un score plus élevé que les autres 
# mais également que poti scoup et st mamet sont un peu en dessous de la moyennes
# on ne peut rien déduire car ils ont tous une très grande variance



# Question 5 : Représentation de la distribution de la variable saveur de pomme crue en fonction du juge

pom_by_juge <- by(S.pom.crue, notes$juge, summary)
boxplot(pom_by_juge, main = "Boîte à moustache - Juges", ylab = "Valeurs", xlab= "Types de Compotes")
abline(h=mean(S.pom.crue), col="red", lwd=3)



# ici on peut voir que les juges ont l'air d'avoir de l'importance sur les notes
# les juges 4 et 6 ont l'air de noté dur
# à l'inverse, les juges 1 et 2 ont l'air de noté gentillement
# les variances sont cependant grande, donc on ne peut pas apporté de conclusion (sauf pour le 4)

# Question 6a: ANOVA à un facteur

mod <- lm(S.pom.crue~compote, data = notes)
resume <- summary(mod)
for (i in 1:6){
  print(resume$coefficients[i])
}

# Question 6b : Effet du facteur

anova(mod)

# toutes la p-value est strictement supérieure à 5% donc on peut considéré au risque de 5%, qu'elles ont la même saveur de pommes crues


# Question 7a : ANOVA à deux facteurs


mod2 <- lm(S.pom.crue~compote+juge, data= notes)
summary(mod2)
resume2 <- summary(mod2)
for (i in 1:11){
  print(resume2$coefficients[i])
}

# Question 7b : Effets des variables sur la note de perception de la saveur de pomme crue

# les variables suivantes ont un effet significatif au risque de 5% : compotesdelisse, juge3, juge4, juge6

# Question 7c : Test sur un coefficient

# non il n'est pas significativement différent de 0 puisque il a une p-value de 39% >> 5%, au risque de 5%


# Question 7d : Comparaison de moyennes

emmeans(mod2, pairwise~juge, adjust="bonferroni")

# au risque de 10%, les notes des juges 4 et 5 ne sont pas significativement différentes puisque p-value = 0.0050

emmeans(mod2, pairwise~compote, adjust="bonferroni")

# Andros et Delisse : oui, elles sont significativement différentes au risque de 10%, car p-value = 1 >>0.1

# je lui conseillerais la compote poti car c'est elles qui ont les notes les moins différentes
# la p-value du test est la plus basse avec 4%  de différence
detach(notes)


