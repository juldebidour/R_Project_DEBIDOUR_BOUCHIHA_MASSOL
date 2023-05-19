#-------------------------------------------------------------------------------------------------------------------------------------------------------


# Projet R 2020 World Hapiness


#-------------------------------------------------------------------------------------------------------------------------------------------------------

donnees <- read.csv2("/Users/debidour/Desktop/ENSC/PERSONNEL/1A/MATHS/Projet R/Data.csv", header = TRUE, dec = ".", sep = ",")

#-------------------------------------------------------------------------------------------------------------------------------------------------------


# INTRODUCTION SUR LA LISTE DES DONNÉES


#-------------------------------------------------------------------------------------------------------------------------------------------------------


summary(donnees)
dimnames(donnees) # affiche les noms des lignes et des colonnes (sous forme de liste)
rownames(donnees) # affiche les noms des lignes (numéro des pays)
colnames(donnees) # affiche les noms des colonnes (variables étudiées içi)
apply(donnees[, -1], 2, mean) # donnees[,-1] permet de supprimer la 1ere colonne (içi country) qui est une variable qualitative (categorielle)
round(apply(donnees[, -1], 2, mean), digits = 1) # on arrondit a un chiffre apres la virgule

#----------------------------------------
# Données utiles à notre étude
#----------------------------------------

joie <- donnees[, 3] # Ladder.score (ou niveau de joie)
pays <- donnees[, 1] # liste des pays
region <- donnees[, 2] # régionalindicator
pib <- donnees[, 7] # Logged GDP per capita
social <- donnees[, 8] # Social support
sante <- donnees[, 9] # Healthy life expectancy
liberte <- donnees[, 10] # Freedom to make life choices
generosite <- donnees[, 11] # Generosity
corrupt <- donnees[, 12] # Perceptions.of.corruption

#----------------------------------------
# Données moins intéressantes dans un premier temps
#----------------------------------------

donnees[, 4] # Standard.error.of.ladder.score
donnees[, 5] # upperwhisker
donnees[, 6] # lowerwhisker
donnees[, 13] # Ladder score in Dystopia
donnees[, 14] # Explained by: Log GDP per capita
donnees[, 15] # Explained by: Social support
donnees[, 16] # Explained by: Healthy life expectancy
donnees[, 17] # Explained by: Freedom to make life choices
donnees[, 18] # Explained by: Generosity
donnees[, 19] # Explained by: Perceptions of corruption
donnees[, 20] # Dystopia + residual

#-------------------------------------------------------------------------------------------------------------------------------------------------------


##### STATISTIQUE DESCRIPTIVE #####


#-------------------------------------------------------------------------------------------------------------------------------------------------------


#----------------------------------------
# étude du Niveau de joie ou bonheur
#----------------------------------------

summary(joie)
max(joie)
min(joie)
range(joie)
mean(joie)
variance(joie)
median(joie)
# étude du niveau de joie selon les régions du monde
mean(joie[which(region == "Western Europe")])
mean(joie[-which(region == "Western Europe")])

mean(joie[which(region == "North America and ANZ")])
mean(joie[-which(region == "North America and ANZ")])

mean(joie[which(region == "Western Europe" | region == "North America and ANZ")])
mean(joie[-which(region == "Western Europe" | region == "North America and ANZ")])

mean(joie[which(region == "Middle East and North Africa ")])
mean(joie[-which(region == "Middle East and North Africa ")])

mean(joie[which(region == "Sub-Saharan Africa")])
mean(joie[-which(region == "Sub-Saharan Africa")])

mean(joie[which(region == "Central and Eastern Europe")])
mean(joie[-which(region == "Central and Eastern Europe")])

mean(joie[which(region == "Latin America and Caribbean ")])
mean(joie[-which(region == "Latin America and Caribbean ")])

mean(joie[which(region == "East Asia")])
mean(joie[-which(region == "East Asia")])

mean(joie[which(region == "Southeast Asia")])
mean(joie[-which(region == "Southeast Asia")])

mean(joie[which(region == "South Asia")])
mean(joie[-which(region == "South Asia")])

mean(joie[which(region == "Commonwealth of Independent States")])
mean(joie[-which(region == "Commonwealth of Independent States")])

quantile(joie, probs = seq(from = 0.1, to = 0.9, by = 0.1))
par(mfrow = c(2, 1))
hist(joie)
par(mfrow = c(1, 1))
boxplot(joie)
head(joie) # affiche les 6 premieres lignes du jeu de donnees joie
dim(joie) # donne le nombre de lignes et de colonnes du jeu de donnees
joie # affiche l'integralite du jeu de donnees


#----------------------------------------
# Listes des divers pays
#----------------------------------------



#----------------------------------------
# Liste des régions du monde
#----------------------------------------


table(region)


#----------------------------------------
# GDP
#----------------------------------------

summary(pib)
max(pib)
min(pib)
range(pib)
mean(pib)
variance(pib)
median(pib)
summary(pib)
quantile(pib, probs = seq(from = 0.1, to = 0.9, by = 0.1))
par(mfrow = c(2, 1))
hist(pib)
par(mfrow = c(1, 1))
boxplot(pib)
head(pib) # affiche les 6 premieres lignes du jeu de donnees "temp"
dim(pib) # donne le nombre de lignes et de colonnes du jeu de donnees
pib # affiche l'integralite du jeu de donnees
dimnames(pib) # affiche les noms des lignes et des colonnes (sous forme de liste)
rownames(pib) # affiche les noms des lignes (individus)
colnames(pib) # affiche les noms des colonnes (variables)
apply(pib[, -1], 2, mean) # temp[,-1] permet de supprimer la 1ere colonne (=ville) qui est une variable qualitative (categorielle)
round(apply(pib[, -1], 2, mean), digits = 1) # on arrondit a un chiffre apres la virgule

#----------------------------------------
# Soutien Social
#----------------------------------------

summary(social)
max(social)
min(social)
range(social)
mean(social)
variance(social)
median(social)
summary(social)
quantile(social, probs = seq(from = 0.1, to = 0.9, by = 0.1))
par(mfrow = c(2, 1))
hist(social)
par(mfrow = c(1, 1))
boxplot(social)
head(social) # affiche les 6 premieres lignes du jeu de donnees "temp"
dim(social) # donne le nombre de lignes et de colonnes du jeu de donnees
social # affiche l'integralite du jeu de donnees
dimnames(social) # affiche les noms des lignes et des colonnes (sous forme de liste)
rownames(social) # affiche les noms des lignes (individus)
colnames(social) # affiche les noms des colonnes (variables)
apply(social[, -1], 2, mean) # temp[,-1] permet de supprimer la 1ere colonne (=ville) qui est une variable qualitative (categorielle)
round(apply(social[, -1], 2, mean), digits = 1) # on arrondit a un chiffre apres la virgule

#----------------------------------------
# Espérance de vie en bonne santé
#----------------------------------------

max(sante)
max(sante)
range(sante)
mean(sante)
variance(sante)
median(sante)
summary(sante)
quantile(sante, probs = seq(from = 0.1, to = 0.9, by = 0.1))
par(mfrow = c(2, 1))
hist(sante)
par(mfrow = c(1, 1))
boxplot(sante)
head(sante) # affiche les 6 premieres lignes du jeu de donnees "temp"
dim(sante) # donne le nombre de lignes et de colonnes du jeu de donnees
sante # affiche l'integralite du jeu de donnees
dimnames(sante) # affiche les noms des lignes et des colonnes (sous forme de liste)
rownames(sante) # affiche les noms des lignes (individus)
colnames(sante) # affiche les noms des colonnes (variables)
apply(sante[, -1], 2, mean) # temp[,-1] permet de supprimer la 1ere colonne (=ville) qui est une variable qualitative (categorielle)
round(apply(sante[, -1], 2, mean), digits = 1) # on arrondit a un chiffre apres la virgule

#----------------------------------------
# Liberté de faire ses propres choix de vie
#----------------------------------------

max(liberte)
max(liberte)
range(liberte)
median(liberte)
mean(liberte)
variance(liberte)
summary(liberte)
quantile(liberte, probs = seq(from = 0.1, to = 0.9, by = 0.1))
par(mfrow = c(2, 1))
hist(liberte)
par(mfrow = c(1, 1))
boxplot(liberte)
head(liberte) # affiche les 6 premieres lignes du jeu de donnees "temp"
dim(liberte) # donne le nombre de lignes et de colonnes du jeu de donnees
liberte # affiche l'integralite du jeu de donnees
dimnames(liberte) # affiche les noms des lignes et des colonnes (sous forme de liste)
rownames(liberte) # affiche les noms des lignes (individus)
colnames(liberte) # affiche les noms des colonnes (variables)
apply(liberte[, -1], 2, mean) # temp[,-1] permet de supprimer la 1ere colonne (=ville) qui est une variable qualitative (categorielle)
round(apply(liberte[, -1], 2, mean), digits = 1) # on arrondit a un chiffre apres la virgule

#----------------------------------------
# Générosité
#----------------------------------------

max(generosite)
max(generosite)
range(generosite)
median(generosite)
summary(generosite)
quantile(generosite, probs = seq(from = 0.1, to = 0.9, by = 0.1))
par(mfrow = c(2, 1))
hist(generosite)
par(mfrow = c(1, 1))
boxplot(generosite)
head(generosite) # affiche les 6 premieres lignes du jeu de donnees "temp"
dim(generosite) # donne le nombre de lignes et de colonnes du jeu de donnees
generosite # affiche l'integralite du jeu de donnees
dimnames(generosite) # affiche les noms des lignes et des colonnes (sous forme de liste)
rownames(generosite) # affiche les noms des lignes (individus)
colnames(generosite) # affiche les noms des colonnes (variables)
apply(generosite[, -1], 2, mean) # temp[,-1] permet de supprimer la 1ere colonne (=ville) qui est une variable qualitative (categorielle)
round(apply(generosite[, -1], 2, mean), digits = 1) # on arrondit a un chiffre apres la virgule

#----------------------------------------
# Niveau de corruption perçue
#----------------------------------------

max(corrupt)
max(corrupt)
range(corrupt)
median(corrupt)
summary(corrupt)
quantile(corrupt, probs = seq(from = 0.1, to = 0.9, by = 0.1))
par(mfrow = c(2, 1))
hist(corrupt)
par(mfrow = c(1, 1))
boxplot(corrupt)
head(corrupt) # affiche les 6 premieres lignes du jeu de donnees "temp"
dim(corrupt) # donne le nombre de lignes et de colonnes du jeu de donnees
corrupt # affiche l'integralite du jeu de donnees
dimnames(corrupt) # affiche les noms des lignes et des colonnes (sous forme de liste)
rownames(corrupt) # affiche les noms des lignes (individus)
colnames(corrupt) # affiche les noms des colonnes (variables)
apply(corrupt[, -1], 2, mean) # temp[,-1] permet de supprimer la 1ere colonne (=ville) qui est une variable qualitative (categorielle)
round(apply(corrupt[, -1], 2, mean), digits = 1) # on arrondit a un chiffre apres la virgule


#-------------------------------------------------------------------------------------------------------------------------------------------------------


##### STATISTIQUE INFERENTIELLE #####


#-------------------------------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------
### LECTURE DES DONNÉES ###
#----------------------------------------

donnees <- read.csv2("/Users/debidour/Desktop/ENSC/PERSONNEL/1A/MATHS/Projet R/Data.csv", header = TRUE, dec = ".", sep = ",")

# Créer un dataframe avec les données
donneesinferentielle <- data.frame(summary(donnees[, c("Ladder.score", "Logged.GDP.per.capita", "Social.support", "Healthy.life.expectancy", "Freedom.to.make.life.choices", "Generosity", "Perceptions.of.corruption")]))

# Résumé des variables numériques
summary(donneesinferentielle[, c("Ladder.score", "Logged.GDP.per.capita", "Social.support", "Healthy.life.expectancy", "Freedom.to.make.life.choices", "Generosity", "Perceptions.of.corruption")])

# Test d'hypothèse : Comparaison de la moyenne de "Ladder.score" à une valeur spécifique
t.test(donneesinferentielle$Ladder.score, mu = 5)


#----------------------------------------
### TESTS DE CORRÉLATION ####
#----------------------------------------


## TEST POUR SOCIAL SUPPORT ##


# Test de corrélation entre "Social.support" et "Ladder.score"
cor.test(donneesinferentielle$Social.support, donneesinferentielle$Ladder.score)

# Test de corrélation entre "Social.support" et "Logged.GDP.per.capita"
cor.test(donneesinferentielle$Social.support, donneesinferentielle$Logged.GDP.per.capita)

# Test de corrélation entre "Social.support" et "Healthy.life.expectancy"
cor.test(donneesinferentielle$Social.support, donneesinferentielle$Healthy.life.expectancy)

# Test de corrélation entre "Social.support" et "Freedom.to.make.life.choices"
cor.test(donneesinferentielle$Social.support, donneesinferentielle$Freedom.to.make.life.choices)

# Test de corrélation entre "Social.support" et "Generosity"
cor.test(donneesinferentielle$Social.support, donneesinferentielle$Generosity)

# Test de corrélation entre "Social.support" et "Perceptions.of.corruption"
cor.test(donneesinferentielle$Social.support, donneesinferentielle$Perceptions.of.corruption)


## TEST POUR LADDER SCORE ##


# Test de corrélation entre "Ladder.score" et "Social.support"
cor.test(donneesinferentielle$Ladder.score, donneesinferentielle$Social.support)

# Test de corrélation entre "Ladder.score" et "Healthy.life.expectancy"
cor.test(donneesinferentielle$Ladder.score, donneesinferentielle$Healthy.life.expectancy)

# Test de corrélation entre "Ladder.score" et "Logged.GDP.per.capita"
cor.test(donneesinferentielle$Ladder.score, donneesinferentielle$Logged.GDP.per.capita)

# Test de corrélation entre "Ladder.score" et "Freedom.to.make.life.choices"
cor.test(donneesinferentielle$Ladder.score, donneesinferentielle$Freedom.to.make.life.choices)

# Test de corrélation entre "Ladder.score" et "Generosity"
cor.test(donneesinferentielle$Ladder.score, donneesinferentielle$Generosity)

# Test de corrélation entre "Ladder.score" et "Perceptions.of.corruption"
cor.test(donneesinferentielle$Ladder.score, donneesinferentielle$Perceptions.of.corruption)


## TEST POUR HEALTHY LIFE EXPECTANCY ##


# Test de corrélation entre "Healthy.life.expectancy" et "Ladder.score"
cor.test(donneesinferentielle$Healthy.life.expectancy, donneesinferentielle$Ladder.score)

# Test de corrélation entre "Healthy.life.expectancy" et "Social.support"
cor.test(donneesinferentielle$Healthy.life.expectancy, donneesinferentielle$Social.support)

# Test de corrélation entre "Healthy.life.expectancy" et "Logged.GDP.per.capita"
cor.test(donneesinferentielle$Healthy.life.expectancy, donneesinferentielle$Logged.GDP.per.capita)

# Test de corrélation entre "Healthy.life.expectancy" et "Freedom.to.make.life.choices"
cor.test(donneesinferentielle$Healthy.life.expectancy, donneesinferentielle$Freedom.to.make.life.choices)

# Test de corrélation entre "Healthy.life.expectancy" et "Generosity"
cor.test(donneesinferentielle$Healthy.life.expectancy, donneesinferentielle$Generosity)

# Test de corrélation entre "Healthy.life.expectancy" et "Perceptions.of.corruption"
cor.test(donneesinferentielle$Healthy.life.expectancy, donneesinferentielle$Perceptions.of.corruption)


## TEST POUR LOGGED GDP PER CAPITA ##


# Test de corrélation entre "Logged.GDP.per.capita" et "Ladder.score"
cor.test(donneesinferentielle$Logged.GDP.per.capita, donneesinferentielle$Ladder.score)

# Test de corrélation entre "Logged.GDP.per.capita" et "Healthy.life.expectancy"
cor.test(donneesinferentielle$Logged.GDP.per.capita, donneesinferentielle$Healthy.life.expectancy)

# Test de corrélation entre "Logged.GDP.per.capita" et "Social.support"
cor.test(donneesinferentielle$Logged.GDP.per.capita, donneesinferentielle$Social.support)

# Test de corrélation entre "Logged.GDP.per.capita" et "Freedom.to.make.life.choices"
cor.test(donneesinferentielle$Logged.GDP.per.capita, donneesinferentielle$Freedom.to.make.life.choices)

# Test de corrélation entre "Logged.GDP.per.capita" et "Generosity"
cor.test(donneesinferentielle$Logged.GDP.per.capita, donneesinferentielle$Generosity)

# Test de corrélation entre "Logged.GDP.per.capita" et "Perceptions.of.corruption"
cor.test(donneesinferentielle$Logged.GDP.per.capita, donneesinferentielle$Perceptions.of.corruption)


## TEST POUR FREEDOM TO MAKE LIFE CHOICES ##


# Test de corrélation entre "Freedom.to.make.life.choices" et "Ladder.score"
cor.test(donneesinferentielle$Freedom.to.make.life.choices, donneesinferentielle$Ladder.score)

# Test de corrélation entre "Freedom.to.make.life.choices" et "Social.support"
cor.test(donneesinferentielle$Freedom.to.make.life.choices, donneesinferentielle$Social.support)

# Test de corrélation entre "Freedom.to.make.life.choices" et "Healthy.life.expectancy"
cor.test(donneesinferentielle$Freedom.to.make.life.choices, donneesinferentielle$Healthy.life.expectancy)

# Test de corrélation entre "Freedom.to.make.life.choices" et "Logged.GDP.per.capita"
cor.test(donneesinferentielle$Freedom.to.make.life.choices, donneesinferentielle$Logged.GDP.per.capita)

# Test de corrélation entre "Freedom.to.make.life.choices" et "Generosity"
cor.test(donneesinferentielle$Freedom.to.make.life.choices, donneesinferentielle$Generosity)

# Test de corrélation entre "Freedom.to.make.life.choices" et "Perceptions.of.corruption"
cor.test(donneesinferentielle$Freedom.to.make.life.choices, donneesinferentielle$Perceptions.of.corruption)


## TEST POUR GENEROSITY ##


# Test de corrélation entre "Generosity" et "Ladder.score"
cor.test(donneesinferentielle$Generosity, donneesinferentielle$Ladder.score)

# Test de corrélation entre "Generosity" et "Social.support"
cor.test(donneesinferentielle$Generosity, donneesinferentielle$Social.support)

# Test de corrélation entre "Generosity" et "Healthy.life.expectancy"
cor.test(donneesinferentielle$Generosity, donneesinferentielle$Healthy.life.expectancy)

# Test de corrélation entre "Generosity" et "Logged.GDP.per.capita"
cor.test(donneesinferentielle$Generosity, donneesinferentielle$Logged.GDP.per.capita)

# Test de corrélation entre "Generosity" et "Freedom.to.make.life.choices"
cor.test(donneesinferentielle$Generosity, donneesinferentielle$Freedom.to.make.life.choices)

# Test de corrélation entre "Generosity" et "Perceptions.of.corruption"
cor.test(donneesinferentielle$Generosity, donneesinferentielle$Perceptions.of.corruption)


## TEST POUR PERCEPTION OF CORRUPTION ##


# Test de corrélation entre "Perceptions.of.corruption" et "Ladder.score"
cor.test(donneesinferentielle$Perceptions.of.corruption, donneesinferentielle$Ladder.score)

# Test de corrélation entre "Perceptions.of.corruption" et "Social.support"
cor.test(donneesinferentielle$Perceptions.of.corruption, donneesinferentielle$Social.support)

# Test de corrélation entre "Perceptions.of.corruption" et "Healthy.life.expectancy"
cor.test(donneesinferentielle$Perceptions.of.corruption, donneesinferentielle$Healthy.life.expectancy)

# Test de corrélation entre "Perceptions.of.corruption" et "Logged.GDP.per.capita"
cor.test(donneesinferentielle$Perceptions.of.corruption, donneesinferentielle$Logged.GDP.per.capita)

# Test de corrélation entre "Perceptions.of.corruption" et "Freedom.to.make.life.choices"
cor.test(donneesinferentielle$Perceptions.of.corruption, donneesinferentielle$Freedom.to.make.life.choices)

# Test de corrélation entre "Perceptions.of.corruption" et "Generosity"
cor.test(donneesinferentielle$Perceptions.of.corruption, donneesinferentielle$Generosity)


#----------------------------------------
#### RÉGRÉSSION LINÉAIRE ####
#----------------------------------------


## 1/ RÉGRÉSSION LINÉAIRE POUR LADDER SCORE ##


# Régression linéaire pour prédire "Ladder.score" à partir de "Social.support"
lm_model <- lm(Ladder.score ~ Social.support, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Ladder.score" à partir de "Healthy.life.expectancy"
lm_model <- lm(Ladder.score ~ Healthy.life.expectancy, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Ladder.score" à partir de "Logged.GDP.per.capita"
lm_model <- lm(Ladder.score ~ Logged.GDP.per.capita, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Ladder.score" à partir de "Freedom.to.make.life.choices"
lm_model <- lm(Ladder.score ~ Freedom.to.make.life.choices, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Ladder.score" à partir de "Generosity"
lm_model <- lm(Ladder.score ~ Generosity, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Ladder.score" à partir de "Perceptions.of.corruption"
lm_model <- lm(Ladder.score ~ Perceptions.of.corruption, data = donnees)
summary(lm_model)


## 2/ RÉGRÉSSION LINÉAIRE POUR SOCIAL SUPPORT ##


# Régression linéaire pour prédire "Social.support" à partir de "Ladder.score"
lm_model <- lm(Social.support ~ Ladder.score, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Social.support" à partir de "Logged.GDP.per.capita"
lm_model <- lm(Social.support ~ Logged.GDP.per.capita, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Social.support" à partir de "Freedom.to.make.life.choices"
lm_model <- lm(Social.support ~ Freedom.to.make.life.choices, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Social.support" à partir de "Generosity"
lm_model <- lm(Social.support ~ Generosity, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Social.support" à partir de "Perceptions.of.corruption"
lm_model <- lm(Social.support ~ Perceptions.of.corruption, data = donnees)
summary(lm_model)


# Régression linéaire pour prédire "Social.support" à partir de "Healthy.life.expectancy"
lm_model <- lm(Social.support ~ Healthy.life.expectancy, data = donnees)
summary(lm_model)


## 3/ RÉGRÉSSION LINÉAIRE POUR HEALTHY LIFE EXPECTANCY ##


# Régression linéaire pour prédire "Healthy.life.expectancy" à partir de "Ladder.score"
lm_model <- lm(Healthy.life.expectancy ~ Ladder.score, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Healthy.life.expectancy" à partir de "Social.support"
lm_model <- lm(Healthy.life.expectancy ~ Social.support, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Healthy.life.expectancy" à partir de "Logged.GDP.per.capita"
lm_model <- lm(Healthy.life.expectancy ~ Logged.GDP.per.capita, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Healthy.life.expectancy" à partir de "Freedom.to.make.life.choices"
lm_model <- lm(Healthy.life.expectancy ~ Freedom.to.make.life.choices, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Healthy.life.expectancy" à partir de "Generosity"
lm_model <- lm(Healthy.life.expectancy ~ Generosity, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Healthy.life.expectancy" à partir de "Perceptions.of.corruption"
lm_model <- lm(Healthy.life.expectancy ~ Perceptions.of.corruption, data = donnees)
summary(lm_model)


## 4/ RÉGRÉSSION LINÉAIRE POUR FREEDOM TO MAKE LIFE CHOICES ##


# Régression linéaire pour prédire "Freedom.to.make.life.choices" à partir de "Ladder.score"
lm_model <- lm(Freedom.to.make.life.choices ~ Ladder.score, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Freedom.to.make.life.choices" à partir de "Social.support"
lm_model <- lm(Freedom.to.make.life.choices ~ Social.support, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Freedom.to.make.life.choices" à partir de "Logged.GDP.per.capita"
lm_model <- lm(Freedom.to.make.life.choices ~ Logged.GDP.per.capita, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Freedom.to.make.life.choices" à partir de "Healthy.life.expectancy"
lm_model <- lm(Freedom.to.make.life.choices ~ Healthy.life.expectancy, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Freedom.to.make.life.choices" à partir de "Generosity"
lm_model <- lm(Freedom.to.make.life.choices ~ Generosity, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Freedom.to.make.life.choices" à partir de "Perceptions.of.corruption"
lm_model <- lm(Freedom.to.make.life.choices ~ Perceptions.of.corruption, data = donnees)
summary(lm_model)


## 5/ RÉGRÉSSION LINÉAIRE POUR GENEROSITY ##


# Régression linéaire pour prédire "Generosity" à partir de "Ladder.score"
lm_model <- lm(Generosity ~ Ladder.score, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Generosity" à partir de "Social.support"
lm_model <- lm(Generosity ~ Social.support, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Generosity" à partir de "Logged.GDP.per.capita"
lm_model <- lm(Generosity ~ Logged.GDP.per.capita, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Generosity" à partir de "Freedom.to.make.life.choices"
lm_model <- lm(Generosity ~ Freedom.to.make.life.choices, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Generosity" à partir de "Healthy.life.expectancy"
lm_model <- lm(Generosity ~ Healthy.life.expectancy, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Generosity" à partir de "Perceptions.of.corruption"
lm_model <- lm(Generosity ~ Perceptions.of.corruption, data = donnees)
summary(lm_model)


## 6/ RÉGRÉSSION LINÉAIRE PERCEPTIONS OF CORRUPTION ##


# Régression linéaire pour prédire "Perceptions.of.corruption" à partir de "Ladder.score"
lm_model <- lm(Perceptions.of.corruption ~ Ladder.score, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Perceptions.of.corruption" à partir de "Social.support"
lm_model <- lm(Perceptions.of.corruption ~ Social.support, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Perceptions.of.corruption" à partir de "Logged.GDP.per.capita"
lm_model <- lm(Perceptions.of.corruption ~ Logged.GDP.per.capita, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Perceptions.of.corruption" à partir de "Freedom.to.make.life.choices"
lm_model <- lm(Perceptions.of.corruption ~ Freedom.to.make.life.choices, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "V" à partir de "Generosity"
lm_model <- lm(Perceptions.of.corruption ~ Generosity, data = donnees)
summary(lm_model)

# Régression linéaire pour prédire "Perceptions.of.corruption" à partir de "Healthy.life.expectancy"
lm_model <- lm(Perceptions.of.corruption ~ Healthy.life.expectancy, data = donnees)
summary(lm_model)


#----------------------------------------
#### TEST DU CHI-SQUARE ####
#----------------------------------------


# Test du Chi-square entre les catégories de "Regional.indicator" et "Perceptions.of.corruption"
chisq.test(donneesinferentielle$Regional.indicator, donneesinferentielle$Perceptions.of.corruption)


#----------------------------------------
#### FONCTION GRAPHIQUE DENSITÉ ÉCHANTILLON DE CHAQUE VARIABLE ####
#----------------------------------------


#----------------------------------------
### LECTURE DES DONNÉES ###
#----------------------------------------

donnees <- read.csv2("/Users/debidour/Desktop/ENSC/PERSONNEL/1A/MATHS/Projet R/Data.csv", header = TRUE, dec = ".", sep = ",")

# Voici une fonction permettant d'estimer et de représenter graphiquement la densité d'un échantillon (de manière plus "lisse" et plus "jolie") qu'un histogramme.
# NB : il s'agit d'un estimateur (nonparamétrique) à noyau de la densité (kernel estimator, en anglais) pour lequel il faut gérer un paramêtre "bandwidth" correspondant à la largeur de fenêtre :
# - plus la largeur de fenêtre est grande, plus l'estimateur sera "lisse" et va donc "écraser"l'information,
# - plus la largeur de fenêtre est petite, moins l'estimateur sera "lisse" et fera un "pic" uniquement là où il y a une observation.
# ==> il faut donc calibrer correctement ce paramêtre "width".  Une valeur raisonnable est proposée par défaut.


Graphdensite.prog <- function(x, bandwidth = 2 * (summary(x)[5] - summary(x)[2])) {
    plot(density(x, width = bandwidth),
        type = "l", xlab = "x",
        ylab = " ", main = " ", xlim = c(min(x) - sd(x), max(x) + sd(x))
    )
    title(paste("Estimation à noyau de la densité avec une largeur de fenêtre = ", bandwidth),
        cex.main = 0.8
    )
}

# Ladder.score 

Graphdensite.prog(joie) # une représentation graphique plus jolie de la densité des données joie
abline(v = 990, col = 2, lwd = 2) # affichage en rouge de la valeur obtenue 
abline(v = mean(donnees), col = 3, lwd = 2) # affichage en vert de la valeur moyenne 

# Logged GDP per capita

Graphdensite.prog(joie) # une représentation graphique plus jolie de la densité des données
abline(v = 990, col = 2, lwd = 2) # affichage en rouge de la valeur obtenue 
abline(v = mean(joie), col = 3, lwd = 2) # affichage en vert de la valeur

Graphdensite.prog(pib) # une représentation graphique plus jolie de la densité des données
abline(v = 990, col = 2, lwd = 2) # affichage en rouge de la valeur obtenue 
abline(v = mean(pib), col = 3, lwd = 2) # affichage en vert de la valeur moyenne 

Graphdensite.prog(joie) # une représentation graphique plus jolie de la densité des données
abline(v = 990, col = 2, lwd = 2) # affichage en rouge de la valeur obtenue 
abline(v = mean(donnees), col = 3, lwd = 2) # affichage en vert de la valeur moyenne 

Graphdensite.prog(joie) # une représentation graphique plus jolie de la densité des données
abline(v = 990, col = 2, lwd = 2) # affichage en rouge de la valeur obtenue 
abline(v = mean(donnees), col = 3, lwd = 2) # affichage en vert de la valeur moyenne 

Graphdensite.prog(joie) # une représentation graphique plus jolie de la densité des données
abline(v = 990, col = 2, lwd = 2) # affichage en rouge de la valeur obtenue 
abline(v = mean(donnees), col = 3, lwd = 2) # affichage en vert de la valeur

Graphdensite.prog(joie) # une représentation graphique plus jolie de la densité des données
abline(v = 990, col = 2, lwd = 2) # affichage en rouge de la valeur obtenue 
abline(v = mean(donnees), col = 3, lwd = 2) # affichage en vert de la valeur moyenne 

Graphdensite.prog(joie) # une représentation graphique plus jolie de la densité des données
abline(v = 990, col = 2, lwd = 2) # affichage en rouge de la valeur obtenue 
abline(v = mean(donnees), col = 3, lwd = 2) # affichage en vert de la valeur moyenne 

social <- donnees[, 8] # Social support
sante <- donnees[, 9] # Healthy life expectancy
liberte <- donnees[, 10] # Freedom to make life choices
generosite <- donnees[, 11] # Generosity
corrupt <- donnees[, 12] # Perceptions.of.corruption

#----------------------------------------
#### TEST DE NORMALITÉ DE CHAQUE VARIABLE ####
#----------------------------------------


## TEST POUR LADDER SCORE ##


# ===========================================
## test de Shapiro-Wilk ##
# ===========================================

shapiro.test(joie)

# ===========================================
## test de Kolmogorov ##
# ===========================================

ks.test(joie, "pnorm", mean(donnees), sqrt(var(joie)))

# Pour ces deux tests, l'hypothèse nulle est H0 : l'échantillon X1,...,X20 suit une loi normale et l'hypothèse alternative est H1 : non H0 (c'est à dire l'échantillon ne suit pas une loi normale).
#
# IMPORTANT : règle universelle d'interprétation des sorties d'un test d'hypothèses
# Il faut regarder la p-value du test et la comparer au risque de première espèce (alpha=Proba de rejeter H0 a tort) que l'utilisateur se donne (par exemple, alpha=5%) :
#   - Si p-value<alpha, alors on rejette H0 et on retient H1.
#   - Si p-value>alpha, alors on ne peut pas rejeter H0 et on conserve donc HO (parfois par défaut).

# ===========================================
# Commentaires des sorties des tests :
# ===========================================

# Pour les deux tests, on a des p-values supérieures à alpha=5% (p-value pour Shapiro-Wilk=9.88%>5%) et p-value pour SKolmogorov=54,1%>5%), on ne peut donc pas rejeter H0.
# En conclusion, on accepte que notre échantillon de travail provient bien d'une loi normale, et on peut donc mettre en oeuvre sereinement le test de Student.
#
# NB : théoriquement, le test de Kolmogorov a été developpé pour des variables aléatoires absolument continues et donc pour lesquelles la probabilité d'observer deux fois la même valeur est nulle.
# Mais ceci peut arriver dans la realité lorsque l'on dispose d'un échantillon d'observations.
# Le "Warning message" indique juste qu'il y a des valeurs égales dans l'échantillon, mais la fonction sait bien gérée ce cas pratique !
# En effet, si on bruite très légèrement l'échantillon pour ne plus avoir de valeurs égales, la p-value du test et la valeur de la statistique de test D ne vont pas être tres différentes :
 
ks.test(donnees + rnorm(length(donnees), mean = 0, sd = 0.001), "pnorm", mean(donnees), sqrt(var(donnees)))

# COMPLEMENTS sur le test de Kolmogorov : la statistique de test D correspond au "sup" de l'écart entre la fonction de répartition empirique de l'échantillon et celle de la loi normale ayant une moyenne égale à celle de l'échantillon et un écart-type égal à celui de l'échantillon.
# Si l'échantillon est bien issu d'une loi normale, la valeur de D devrait être petite.
# Le test rejette donc l'hypothèse H0 dès que la statistique de test D dépasse un certain seuil (qui a été calculé théoriquement par Kolmogorov).
# Les trois lignes de codes R ci-dessous permettent de tracer ces deux fonctions de répartition :

par(mfrow = c(1, 1))
plot.ecdf(joie) # tracé de la fonction de répartition empirique de l'échantillon (en noir)
plot.ecdf(rnorm(5000, mean = mean(joie), sd = sqrt(var(joie))), add = T, lty = "dotted", pch = " ", col = 2) # tracé de la fonction de répartition de la loi normale la mieux adaptée (en rouge).

# NB : ecdf = "empirical cumulative distribution function" = fonction de répartition empirique.
# On observe bien que ces deux courbes sont relativement proches l'une de l'autre, d'où le non-rejet de H0.

# ===========================================
# COMMENTAIRES 
# ===========================================

# Une fois la normalité acceptée, passons maintenant au test de Student.
# On désire tester si H0 : mu=990 (vitesse de Cornu) contre une hypothèse alternative qu'il convient de
# choisir convenable !
# Au des observations faites précédemment (vitesse moyenne observée par Michelson = 909, décalage de la
# densité vers la gauche par rapport à 990), il parait raisonnable de choisir comme alternative
# H1 : mu<990 (plutôt que l'hypothèse H1 par défaut qui est "mu différent de 990").
# Cela revient à repondre à la question "est-ce que la vitesse moyenne mu de Michelson est
# significativement plus petite que la vitesse 909 proposée par Cornu ?".
# NB : cela ne sert à rien de mettre une partie du risque de première espèce alpha sur la question "est-ce
# la vitesse moyenne mu de Michelson est significativement plus grande que la vitesse 909 proposée
# par Cornu ?". En règle générale, évitons de mettre du risque là où il n'y a pas de raison d'en mettre.
#
# Pour tester H0 : mu=990 contre H1 : mu <990, il faut taper la commende :

t.test(joie, mu = 990, alternative = "less")

# ===========================================
# Commentaires : pour lire les sorties de ce test
# ===========================================

# On retrouve de bas en haut l'estimation ponctuelle de mu, l'estimation par intervalle de confiance de mu
# avec un niveau de confiance à 95% (par défaut), puis le test de Student de H0 versus H1.
#
# 	One Sample t-test
#
# data:  ech
# t = -3.4524, df = 19, p-value = 0.001334   <-- test de Student de H0 : mu=990 contre H1 : mu<990
# alternative hypothesis: true mean is less than 990
# 95 percent confidence interval:  <--- estimation par intervalle de confiance de mu
#      -Inf 949.5692
# sample estimates:       <--- estimation ponctuelle de mu
# mean of x
#       909
#
# NB1 : pour l'intervalle de confiance, lorsque l'hypothèse alternative H1 testée est unilatérale, l'intervalle de confiance ne fournit qu'une borne supérieure si H1 : mu < mu0 ou qu'une borne supérieure si H1 : mu>mu0.
# Interprétation de l'intervalle de confiance obtenu ici (de manière un peu abusive car il s'agit de la réalisation de l'intevalle de confiance théorique qui est un objet aléatoire) : "Il y a 95% de chances que la vitesse moyenne de la lumière issu des expériences de Michelson soit inférieure à 949.57 (<990)".
#
# NB2 : pour le test de Student : "t" correspond à la réalisation de la statistique Tn du test de Student vu en cours, "df" correspond au nombre de degrés de liberté de la loi de Student que suit la statistique
# Tn sous H0, à savoir la loi de Student T(n-1). Au vu de la valeur de la statistique de test Tn, de sa loi sous H0 (T(n-1)) et de l'hypothèse H1 (qui donne la forme de la région de rejet), la "p-value" du test a pu être calculée.
# Interprétation : ici p-value=0.13% < 5%(= risque de première espèce que l'on se donne), on rejette donc l'hypothèse nulle H0 : mu=990 et l'on retient l'hypothèse alternative H1 : mu<990.

# ===========================================
# COMPLEMENTS :
# ===========================================

# 1) Par défaut, si on ne précise pas l'alternative, le tets de Student réalisé est le suivant :
# H0 : mu=990 contre H1 : mu différent de 990, voir ci-dessous.
# Il s'agit d'une alternative bilatérale.
# L'intervalle de confiance de mu à 95% est alors fourni avec deux bornes en répartissant 2,5% du risque
# de chaque côté.

t.test(joie, mu = 990)

# 	One Sample t-test
#
# data:  ech
# t = -3.4524, df = 19, p-value = 0.002669
# alternative hypothesis: true mean is not equal to 990
# 95 percent confidence interval:
# 859.8931 958.1069
# sample estimates:
# mean of x
#       909
#
#-----------------------------------------------------
# Commentaires sur les sorties numériques de ce test :
#-----------------------------------------------------

# En ayant mis 2,5% du risque pour mu>990 (inutile ici)
# et 2,5% pour mu<990, on voit que la p-value du test est 0.27%<5% donc on rejette toujours H0,
# mais la p-value a augmenté par celle obtenue précédemment (p-value=0.13%)
# avec H1 : mu<990 (seule alternative utile).
#
# 2) Si on met l'alternative H1 :mu>990 (alternative absurde ici vu que l'on a observé une moyenne ( égale à
# 909) bien inférieure à 990), on obtient les sorties numériques suivantes :

t.test(joie, mu = 990, alternative = "greater")

# 	One Sample t-test
#
# data:  ech
# t = -3.4524, df = 19, p-value = 0.9987
# alternative hypothesis: true mean is greater than 990
# 95 percent confidence interval:
#  868.4308      Inf
# sample estimates:
# mean of x
#       909
#
#-----------------------------------------------------
# Commentaires sur les sorties numériques de ce test :
#-----------------------------------------------------

# On a ici une p-value égale à 99.87%>5%, ainsi on ne peut pas rejeter HO.
# On conserve clairement H0 : mu=990 par défaut ! En effet l'alternative H1 est ici pire comme hypothèse
# que H0. On sait bien que mu est significativement inférieur à 990 !
# C'est pour cela qu'il faut toujours bien réfléchir au choix de l'hypothèse alternative H1 et que
# l'étude descriptive préliminaire (statistiques descriptives, graphiques) est très utile.

#-------------------------------------------------------------------------------------------------
# Pour donner un intervalle de confiance de niveau 95% (pui de niveau 90%) du paramètre mu.
#-------------------------------------------------------------------------------------------------

#
# Si désire un intervalle de confiance bilatéral, il convient de ne pas spécifier l'alternative dans
# la fonction "t.test" (par défaut alternative="two.sided", c'est à dire "différent").
# Il est possible de récupérer uniquement les bornes de l'invervalle de confiance de mu en utilisant
# "$conf.int" et de gérer le niveau de confiance avec le paramètre "conf.level", par défaut 95%) :

t.test(joie, mu = 990, conf.level = 0.95)$conf.int # IC (bilatéral) de mu à 95%
t.test(joie, mu = 990, conf.level = 0.90)$conf.int # IC (bilatéral) de mu à 90%

# Commentaires sur ces deux intervalles de confiance de mu :
# L'intervalle de confiance de mu à 90% est naturellement plus étroit que celui à 95%, en effet on
# s'autorise 5% d'erreur supplémentaire.
# Pour l'interprétation de l'IC de mu à 95%, on peut dire (abusivement, voir une remarque précédente)
# qu'il y a 95% de chances que la vitesse moyenne de la lumière obtenue par Michelson
# soit comprise entre 859.8931 et 958.1069 (en rajoutant 299.000).

## TEST POUR PERCEPTION OF CORRUPTION ##


# ===========================================
## test de Shapiro-Wilk ##
# ===========================================

shapiro.test(corrupt)

# ===========================================
## test de Kolmogorov ##
# ===========================================

ks.test(corrupt, "pnorm", mean(donnees), sqrt(var(corrupt)))

# Pour ces deux tests, l'hypothèse nulle est H0 : l'échantillon X1,...,X20 suit une loi normale et l'hypothèse alternative est H1 : non H0 (c'est à dire l'échantillon ne suit pas une loi normale).
#
# IMPORTANT : règle universelle d'interprétation des sorties d'un test d'hypothèses
# Il faut regarder la p-value du test et la comparer au risque de première espèce (alpha=Proba de rejeter H0 a tort) que l'utilisateur se donne (par exemple, alpha=5%) :
#   - Si p-value<alpha, alors on rejette H0 et on retient H1.
#   - Si p-value>alpha, alors on ne peut pas rejeter H0 et on conserve donc HO (parfois par défaut).

# ===========================================
# Commentaires des sorties des tests :
# ===========================================

# Pour les deux tests, on a des p-values supérieures à alpha=5% (p-value pour Shapiro-Wilk=9.88%>5%) et p-value pour SKolmogorov=54,1%>5%), on ne peut donc pas rejeter H0.
# En conclusion, on accepte que notre échantillon de travail provient bien d'une loi normale, et on peut donc mettre en oeuvre sereinement le test de Student.
#
# NB : théoriquement, le test de Kolmogorov a été developpé pour des variables aléatoires absolument continues et donc pour lesquelles la probabilité d'observer deux fois la même valeur est nulle.
# Mais ceci peut arriver dans la realité lorsque l'on dispose d'un échantillon d'observations.
# Le "Warning message" indique juste qu'il y a des valeurs égales dans l'échantillon, mais la fonction sait bien gérée ce cas pratique !
# En effet, si on bruite très légèrement l'échantillon pour ne plus avoir de valeurs égales, la p-value du test et la valeur de la statistique de test D ne vont pas être tres différentes :

ks.test(donnees + rnorm(length(donnees), mean = 0, sd = 0.001), "pnorm", mean(donnees), sqrt(var(donnees)))

# COMPLEMENTS sur le test de Kolmogorov : la statistique de test D correspond au "sup" de l'écart entre la fonction de répartition empirique de l'échantillon et celle de la loi normale ayant une moyenne égale à celle de l'échantillon et un écart-type égal à celui de l'échantillon.
# Si l'échantillon est bien issu d'une loi normale, la valeur de D devrait être petite.
# Le test rejette donc l'hypothèse H0 dès que la statistique de test D dépasse un certain seuil (qui a été calculé théoriquement par Kolmogorov).
# Les trois lignes de codes R ci-dessous permettent de tracer ces deux fonctions de répartition :

par(mfrow = c(1, 1))
plot.ecdf(v) # tracé de la fonction de répartition empirique de l'échantillon (en noir)
plot.ecdf(rnorm(5000, mean = mean(corrupt), sd = sqrt(var(corrupt))), add = T, lty = "dotted", pch = " ", col = 2) # tracé de la fonction de répartition de la loi normale la mieux adaptée (en rouge).

# NB : ecdf = "empirical cumulative distribution function" = fonction de répartition empirique.
# On observe bien que ces deux courbes sont relativement proches l'une de l'autre, d'où le non-rejet de H0.

# ===========================================
# COMMENTAIRES 
# ===========================================

# Une fois la normalité acceptée, passons maintenant au test de Student.
# On désire tester si H0 : mu=990 (vitesse de Cornu) contre une hypothèse alternative qu'il convient de
# choisir convenable !
# Au des observations faites précédemment (vitesse moyenne observée par Michelson = 909, décalage de la
# densité vers la gauche par rapport à 990), il parait raisonnable de choisir comme alternative
# H1 : mu<990 (plutôt que l'hypothèse H1 par défaut qui est "mu différent de 990").
# Cela revient à repondre à la question "est-ce que la vitesse moyenne mu de Michelson est
# significativement plus petite que la vitesse 909 proposée par Cornu ?".
# NB : cela ne sert à rien de mettre une partie du risque de première espèce alpha sur la question "est-ce
# la vitesse moyenne mu de Michelson est significativement plus grande que la vitesse 909 proposée
# par Cornu ?". En règle générale, évitons de mettre du risque là où il n'y a pas de raison d'en mettre.
#
# Pour tester H0 : mu=990 contre H1 : mu <990, il faut taper la commende :

t.test(corrupt, mu = 990, alternative = "less")

# ===========================================
# Commentaires : pour lire les sorties de ce test
# ===========================================

# On retrouve de bas en haut l'estimation ponctuelle de mu, l'estimation par intervalle de confiance de mu
# avec un niveau de confiance à 95% (par défaut), puis le test de Student de H0 versus H1.
#
# 	One Sample t-test
#
# data:  ech
# t = -3.4524, df = 19, p-value = 0.001334   <-- test de Student de H0 : mu=990 contre H1 : mu<990
# alternative hypothesis: true mean is less than 990
# 95 percent confidence interval:  <--- estimation par intervalle de confiance de mu
#      -Inf 949.5692
# sample estimates:       <--- estimation ponctuelle de mu
# mean of x
#       909
#
# NB1 : pour l'intervalle de confiance, lorsque l'hypothèse alternative H1 testée est unilatérale, l'intervalle de confiance ne fournit qu'une borne supérieure si H1 : mu < mu0 ou qu'une borne supérieure si H1 : mu>mu0.
# Interprétation de l'intervalle de confiance obtenu ici (de manière un peu abusive car il s'agit de la réalisation de l'intevalle de confiance théorique qui est un objet aléatoire) : "Il y a 95% de chances que la vitesse moyenne de la lumière issu des expériences de Michelson soit inférieure à 949.57 (<990)".
#
# NB2 : pour le test de Student : "t" correspond à la réalisation de la statistique Tn du test de Student vu en cours, "df" correspond au nombre de degrés de liberté de la loi de Student que suit la statistique
# Tn sous H0, à savoir la loi de Student T(n-1). Au vu de la valeur de la statistique de test Tn, de sa loi sous H0 (T(n-1)) et de l'hypothèse H1 (qui donne la forme de la région de rejet), la "p-value" du test a pu être calculée.
# Interprétation : ici p-value=0.13% < 5%(= risque de première espèce que l'on se donne), on rejette donc l'hypothèse nulle H0 : mu=990 et l'on retient l'hypothèse alternative H1 : mu<990.

# ===========================================
# COMPLEMENTS :
# ===========================================

# 1) Par défaut, si on ne précise pas l'alternative, le tets de Student réalisé est le suivant :
# H0 : mu=990 contre H1 : mu différent de 990, voir ci-dessous.
# Il s'agit d'une alternative bilatérale.
# L'intervalle de confiance de mu à 95% est alors fourni avec deux bornes en répartissant 2,5% du risque
# de chaque côté.

t.test(corrupt, mu = 990)

# 	One Sample t-test
#
# data:  ech
# t = -3.4524, df = 19, p-value = 0.002669
# alternative hypothesis: true mean is not equal to 990
# 95 percent confidence interval:
# 859.8931 958.1069
# sample estimates:
# mean of x
#       909
#
#-----------------------------------------------------
# Commentaires sur les sorties numériques de ce test :
#-----------------------------------------------------

# En ayant mis 2,5% du risque pour mu>990 (inutile ici)
# et 2,5% pour mu<990, on voit que la p-value du test est 0.27%<5% donc on rejette toujours H0,
# mais la p-value a augmenté par celle obtenue précédemment (p-value=0.13%)
# avec H1 : mu<990 (seule alternative utile).
#
# 2) Si on met l'alternative H1 :mu>990 (alternative absurde ici vu que l'on a observé une moyenne ( égale à
# 909) bien inférieure à 990), on obtient les sorties numériques suivantes :

t.test(corrupt, mu = 990, alternative = "greater")

# 	One Sample t-test
#
# data:  ech
# t = -3.4524, df = 19, p-value = 0.9987
# alternative hypothesis: true mean is greater than 990
# 95 percent confidence interval:
#  868.4308      Inf
# sample estimates:
# mean of x
#       909
#
#-----------------------------------------------------
# Commentaires sur les sorties numériques de ce test :
#-----------------------------------------------------

# On a ici une p-value égale à 99.87%>5%, ainsi on ne peut pas rejeter HO.
# On conserve clairement H0 : mu=990 par défaut ! En effet l'alternative H1 est ici pire comme hypothèse
# que H0. On sait bien que mu est significativement inférieur à 990 !
# C'est pour cela qu'il faut toujours bien réfléchir au choix de l'hypothèse alternative H1 et que
# l'étude descriptive préliminaire (statistiques descriptives, graphiques) est très utile.

#-------------------------------------------------------------------------------------------------
# Pour donner un intervalle de confiance de niveau 95% (pui de niveau 90%) du paramètre mu.
#-------------------------------------------------------------------------------------------------

#
# Si désire un intervalle de confiance bilatéral, il convient de ne pas spécifier l'alternative dans
# la fonction "t.test" (par défaut alternative="two.sided", c'est à dire "différent").
# Il est possible de récupérer uniquement les bornes de l'invervalle de confiance de mu en utilisant
# "$conf.int" et de gérer le niveau de confiance avec le paramètre "conf.level", par défaut 95%) :

t.test(corrupt, mu = 990, conf.level = 0.95)$conf.int # IC (bilatéral) de mu à 95%
t.test(corrupt, mu = 990, conf.level = 0.90)$conf.int # IC (bilatéral) de mu à 90%

# Commentaires sur ces deux intervalles de confiance de mu :
# L'intervalle de confiance de mu à 90% est naturellement plus étroit que celui à 95%, en effet on
# s'autorise 5% d'erreur supplémentaire.
# Pour l'interprétation de l'IC de mu à 95%, on peut dire (abusivement, voir une remarque précédente)
# qu'il y a 95% de chances que la vitesse moyenne de la lumière obtenue par Michelson
# soit comprise entre 859.8931 et 958.1069 (en rajoutant 299.000).


#-------------------------------------------------------------------------------------------------------------------------------------------------------


#####  ANALYSE EN COMPOSANTES PRINCIPALES (ACP) #####


#-------------------------------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------
# Installation de PCAmixdata
#----------------------------------------

install.packages("PCAmixdata")
library(PCAmixdata)

donnees <- read.csv2("/Users/debidour/Desktop/ENSC/PERSONNEL/1A/MATHS/Projet R/Data.csv", header = TRUE, dec = ".", sep = ",")

# chargement du package "PCAmixdata" afin de pouvoir l’utiliser par la suite

require(PCAmixdata)
help(PCAmix)

#----------------------------------------
# Mise en oeuvre de l’ACP
#----------------------------------------

donnees_numeriques <- donnees[, c("Ladder.score", "Logged.GDP.per.capita", "Social.support", "Healthy.life.expectancy", "Freedom.to.make.life.choices", "Generosity", "Perceptions.of.corruption")]
res <- PCAmix(donnees_numeriques) # tous les calculs de l’ACP sont stockes dans l’objet "res"n NB : par defaut les graphiques des plans factoriels 1-2 sont affiches a l’ecran
res <- PCAmix(donnees_numeriques, graph = FALSE) # idem sans les graphiques
res # permet de voir l’ensemble des sorties numeriques disponibles

#-----------------------------------
# Choix du nombre d’axes ~A retenir
#-----------------------------------

round(res$eig, digit = 2) # permet d’afficher les valeurs propres et les pourcentages de variances expliquées par chaque axe
# Graphique de l’ébouli des valeurs propres
barplot(res$eig[, 1], main = "Valeurs propres", names.arg = 1:nrow(res$eig))
abline(h = 1, col = 2, lwd = 2)

#--------------------------------------------------------------------
# Graphiques des individus et des variables sur le plan factoriel 1-2
#--------------------------------------------------------------------

?plot.PCAmix # permet d’afficher la fenetre d’aide de la commande "plot.PCA"
plot(res, axes = c(1, 2), choice = "ind") # on retrouve ici le graphique des individus (plan 1-2)
plot(res, axes = c(1, 2), choice = "cor") # on retrouve ici le cercle des corr\’elations
# des variables (plan 1-2)
plot(res, axes = c(1, 2), choice = "sqload") # on retrouve ici le graphique des "square loadings" (plan 1-2)

#--------------------------------------------------------------------
# Sorties numeriques pour les individus et es variables
#--------------------------------------------------------------------

res$ind # permet d’afficher l’ensemble des sorties numeriques associees aux individus :
# coordonnees, contributions, cosinus carres
round(res$ind$cos2, digit = 3) # uniquement les cosinus carres
res$quanti # permet d’afficher l’ensemble des sorties numeriques associees aux variables :
# coordonnees, contributions, cosinus carres
round(res$quanti$cos2, digit = 3) # uniquement les cosinus carres
