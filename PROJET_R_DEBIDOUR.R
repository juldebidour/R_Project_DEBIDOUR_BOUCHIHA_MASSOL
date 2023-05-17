#Projet R 2020 World Hapiness

donnees<-read.csv2("/Users/debidour/Desktop/ENSC/PERSONNEL/1A/MATHS/Projet R/Data.csv",header=TRUE,dec=".",sep=",")

#INTRODUCTION SUR LA LISTE DES DONNÉES 
summary(donnees)
dimnames(donnees)  # affiche les noms des lignes et des colonnes (sous forme de liste)
rownames(donnees)  # affiche les noms des lignes (numéro des pays)
colnames(donnees)  # affiche les noms des colonnes (variables étudiées içi) 
apply(donnees[,-1],2,mean)     # donnees[,-1] permet de supprimer la 1ere colonne (içi country) qui est une variable qualitative (categorielle)
round(apply(donnees[,-1],2,mean),digits=1)  # on arrondit a un chiffre apres la virgule

#Données utiles à notre étude

joie <- donnees[,3] #Ladder.score (ou niveau de joie)
pays <- donnees[,1] #liste des pays 
region <- donnees[,2]#régionalindicator
pib <- donnees[,7] #Logged GDP per capita
social <- donnees[,8] #Social support
sante <- donnees[,9] #Healthy life expectancy
liberte <- donnees[,10] #Freedom to make life choices
generosite <- donnees[,11] #Generosity
corrupt <- donnees[,12] #Perceptions.of.corruption

#Données moins intéressantes dans un premier temps

donnees[,4]#Standard.error.of.ladder.score
donnees[,5]#upperwhisker
donnees[,6]#lowerwhisker
donnees[,13]#Ladder score in Dystopia
donnees[,14]#Explained by: Log GDP per capita
donnees[,15]#Explained by: Social support
donnees[,16]#Explained by: Healthy life expectancy
donnees[,17]#Explained by: Freedom to make life choices
donnees[,18]#Explained by: Generosity
donnees[,19]#Explained by: Perceptions of corruption
donnees[,20]#Dystopia + residual


                                                                      ##### STATISTIQUE DESCRIPTIVE ##### 

#----------------------------------------
#étude du Niveau de joie ou bonheur
#----------------------------------------

summary(joie)
max(joie)
min(joie)
range(joie)
mean(joie)
variance(joie)
median(joie)
# étude du niveau de joie selon les régions du monde
mean(joie[which(region=="Western Europe")])
mean(joie[-which(region=="Western Europe")])

mean(joie[which(region=="North America and ANZ")])
mean(joie[-which(region=="North America and ANZ")])

mean(joie[which(region == "Western Europe" | region == "North America and ANZ")])
mean(joie[-which(region == "Western Europe" | region == "North America and ANZ")])

mean(joie[which(region=="Middle East and North Africa ")])
mean(joie[-which(region=="Middle East and North Africa ")])

mean(joie[which(region=="Sub-Saharan Africa")])
mean(joie[-which(region=="Sub-Saharan Africa")])

mean(joie[which(region=="Central and Eastern Europe")])
mean(joie[-which(region=="Central and Eastern Europe")])

mean(joie[which(region=="Latin America and Caribbean ")])
mean(joie[-which(region=="Latin America and Caribbean ")])

mean(joie[which(region=="East Asia")])
mean(joie[-which(region=="East Asia")])

mean(joie[which(region=="Southeast Asia")])
mean(joie[-which(region=="Southeast Asia")])

mean(joie[which(region=="South Asia")])
mean(joie[-which(region=="South Asia")])

mean(joie[which(region=="Commonwealth of Independent States")])
mean(joie[-which(region=="Commonwealth of Independent States")])

quantile(joie, probs=seq(from=0.1,to=0.9,by=0.1))
par(mfrow=c(2,1))
hist(joie)
par(mfrow=c(1,1))
boxplot(joie)
head(joie)    # affiche les 6 premieres lignes du jeu de donnees joie
dim(joie)     # donne le nombre de lignes et de colonnes du jeu de donnees
joie          # affiche l'integralite du jeu de donnees

#----------------------------------------
#Listes des divers pays 
#----------------------------------------


#----------------------------------------
#Liste des régions du monde
#----------------------------------------

table(region)

#----------------------------------------
#GDP
#----------------------------------------

summary(pib)
max(pib)
min(pib)
range(pib)
mean(pib)
variance(pib)
median(pib)
summary(pib)
quantile(pib, probs=seq(from=0.1,to=0.9,by=0.1))
par(mfrow=c(2,1))
hist(pib)
par(mfrow=c(1,1))
boxplot(pib)
head(pib)    # affiche les 6 premieres lignes du jeu de donnees "temp"
dim(pib)     # donne le nombre de lignes et de colonnes du jeu de donnees
pib          # affiche l'integralite du jeu de donnees
dimnames(pib)  # affiche les noms des lignes et des colonnes (sous forme de liste)
rownames(pib)  # affiche les noms des lignes (individus)
colnames(pib)  # affiche les noms des colonnes (variables) 
apply(pib[,-1],2,mean)     # temp[,-1] permet de supprimer la 1ere colonne (=ville) qui est une variable qualitative (categorielle)
round(apply(pib[,-1],2,mean),digits=1)  # on arrondit a un chiffre apres la virgule

#----------------------------------------
#Soutien Social
#----------------------------------------

summary(social)
max(social)
min(social)
range(social)
mean(social)
variance(social)
median(social)
summary(social)
quantile(social, probs=seq(from=0.1,to=0.9,by=0.1))
par(mfrow=c(2,1))
hist(social)
par(mfrow=c(1,1))
boxplot(social)
head(social)    # affiche les 6 premieres lignes du jeu de donnees "temp"
dim(social)     # donne le nombre de lignes et de colonnes du jeu de donnees
social          # affiche l'integralite du jeu de donnees
dimnames(social)  # affiche les noms des lignes et des colonnes (sous forme de liste)
rownames(social)  # affiche les noms des lignes (individus)
colnames(social)  # affiche les noms des colonnes (variables) 
apply(social[,-1],2,mean)     # temp[,-1] permet de supprimer la 1ere colonne (=ville) qui est une variable qualitative (categorielle)
round(apply(social[,-1],2,mean),digits=1)  # on arrondit a un chiffre apres la virgule

#----------------------------------------
#Espérance de vie en bonne santé
#----------------------------------------

max(sante)
max(sante)
range(sante)
mean(sante)
variance(sante)
median(sante)
summary(sante)
quantile(sante, probs=seq(from=0.1,to=0.9,by=0.1))
par(mfrow=c(2,1))
hist(sante)
par(mfrow=c(1,1))
boxplot(sante)
head(sante)    # affiche les 6 premieres lignes du jeu de donnees "temp"
dim(sante)     # donne le nombre de lignes et de colonnes du jeu de donnees
sante          # affiche l'integralite du jeu de donnees
dimnames(sante)  # affiche les noms des lignes et des colonnes (sous forme de liste)
rownames(sante)  # affiche les noms des lignes (individus)
colnames(sante)  # affiche les noms des colonnes (variables) 
apply(sante[,-1],2,mean)     # temp[,-1] permet de supprimer la 1ere colonne (=ville) qui est une variable qualitative (categorielle)
round(apply(sante[,-1],2,mean),digits=1)  # on arrondit a un chiffre apres la virgule

#----------------------------------------
#Liberté de faire ses propres choix de vie
#----------------------------------------

max(liberte)
max(liberte)
range(liberte)
median(liberte)
mean(liberte)
variance(liberte)
summary(liberte)
quantile(liberte, probs=seq(from=0.1,to=0.9,by=0.1))
par(mfrow=c(2,1))
hist(liberte)
par(mfrow=c(1,1))
boxplot(liberte)
head(liberte)    # affiche les 6 premieres lignes du jeu de donnees "temp"
dim(liberte)     # donne le nombre de lignes et de colonnes du jeu de donnees
liberte          # affiche l'integralite du jeu de donnees
dimnames(liberte)  # affiche les noms des lignes et des colonnes (sous forme de liste)
rownames(liberte)  # affiche les noms des lignes (individus)
colnames(liberte)  # affiche les noms des colonnes (variables) 
apply(liberte[,-1],2,mean)     # temp[,-1] permet de supprimer la 1ere colonne (=ville) qui est une variable qualitative (categorielle)
round(apply(liberte[,-1],2,mean),digits=1)  # on arrondit a un chiffre apres la virgule

#----------------------------------------
#Générosité
#----------------------------------------

max(generosite)
max(generosite)
range(generosite)
median(generosite)
summary(generosite)
quantile(generosite, probs=seq(from=0.1,to=0.9,by=0.1))
par(mfrow=c(2,1))
hist(generosite)
par(mfrow=c(1,1))
boxplot(generosite)
head(generosite)    # affiche les 6 premieres lignes du jeu de donnees "temp"
dim(generosite)     # donne le nombre de lignes et de colonnes du jeu de donnees
generosite          # affiche l'integralite du jeu de donnees
dimnames(generosite)  # affiche les noms des lignes et des colonnes (sous forme de liste)
rownames(generosite)  # affiche les noms des lignes (individus)
colnames(generosite)  # affiche les noms des colonnes (variables) 
apply(generosite[,-1],2,mean)     # temp[,-1] permet de supprimer la 1ere colonne (=ville) qui est une variable qualitative (categorielle)
round(apply(generosite[,-1],2,mean),digits=1)  # on arrondit a un chiffre apres la virgule

#----------------------------------------
#Niveau de corruption perçue
#----------------------------------------

max(corrupt)
max(corrupt)
range(corrupt)
median(corrupt)
summary(corrupt)
quantile(corrupt, probs=seq(from=0.1,to=0.9,by=0.1))
par(mfrow=c(2,1))
hist(corrupt)
par(mfrow=c(1,1))
boxplot(corrupt)
head(corrupt)    # affiche les 6 premieres lignes du jeu de donnees "temp"
dim(corrupt)     # donne le nombre de lignes et de colonnes du jeu de donnees
corrupt          # affiche l'integralite du jeu de donnees
dimnames(corrupt)  # affiche les noms des lignes et des colonnes (sous forme de liste)
rownames(corrupt)  # affiche les noms des lignes (individus)
colnames(corrupt)  # affiche les noms des colonnes (variables) 
apply(corrupt[,-1],2,mean)     # temp[,-1] permet de supprimer la 1ere colonne (=ville) qui est une variable qualitative (categorielle)
round(apply(corrupt[,-1],2,mean),digits=1)  # on arrondit a un chiffre apres la virgule




                                                                                              ##### STATISTIQUE INFERENTIELLE #####


donnees<-read.csv2("/Users/debidour/Desktop/ENSC/PERSONNEL/1A/MATHS/Projet R/Data.csv",header=TRUE,dec=".",sep=",")

# Créer un dataframe avec les données
donneesinferentielle <- data.frame(summary(donnees[, c("Ladder.score","Logged.GDP.per.capita", "Social.support", "Healthy.life.expectancy", "Freedom.to.make.life.choices", "Generosity", "Perceptions.of.corruption")]))

# Résumé des variables numériques
summary(donneesinferentielle[, c("Ladder.score", "Logged.GDP.per.capita", "Social.support", "Healthy.life.expectancy", "Freedom.to.make.life.choices", "Generosity", "Perceptions.of.corruption")])

# Test d'hypothèse : Comparaison de la moyenne de "Ladder.score" à une valeur spécifique
t.test(donneesinferentielle$Ladder.score, mu = 5)


                            ### TESTS DE CORRÉLATION ####


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



          #### RÉGRÉSSION LINÉAIRE ####

  

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



          #### TEST DU CHI-SQUARE #### 

# Test du Chi-square entre les catégories de "Regional.indicator" et "Perceptions.of.corruption"
chisq.test(donneesinferentielle$Regional.indicator, donneesinferentielle$Perceptions.of.corruption)


                                                                                #####  ANALYSE EN COMPOSANTES PRINCIPALES (ACP) ##### 

#----------------------------------------
# Installation de PCAmixdata
#----------------------------------------
install.packages("PCAmixdata")
library(PCAmixdata)

donnees<-read.csv2("/Users/debidour/Desktop/ENSC/PERSONNEL/1A/MATHS/Projet R/Data.csv",header=TRUE,dec=".",sep=",")

# chargement du package "PCAmixdata" afin de pouvoir l’utiliser par la suite

require(PCAmixdata)
help(PCAmix)

#----------------------------------------
# Mise en oeuvre de l’ACP
#----------------------------------------

donnees_numeriques <- donnees[, c("Ladder.score","Logged.GDP.per.capita", "Social.support", "Healthy.life.expectancy", "Freedom.to.make.life.choices", "Generosity", "Perceptions.of.corruption")]
res<-PCAmix(donnees_numeriques) # tous les calculs de l’ACP sont stockes dans l’objet "res"n NB : par defaut les graphiques des plans factoriels 1-2 sont affiches a l’ecran
res <- PCAmix(donnees_numeriques,graph=FALSE) # idem sans les graphiques
res # permet de voir l’ensemble des sorties numeriques disponibles

#-----------------------------------
# Choix du nombre d’axes ~A retenir
#-----------------------------------

round(res$eig,digit=2) # permet d’afficher les valeurs propres et les pourcentages
# de variances expliquees par chaque axe
# Graphique de l’ebouli des valeurs propres
barplot(res$eig[,1],main="Eigenvalues",names.arg=1:nrow(res$eig))
abline(h=1,col=2,lwd=2)

#--------------------------------------------------------------------
# Graphiques des individus et des variables sur le plan factoriel 1-2
#--------------------------------------------------------------------

?plot.PCAmix # permet d’afficher la fenetre d’aide de la commande "plot.PCA"
plot(res,axes=c(1,2),choice="ind") # on retrouve ici le graphique des individus (plan 1-2)
plot(res,axes=c(1,2),choice="cor") # on retrouve ici le cercle des corr\’elations
# des variables (plan 1-2)
plot(res,axes=c(1,2),choice="sqload") # on retrouve ici le graphique des "square loadings" (plan 1-2)

#--------------------------------------------------------------------
# Sorties numeriques pour les individus et es variables
#--------------------------------------------------------------------

res$ind # permet d’afficher l’ensemble des sorties numeriques associees aux individus :
# coordonnees, contributions, cosinus carres
round(res$ind$cos2,digit=3) # uniquement les cosinus carres
res$quanti # permet d’afficher l’ensemble des sorties numeriques associees aux variables :
# coordonnees, contributions, cosinus carres
round(res$quanti$cos2,digit=3) # uniquement les cosinus carres

