#Projet R 2020 World Hapiness

donnees<-read.csv2("/Users/debidour/Desktop/ENSC/PERSONNEL/1A/MATHS/Projet R/Data.csv",header=TRUE,dec=".",sep=",")

#Données utiles à notre étude

joie <- donnees[3] #Ladder.score (ou niveau de joie)
joie_num <- as.numeric(joie)
pays <- donnees[1] #liste des pays 
region <- donnees[2]#régionalindicator
pib <- donnees[7] #Logged GDP per capita
social <- donnees[8] #Social support
sante <- donnees[9] #Healthy life expectancy
liberte <- donnees[10] #Freedom to make life choices
generosite <- donnees[11] #Generosity
corrupt <- donnees[12] #Perceptions.of.corruption

#Données moins intéressantes dans un premier temps

donnees[4]#Standard.error.of.ladder.score
donnees[5]#upperwhisker
donnees[6]#lowerwhisker
donnees[13]#Ladder score in Dystopia
donnees[14]#Explained by: Log GDP per capita
donnees[15]#Explained by: Social support
donnees[16]#Explained by: Healthy life expectancy
donnees[17]#Explained by: Freedom to make life choices
donnees[18]#Explained by: Generosity
donnees[19]#Explained by: Perceptions of corruption
donnees[20]#Dystopia + residual


## STATISTIQUE DESCRIPTIVE


#Niveau de joie

max(joie)
min(joie)
range(joie)
median(joie)
summary(joie)
quantile(joie, probs=seq(from=0.1,to=0.9,by=0.1))
par(mfrow=c(2,1))
hist(joie)
par(mfrow=c(1,1))
boxplot(joie)
head(joie)    # affiche les 6 premieres lignes du jeu de donnees "temp"
dim(joie)     # donne le nombre de lignes et de colonnes du jeu de donnees
joie          # affiche l'integralite du jeu de donnees
dimnames(joie)  # affiche les noms des lignes et des colonnes (sous forme de liste)
rownames(joie)  # affiche les noms des lignes (individus)
colnames(joie)  # affiche les noms des colonnes (variables) 
apply(joie[,-1],2,mean)     # temp[,-1] permet de supprimer la 1ere colonne (=ville) qui est une variable qualitative (categorielle)
round(apply(joie[,-1],2,mean),digits=1)  # on arrondit a un chiffre apres la virgule


#Listes des divers pays 


#Liste des régions du monde


#GDP

max(pib)
min(pib)
range(pib)
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

#Soutien Social
max(social)
min(social)
range(social)
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

#Espérance de vie en bonne santé
max(sante)
max(sante)
range(sante)
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

#Liberté de faire ses propres choix de vie
max(liberte)
max(liberte)
range(liberte)
median(liberte)
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

#Générosité
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

#Niveau de corruption perçue
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




## STATISTIQUE INFERENTIELLE 



## ANALYSE EN COMPOSANTES PRINCIPALES (ACP)



require(PCAmixdata) # permet de charger le package "PCAmixdata" 
# Mise en oeuvre de l'ACP
#----------------------------------------
res<-PCAmix(pib[1,10]) # tous les calculs de l'ACP sont stockes dans l'objet "res"
# NB : par défaut les 3 graphiques des plans factoriels 1-2 
# sont affichés a l'écran : 
#   - projection des individus sur le plan 1-2
#   - cercle des corrélations du plan 1-2
#   - Graphique des "squared loadings" du plan 1-2
