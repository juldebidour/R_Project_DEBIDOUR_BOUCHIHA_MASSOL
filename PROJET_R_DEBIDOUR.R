#Projet R 2020 World Hapiness

donnees<-read.csv2("/Users/debidour/Desktop/ENSC/PERSONNEL/1A/MATHS/Projet R/Data.csv",header=TRUE,dec=".",sep=",")
donnees[1]#liste des pays 
donnees[2]#régionalindicator
donnees[3]#Ladder.score
donnees[4]#Standard.error.of.ladder.score
donnees[5]#upperwhisker
donnees[6]#lowerwhisker
donnees[7]#Logged GDP per capita
donnees[8]#Social support
donnees[9]#Healthy life expectancy
donnees[10]#Freedom to make life choices
donnees[11]#Generosity
donnees[12]#Perceptions.of.corruption
donnees[13]#Ladder score in Dystopia
donnees[14]#Explained by: Log GDP per capita
donnees[15]#Explained by: Social support
donnees[16]#Explained by: Healthy life expectancy
donnees[17]#Explained by: Freedom to make life choices
donnees[18]#Explained by: Generosity
donnees[19]#Explained by: Perceptions of corruption
donnees[20]#Dystopia + residual


max(donnees[7])#plus grand GDP

