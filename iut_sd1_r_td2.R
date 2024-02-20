#TD 2

#Exercice 1 

getwd()

setwd("L:/BUT/SD/Promo 2023/glegalloudec/Programmation Statistiques/TD/TD2/Dataset")

getwd()

karts = read.csv("bodies_karts.csv", header = TRUE, dec = ",", sep =  ";")
drivers = read.csv("drivers.csv", header = TRUE, dec = ",", sep =  ";")
gliders = read.csv("gliders.csv", header = TRUE, dec = ".", sep =  "|")
tires = read.csv("tires.csv", header = TRUE, dec = ",", sep =  "\t")


dim(karts)
dim(tires)
dim(gliders)
dim(drivers)


#Exercice 2 

summary(karts)
summary(tires)
summary(gliders)
summary(drivers)

plot(drivers$Weight, drivers$Acceleration, main = "drivers : weight/accelaration")

#plus le kart est lourd, plus le kart a du mal à accélérer
#il y a des doublons qui se sont superposés

coefCorr = cor(x = drivers$Weight, y = drivers$Acceleration)

w_a = cov(drivers$Weight, y = drivers$Acceleration)
eX = sd(drivers$Weight)
eY = sd(drivers$Acceleration) 
print(w_a/(eX*eY))

coefDeter = coefCorr^2
print(coefDeter)

matriceCor = round(cor(drivers[, -1]), 2)
View(matriceCor)

install.packages("corrplot")

library(corrplot)

corrplot(matriceCor, method = "circle")

matriceCor1 = round(cor(karts[, -1]), 2)
matriceCor2 = round(cor(gliders[, -1]), 2)
matriceCor3 = round(cor(tires[, -1]), 2)

corrplot(matriceCor1, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
)

#+ : accél/mini-turbo   - : ground.speed/miniturbo

corrplot(matriceCor2, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
)


corrplot(matriceCor3, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
)


#Exercice 3 

resultat = drivers[ , c("Driver", "Weight")]
View(resultat)

resultat1 = drivers[1:10, c("Driver", "Acceleration")]
View(resultat1)

resultat2 = drivers[ , -c(5,7,9)]
View(resultat2)

resultat3 = drivers[ , -c("Weight","Acceleration")] #cela fonctionne uniquement sur des index numériques.

resultat4 = drivers[ , c("Driver", "Acceleration", "Weight")]
View(resultat4)

resultat5 = drivers[c(3, 12, 32),]
View(resultat5)

resultat6 = drivers[c(32, 3, 12),]
View(resultat6)

ordreW = order(drivers$Weight)
resultat7 = drivers[ordreW ,c("Driver","Weight")]
View(resultat7)

ordreA = order(drivers$Acceleration, decreasing = TRUE)
resultat8 = drivers[ordreA, c("Driver",'Acceleration')]
View(resultat8)

ordreAW = order(drivers$Acceleration, drivers$Weight, decreasing = c(TRUE,FALSE))
resultat9 = drivers[ ordreAW  , c("Driver", "Acceleration","Weight") ]
View(resultat9)


#Exercice 4 

help(subset)

topDrivers = subset(drivers, subset = Acceleration == max(Acceleration), select = c("Driver","Acceleration"))
topGliders = subset(gliders, subset = Acceleration == max(Acceleration), select = c("Glider","Acceleration"))
topKarts = subset(karts, subset = Acceleration == max(Acceleration), select = c("Body","Acceleration"))
topTires = subset(tires, subset = Acceleration == max(Acceleration), select = c("Tire","Acceleration"))

print(topDrivers)
print(topGliders)
print(topKarts)
print(topTires)








































































































