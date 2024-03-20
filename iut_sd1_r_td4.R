#TD4

#Exercice 1

df = read.csv("velov.csv",header = TRUE, sep = ";", dec = ",")

summary(df)
class(df$status)
class(df$CodePostal)

df$status = as.factor(df$status)
df$CodePostal = as.factor(df$CodePostal)
summary(df)

df$bornes = ifelse(df$capacity != (df$bikes + df$stands), "KO", "OK")

#Exercice 2 

hist(df$capacity, 
     main = "Histogramme de la distribution de la capacité", 
     breaks = 6, 
     col = "red", 
     xlab = "Capacity")

abline(a = 100, b = 0, col = "blue", lty = 2)

hist(df$capacity, 
     main = "Histogramme de la distribution de la capacité", 
     probability = TRUE,
     col = "red", 
     xlab = "Capacity",
     ylim = c(0, 0.08))

lines(density(df$capacity), lwd = 2, lty = 2, col = "blue")

#Exercice 3 

boxplot(df$capacity,
        main = "Boite à moustache de capacity",
        outline = FALSE)

points(mean(df$capacity), col = "red", pch = 15, cex = 3)

par(mfrow = c(1,2)) #Scindé en deux pour faire 2 graphique

df7 = subset(df, CodePostal == 69007)
boxplot(df7$bikes,
        main = "Boite à moustache vélo du 7ème"
        )

df8 = subset(df, CodePostal == 69008)
boxplot(df8$bikes,
        main = "Boite à moustache vélo du 8ème"
)

par(mfrow = c(1,1))
boxplot(formula = bikes ~ bonus, 
        data = df, 
        main = "Dispos vélos selon station bonus")

moyenne = tapply(X = df$bikes,
                 INDEX = df$bonus,
                 FUN = function(x) mean(x))
points(x= moyenne, col = "red", cph = 19)

#Exercice 4 

barplot(table(df$bonus),
        main = "Diagramme de la répartition du nombre de station bonus",
        horiz = TRUE) 

prob = prop.table(df$bonus)
barplot(prob,
        main = "Répartition en %",
        horiz = TRUE)

effectif = table(df$banking, df$bonus)
print(effectif)
barplot(effectif,
        main = "Station avec banque VS Station bonus",
        xlab = "Station bonus ?")

frequence = prop.table(x = effectif)
barplot(height = frequence,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?",
        col = c("red","green"))

legend_labels = colnames(frequence)

legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green"))

frequence = prop.table(x = effectif, margin = 2)
barplot(height = frequence,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?",
        col = c("red","green"))

legend_labels = colnames(frequence)

legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green"))

frequence = prop.table(x = effectif, margin = 2)
barplot(height = frequence,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?",
        col = c("red","green"),
        beside = TRUE)

legend_labels = colnames(frequence)

legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green"))

etiquette = paste(rownames(table(df$bonus),"\n",table(df$bonus)))
pie(table(df$bonus),
    main = "diagramme de la répartition des station bonus", 
    col = c("yellow", "green"),
    labels = etiquette)

effectif = table(df$CodePostal)
top10 = sort(effectif, decreasing = TRUE)[1:10]
barplot(height = top10,
        main = "Top 10 sur le \n nombre de station",
        col = palette(),
        las = 2)

barplot(height = top10,
        main = "Top 10 sur le \n nombre de station",
        col = colors(),
        las = 2)

dev.print(device = png, file = "GraphTD4.png", width = 600)

#Exercice 5 

plot(x = df$stands, y = df$capacity,
      main = "COrrélation entre dispo et capacité",
     xlim = c(0,60),
     ylim = c(0,60),
     pch = 19) 

df$bornes = as.factor(df$bornes)
plot(x = df$stands, y = df$capacity,
     main = "COrrélation entre dispo et capacité",
     xlim = c(0,60),
     ylim = c(0,60),
     col = df$bornes,
     pch = 19) 

legend("topright", 
       legend = levels(df$bornes), 
       col = palette(), 
       pch = 19)

myColors = c("red", "blue", "green", "purple", "pink")

plot(x = df$stands, y = df$capacity,
     main = "Place disponible vs Capacité",
     xlim = c(0, 60),
     ylim = c(0, 60),
     col = myColors[df$bornes],
     pch = 19)

legend("topright", 
       legend = levels(df$bornes), 
       col = myColors, 
       pch = 19)

moystand = mean(df$stands)
moycapa = mean(df$capacity)

points(x = moystand,
       y = moycapa, 
       pch = 15,
       col = "green",
       cex = 2)



#Exercice 6

# Librairies nécessaires
install.packages("leaflet")
install.packages("dplyr")
install.packages("ggplot2")
library(leaflet)
library(dplyr)
library(ggplot2)

# Créer une carte Leaflet
maCarte <- leaflet(df) %>% 
  addTiles() %>% 
  addMarkers(~position_longitude, 
             ~position_latitude, 
             popup = ~address)

# Afficher la carte
maCarte


