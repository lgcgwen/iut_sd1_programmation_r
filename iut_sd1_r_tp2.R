#TP 2 

#Exercice 1 
df = read.csv("L:/BUT/SD/Promo 2023/glegalloudec/Programmation Statistiques/TP/TP2/fao.csv", header = TRUE, sep = ";", dec = ",")

length(df$Nom)
#186 pays sont présent

summary(df)

#Exercice 2 

mean(df$Dispo_alim) #moyenne

sum(df$Population, na.rm = TRUE) #somme

sd(df$Import_viande)  # ecart type
sd(df$Export_viande, na.rm = TRUE)    #na.rm enleve les données NULL

median(df$Prod_viande, na.rm = TRUE) #mediane

quantile(df$Dispo_alim, probs = c(0.25, 0.5, 0.75)) #quartiles

quantile(df$Import_viande, probs = seq(0.1, 0.01)) #centiles

#Exercice 3 

pp = order(df$Population, decreasing = TRUE)
pluspeup = df[pp, ]
top5 = head(pluspeup, n = 5)
View(top5)

mp = order(df$Population, decreasing = FALSE)
moinspeup = df[mp, ]
flop5 = head(moinspeup, n = 5)
View(flop5)

vianpl = order(df$Prod_viande, decreasing = FALSE)
vpl = df[vianpl,]
vpltop = head(vpl, n = 5)
View(vpltop)

vianmo = order(df$Prod_viande, decreasing = FALSE)
vpm = df[vianmo,]
vplflop = head(vpm, n = 5)
View(vplflop)

sup2300 = subset(df$Dispo_alim, df$Dispo_alim >= 2300)
length(sup2300)

sup23k1m = subset(df, df$Dispo_alim > 3500 & df$Import_viande >= 1000000)
length(sup23k1m)

df[c(18, 57), ]

#Exercice 4 

df$part_export = 2

df$Dispo_alim_pays = df$Population * df$Dispo_alim

write.table(df, file = "L:/BUT/SD/Promo 2023/glegalloudec/Programmation Statistiques/TP/TP2/ExportTP2", sep = ";", row.names = FALSE)

dispomax = sum(df$Dispo_alim_pays, na.rm = TRUE)
adultenourri = dispomax/2300
print(adultenourri)

plot(df$Prod_viande, df$Export_viande, xlab = "Prod_viande", ylab = "Export_viande", main = "Nuage de points")

cor(x = df$Prod_viande, y = df$Export_viande, method = "spearman")

matriceCor = cor(df[, -1])
matriceCor = round(matriceCor, 2)
View(matriceCor)

install.packages("corrplot")

corrplot(matriceCor, method = "color", type="upper", order="hclust",add.Coef.col="black",tl.col="black",tl.srt=45,diag=FALSE)







