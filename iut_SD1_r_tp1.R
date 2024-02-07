#TP 1

#Exercice 1

iris
class(iris)

view(iris)

nrow(iris)

ncol(iris)

colnames(iris)

summary(iris)

iris[c("Sepal.Length", "Species")]

iris[c(100, 103, 105),]

iris[(50:100),]

mean(iris$Sepal.Length)

sd(iris$Petal.Width)

sd(iris$Petal.Length)

quantile(iris$Petal.Width, seq(0.1,0.9,0.01))

#Exercice 2 

anime = read.csv("L:/BUT/SD/Promo 2023/glegalloudec/Programmation Statistiques/TP/TP 1/anime.csv", header = TRUE, sep = ",", dec = ".")
manga = read.csv("L:/BUT/SD/Promo 2023/glegalloudec/Programmation Statistiques/TP/TP 1/manga.csv", header = TRUE, sep = ",", dec = ".")
class(manga)
class(anime)

View(anime)
View(manga)

dim(anime)
dim(manga)

mean(anime$Score)
mean(manga$Score)

sum(anime$Vote)
sum(manga$Vote)

sd(anime$Score)
sd(manga$Score)

quantile(anime$Score, seq(0.1,0.9,0.01))
quantile(manga$Score, seq(0.1,0.9,0.01))

extraction1 = subset(manga, Score > 9)
nrow(extraction1)

extraction2 = subset(manga, Vote >= 200000)
nrow(extraction2)

extraction3 = subset(manga, Vote > 200000 & Score >= 8)
nrow(extraction3)

extraction4 = subset(manga, Score > 7 & Score < 8)
nrow(extraction4)

effectifs = table(anime$Rating)
print(effectifs)
length(effectifs)
prop.table(effectifs)

extraction5 = subset(anime, Rating == "R - 17+ (violence & profanity)")
nrow(extraction5)

extraction6 = subset(anime, Rating =="R - 17+ (violence & profanity)" & Score > 8)
nrow(extraction6)

extraction7 = subset(anime, Rating != "R - 17+ (violence & profanity)")
nrow(extraction7)

extration8 = subset(anime, Rating == "PG - Children" & Rating == "G - All Ages")
nrow(extration8)

extration9 = subset(anime, Rating != "PG - Children" & Rating != "G - All Ages")
nrow(extration9)

extraction10 = subset(anime, Score > 9, Vote > 400000)
nrow(extraction10)

anime = anime[, c("Title","Score","Vote","Ranked")]
manga = manga[, c("Title","Score","Vote","Ranked")]

anime = "Anime"
manga = "Manga"

dfConcat = rbind(manga,anime)
View(dfConcat)

write.table(dfConcat,"L:/BUT/SD/Promo 2023/glegalloudec/Programmation Statistiques/TP/TP 1/ExportTP1.csv", 
            sep = ";", row.names = FALSE)