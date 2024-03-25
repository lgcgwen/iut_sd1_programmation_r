#TD5

#Exercice 1

par(mfrow = c(1,1))
plot(NA, xlim=c(-5, 5), ylim=c(0, 1), xlab="X", 
     ylab="Densité de probabilité", 
     main="Densités de probabilité \n de lois normales")

# Tracer la densité de probabilité pour chaque simulation
moyennes <- c(0, 0, 0, -2)
sigmas <- c(0.45, 1, 2.25, 0.7)
colors <- c("red", "blue", "green", "orange")
legend_labels <- c()
for (i in 1:length(moyennes)) {
  serie = rnorm(n = 100000, 
                mean = moyennes[i], 
                sd = sigmas[i])
  lines(density(serie), col = colors[i])
  legend_labels <- c(legend_labels, paste("m =", moyennes[i], ",", "s =", sigmas[i]))
}

# Ajouter une légende
legend("topright", legend=legend_labels, col=colors, lwd=2, cex=0.8)

serie1 = rnorm(10000, mean = 0, sd = 1)

hist(serie1, main = "Loi normal centrée réduite", probability =  TRUE)
lines(serie1)

median(serie1)

quantile(x = serie1)

quantile(x = serie1, probs = seq(from = 0, to = 1, by = 0.01))
#environ 1.66

qnorm(p = 0.95, mean = 0, sd= 1)
pnorm(q = 1.644854, mean = 0, sd = 1)

qnorm(p = 0.975, mean = 0, sd =  1)

pnorm(q = 1.96, mean = 0, sd = 1)

#Exrercice 2 

indices_lignes = seq(from = 0, to = 3.9, by = 0.1)

#on crée un vecteur vide pour ajouter les probas au fur et à mesure
all_probas = c()
#On parcourt les indices lignes
for (i in indices_lignes){
  proba = pnorm(q = i, mean = 0, sd = 1)
  #on ajoute la nouvelle proba au vecteur existant
  all_probas = c(all_probas,proba)
  all_probas = round(all_probas,digits = 4)
}

indices_colones = seq(from = 0.00, to = 0.09, by = 0.01)
indices_lignes = seq(from = 0, to = 3.9, by = 0.1)

#On crée un objet résultat vide.
resultat = NULL
#On parcourt les indices colonnes
for (j in indices_colones) {
  #on crée un vecteur vide pour ajouter les probas au fur et à mesure
  all_probas = c()
  #On parcourt les indices lignes
  for (i in indices_lignes){
    quantile = i + j
    proba = pnorm(q = quantile, mean = 0, sd = 1)
    #on ajoute la nouvelle proba au vecteur existant
    all_probas = c(all_probas,proba)
    all_probas = round(all_probas,digits = 4)
  }
  #On ajoute une colonne au resultat
  resultat = cbind(resultat,all_probas)
}

class(resultat)
table = data.frame(resultat)
colnames(table) = indices_colones
rownames(table) = indices_lignes
View(table)

#Exercice 3


population = rnorm(10000000, mean = 171, sd = 9)

mean(population)
sd(population)
#la moyenne est exactement la meme et l'ecart type est tres proche de 1 

hist(population)
#l'histogramme est bien en cloche

population190 = population[population < 190]
length(population190)
length(population190)/length(population)

pnorm(q = 190, mean = 171, sd = 9)

#tous la population de l'échantillon a une taille inférieur à 190 tandis que 
#je serais sensé en trouvé un peu moins de 9 999 000


pop200 = population[population > 200]
length(pop200)

1*1e7 - pnorm(q = 200, mean = 171, sd = 9)*1e7

#je suis sensé trouvé environ 9600 personnes contre 0 observé dans l'échantillon 

#Exercice 4 

echantillon = sample(x = population, size = 100, replace =  TRUE)
mean(echantillon)
sd(echantillon)

taille = qnorm(p = 0.975, mean = 0, sd = 1)*9/sqrt(100)
borne_inf = 171-taille
borne_sup = 171+taille
borne_inf
borne_sup

echantillon1000 = replicate(n = 1000, expr = sample(population, 100, replace = TRUE))
moyenne1000 = apply(echantillon1000, MARGIN = 2, FUN = function(x) mean(x))
ecarttype1000 = apply(echantillon1000, MARGIN = 2, FUN = function(x) sd(x))

hist(moyenne1000)
#On retrouve la cloche 

mean(moyenne1000)
sd(moyenne1000)

pop1728 = echantillon1000[echantillon1000 > 172.8]
length(pop1728)
length(pop1728)/length(moyenne1000)

1 - pnorm(q = 172.8, mean = 171, sd = 9/sqrt(100))

largeur<-apply(X = echantillon1000,
               MARGIN = 2,
               FUN = function(x) pnorm(0.975)*sd(x)/100)

borne_inf_IC<-moyennes-largeur
borne_sup_IC<-moyennes+largeur

resultat1 = data.frame(largeur,borne_inf_IC, borne_sup_IC)
View(resultat1)

#Exercice 5 

moyenne_echantillon = function(V,n) {
  moy = mean(sample(x = V, size = n, replace = TRUE))
  return(moy)
}

moy20 = replicate(n = 10000, expr = moyenne_echantillon(V = population, n = 20))
moy30 = replicate(n = 100000, expr = moyenne_echantillon(V = population, n = 30))
moy50 = replicate(n = 100000, expr = moyenne_echantillon(V = population, n = 50))
moy100 = replicate(n = 100000, expr = moyenne_echantillon(V = population, n = 100))
moy500 = replicate(n = 100000, expr = moyenne_echantillon(V = population, n = 500))
moy1000 = replicate(n = 100000, expr = moyenne_echantillon(V = population, n = 1000))

par(mfrow = c(3,3))
hist(moy20, xlim=c(161,181), main="moy20")
hist(moy30, xlim=c(161,181), main="moy30")
hist(moy50, xlim=c(161,181), main="moy50")
hist(moy100, xlim=c(161,181), main="moy100")
hist(moy500, xlim=c(161,181), main="moy500")
hist(moy1000, xlim=c(161,181), main="moy1000")

nb_replicat = 100000
population<-runif(n = 1e7, min = 0, max = 1)
moyennes_20<-replicate(nb_replicat, moyenne_echantillon(population,20))
moyennes_30<-replicate(nb_replicat, moyenne_echantillon(population,30))
moyennes_50<-replicate(nb_replicat, moyenne_echantillon(population,50))
moyennes_100<-replicate(nb_replicat, moyenne_echantillon(population,100))
moyennes_500<-replicate(nb_replicat, moyenne_echantillon(population,500))
par(mfrow=c(2,3))
hist(moyennes_20, xlim=c(0,1), main="20")
hist(moyennes_30, xlim=c(0,1), main="30")
hist(moyennes_50, xlim=c(0,1), main="50")
hist(moyennes_100, xlim=c(0,1), main="100")
hist(moyennes_500, xlim=c(0,1), main="500")

