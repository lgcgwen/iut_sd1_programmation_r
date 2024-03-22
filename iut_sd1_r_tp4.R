#TP 4 

#Exercice 1 
salaire_net_cadre = function(salaire_brut = 2500,temps_travail = 1) {
  
  if (!is.numeric(salaire_brut)) {
    return("Erreur :  le salaire brut doit être une valeur numérique")
  }
  
  if (is.numeric(temps_travail) & (temps_travail >= 0) & (temps_travail <= 1)) {
    return("Erreur :  le temps de travail doit être une valeur numérique entre 0 et 1")
  }
  
  salaire_net_avant_impot = salaire_brut * temps_travail
  return(salaire_net_avant_impot) 
}




salaire_net = function(salaire_brut = 2500,temps_travail = 1, statut = "cadre"){
  
  if (!is.numeric(salaire_brut)) {
    return("Erreur :  le salaire brut doit être une valeur numérique")
  }
  
  if (!is.numeric(temps_travail) & (temps_travail >= 0) & (temps_travail <= 1)) {
    return("Erreur :  le temps de travail doit être une valeur numérique entre 0 et 1")
  }
  
  if (!statut == "cadre" & !statut == "non-cadre"){
    return("Erreur : le statut doit etre cadre ou non-cadre")
    
  }
  if(statut =="cadre"){
    salaire_net_avant_impots = salaire_brut * temps_travail * 0.75
    }else {
      salaire_net_avant_impots = salaire_brut * temps_travail * 0.78
    }

  if (salaire_net_avant_impots <= 1591) {
    salaire_net_apres_impots = salaire_net_avant_impots
  } else if(salaire_net_avant_impots <= 2006){
    salaire_net_apres_impots = salaire_net_avant_impots * 0.971
  } else if (salaire_net_avant_impots <= 3476){
    salaire_net_apres_impots = salaire_net_avant_impots * 0.901
  } else if(salaire_net_avant_impots <= 8557){
    salaire_net_apres_impots = salaire_net_avant_impots * 0.8
  } else if (salaire_net_avant_impots > 8557){
    salaire_net_apres_impots = salaire_net_avant_impots * 0.57
  }

    
  
  return(salaire_net_apres_impots) 
}

salaire_net(salaire_brut = 100000, temps_travail = 0.8)

#Exercice 2 

fin = 0 
for(i in c(1,2,3,4,5)){
  fin = fin + i
  print(fin)
}

i = 0
resulat = 0 
while(i < 50){
  resulat =resulat + i 
  i = i + 1
  print(resulat)
}

View(iris)

for(i in colnames(iris)){
  type = class(iris[,i])
  print(type)
}

i = 1 

while(i <= ncol(iris)){
  type = class(iris[, i])
  i = i + 1 
  print(type)
}

#Exercice 3 

for(i in 1:5){ #boucle 5 fois
  nb_u = readline(prompt = "donnez moi un nombre")
  nb_u = as.numeric(nb_u)
  
  carre = nb_u ^ 2
  print(carre)
  }

dossier = "chemin"

fichiers = list.files(path = dossier, full.names = TRUE)

for(i in fichiers) {
  information = file.info(fichier)
  taille = information$size
  print(taille)
}


colonne = colnames(iris)
par(mfrow = c(2, 3))

for(i in colnames(iris)){ #Que le colnames qui marche
  if(is.numeric(iris[, i])){
    boxplot(iris[, i], main = paste("boite à moustache pour", i))
  } else {
    barplot(table(iris[,i]), main = paste("diagramme pour", i))
  }
  
}

# Boucle while pour jouer au shifumi jusqu'à ce que l'utilisateur décide d'arrêter
continuer <- TRUE
while (continuer) {
  # Appeler la fonction shifumi et afficher le résultat
  resultat <- shifumi()
  cat("Résultat du jeu :", resultat, "\n")
  
  # Demander à l'utilisateur s'il souhaite continuer
  reponse <- readline(prompt = "Voulez-vous continuer à jouer ? (oui/non) : ")
  
  # Vérifier la réponse de l'utilisateur
  if (tolower(reponse) == "non") {
    print("Arrêt du jeu.")
    continuer <- FALSE  # Mettre fin à la boucle
  }
}

# Fonction pour le jeu du juste prix
juste_prix <- function() {
  # Génération d'un nombre aléatoire entre 1 et 100
  nombre_a_deviner <- sample(1:100, 1)
  
  # Initialisation de la réponse de l'utilisateur
  reponse <- -1
  
  # Boucle tant que l'utilisateur n'a pas trouvé le bon nombre
  while (reponse != nombre_a_deviner) {
    # Demande à l'utilisateur de saisir un nombre
    reponse <- as.integer(readline(prompt = "Devinez le nombre : "))
    
    # Vérifie si le nombre est trop grand, trop petit ou correct
    if (reponse < nombre_a_deviner) {
      cat("C'est plus !\n")
    } else if (reponse > nombre_a_deviner) {
      cat("C'est moins !\n")
    } else {
      cat("Bravo, vous avez trouvé le juste prix !\n")
    }
  }
}

# Appel de la fonction juste_prix pour commencer le jeu
juste_prix()


