# TP 5 

#Exercice 1 

fichiers = list.files(path = getwd(), pattern = "\\.csv$", full.names = TRUE)

library(tools)

#fichier sans le chemin
nom_fichier = basename(path = "L:/BUT/SD/Promo 2023/glegalloudec/Programmation Statistiques/TP/TP 5/nba/common_player_info.csv" )

#fichier sans le chemin + sans extension 
fichier_sans_extension = file_path_sans_ext(nom_fichier)

#créer un data frame
assign(x = fichier_sans_extension, read.csv("common_player_info.csv", sep = ",", dec = "."))

for (fichier in fichiers) {
  # Extraire le nom du fichier sans extension
  nom_objet = file_path_sans_ext(basename(fichier))
  
  # Lire le fichier CSV et l'affecter à une variable avec le nom du fichier
  start_time <- Sys.time()
  assign(nom_objet, read.csv(fichier, 
                             sep = ",",
                             dec = "."))
  end_time <- Sys.time()
  # Calcul du temps écoulé
  execution_time <- end_time - start_time
  cat("Importation : ",nom_objet, "=" , execution_time , "\n")
}

#Exerice 2 

dx = subset(team, city == "Los Angeles", select = c("id", "city"))
dy = subset(game, select = c("game_id", "team_id_home"))
dfjoin = merge(x = dx, y = dy, by.x = "id", by.y = "team_id_home")
nrow(dfjoin)

dx1 = dfjoin
dy1 = subset(game_info, select = c("game_id", "attendance"))
dfjoin = merge(x = dx1, y = dy1, by = "game_id", all.x = TRUE)
mean(dfjoin$attendance, na.rm = TRUE)

dx2 = subset(game_summary, season == 2020, select = c("game_id", "season"))
dy2 = officials
dfjoin = merge(x = dx2, y = dy2, by = "game_id", all.x = TRUE)
length(unique(dfjoin$official_id))

dx3 = subset(game_summary, select =  c("game_id", "season"))
dy3 = subset(officials, first_name == "Dick" & last_name == "Bavetta")
dfjoin = merge(x = dx3, y = dy3, by = "game_id", all.y = TRUE)
table(dfjoin$season)
View(dfjoin)

#Exercice 3 

install.packages("DBI")
install.packages("RSQLite")

mydb = dbConnect(SQLite(), "nbaDb.sqlite")

dbListTables(mydb)

dbGetQuery(mydb, 'SELECT * FROM team LIMIT 5')

dbWriteTable(mydb, "nom_table", dfJoin)
dbListTables(mydb)

dbDisconnect(mydb)









