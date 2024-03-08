#TD 3

#Exercice 3 

install.packages("readxl")
library(readxl)
pokemon = read_excel(path = "pokemon.xlsx" ,sheet = "pokemon")

dim(pokemon)
nrow(pokemon)
ncol(pokemon)

summary(pokemon)

pokemon$is_legendary = as.factor(pokemon$is_legendary)
pokemon$generation = as.factor(pokemon$generation)
pokemon$type = as.factor(pokemon$type)

summary(pokemon)

#Exercice 2

med = median(pokemon$attack)
pokemon$attack_group = ifelse(test = pokemon$attack < med, yes = "attack+", no = "attack-")
pokemon$attack_group = as.factor(pokemon$attack_group)
summary(pokemon$attack_group)

pokemon$water_fire = ifelse(test = pokemon$type %in% c("water","fire"), yes = "yes", no ="no")
pokemon$water_fire = as.factor(pokemon$water_fire)
summary(pokemon$water_fire)

attackq3 = quantile(pokemon$attack, probs = 0.75)
defq3 = quantile(pokemon$defense, probs = 0.75)
speedq3 = quantile(pokemon$speed, probs = 0.75)
pokemon$best = ifelse(test = pokemon$attack > attackq3 & pokemon$defense > defq3 & pokemon$speed > speedq3, yes = "Yes", no = "No")
pokemon$best = as.factor(pokemon$best)
summary(pokemon$best)

requete = subset(pokemon, is.na(weight_kg))
View(requete)

requete1 = subset(pokemon, !is.na(weight_kg))
View(requete1)

med_weightkg = median(pokemon$weight_kg, na.rm = TRUE)
med_height = median(pokemon$height_m, na.rm = TRUE)

pokemon$weight_kgNA = ifelse(is.na(pokemon$weight_kg), yes = med_weightkg, no = pokemon$weight_kg)
pokemon$height_mNA = ifelse(is.na(pokemon$height_m), yes = med_height, no = pokemon$height_m)

pokemon$weight_m_group = cut(pokemon$weight_kgNA, breaks = 3, c("Léger", "Moyen", "Lourd"))
pokemon$height_m_group = cut(pokemon$height_mNA,  breaks = c(0,1,2,3,max(pokemon$height_mNA, na.rm = TRUE)))

pokemon$defense_group = cut(pokemon$defense, breaks = quantile(pokemon$defense, na.rm = TRUE), include.lowest = TRUE)
summary(pokemon$defense_group)

#EXercice 3

aggregate(x = attack ~ type, data = pokemon, FUN = function(x) mean(x))

aggregate(x = attack ~ generation + type, data = pokemon, FUN = function(x) median((x)))

aggregate(x = pokedex_number ~ type, data =  pokemon, FUN = function(x) length(x))

aggregate(x = speed ~ generation + type, data = pokemon, FUN = function(x) c(moyenne = mean(x), med = median(x), effectif = length(x)))






















