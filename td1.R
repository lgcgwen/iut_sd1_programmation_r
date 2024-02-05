#TD1

#Exercice 1 
a = 10
b = 5
resultat = a * b 
print(resultat)

A = 7.2
B = 10.1

#Création deux variables différentes A != a

resultat = A + B

#change la valeur de mon ancienne variable résultat

rm(a, b, A, B, resultat)


#Exercice 2

vec1 = c(1, 2, 3, 4, 5)
class(vec1)
vec1[3]

v1 = 1:5

v2 = v1 + 3 

v3 = c(1, 2, 3, 4, 5, 6)

v4 = v3^2

v5 = v4/2

v6 = c("lundi", 
       "mardi", 
       "mercredi", 
       "jeudi", 
       "vendredi", 
       "samedi", 
       "dimanche")
class(v6)
v6[c(2, 7)]


v7 = c(TRUE, TRUE, FALSE, FALSE, TRUE)
class(v7)

v8 = c(10.1, 5.3, 8.5, 4.2, 20.9)
class(v8)
v8[-3]

v9 = c("Janvier","Fevrier","Mars","Avril",
       "Mai","Juin","Juillet","Aout","Septembre",
       "Octobre", "Novembre","Decembre")
class(v9)
v9[c(1, 2, 3)]

v10 = c(-1, -2, -3, -4, -5)
class(v10)
v10[c(5, 1)]

v11 = c("Pomme", "Poire", "Ananas", "Abricot", "Peche")
class(v11)
v11[-c(1, 2)]

v12 = c(1, 2, 3, NA, 5)
class(v12)

s1 = seq(1, 10, by = 1)
length(s1)

s2 = seq(2, 20, by = 2)
length(s2)

s3 = seq (0, -5 )
length(s3)

s4 = seq(5, 50, 5)
length(s4)

s5 = seq(10, 1)
length(s5)

s6 = seq(0, 1, 0.1)
length(s6)

s7 = seq( 5, -5, -1)
length(s7)

s8 = seq(1, 10, 2)
length(s8)

v13 = rep(3, 5)

v14 = rep(c("A", "B", "C"), 3)

v15 = rep(1:3, 3)

v16 = rep(c(TRUE, FALSE), 4)

rm(v16)

v17 = runif(5, 0, 1)
mean(v17)
median(v17)
min(v17)
max(v17)

v18 = runif(10, -5, 5)
mean(v18)
median(v18)
min(v18)
max(v18)

v19 = runif(100, 10, 20)
mean(v19)
median(v19)
min(v19)
max(v19)

v20 = runif(15, 50, 100)
mean(v20)
median(v20)
min(v20)
max(v20)

v21 = rnorm(20, -2, 3)
mean(v21)
sd(v21)

v22 = rnorm(2000, -2, 3)
mean(v22)
sd(v22)

v23 = rnorm(2000, 0, 1) 
mean(v23)
sd(v23)
quantile(v23, c(0.25, 0.50, 0.75))
quantile(v23, c(0, 1, 0.1))
quantile(v23, c(0, 1, 0.01))
hist(v23)

v24 =rnorm(3000, 2400, 300)
round(v24, 2)
sum(v24)
median(v24)
quantile(v24, 0.99)
#1% des salariés ont minimum ce salaire
quantile(v24, 0.2)
#80% des slariés ont ce salaire

v25 = sample(c(1, 2, 3, 4, 5, 6), 1)
simulation = sample(c(1, 2, 3, 4, 5, 6), 12, TRUE)
print(simulation)
unique(simulation)
table(simulation)
prop.table(table(simulation))
#Non ce n'est pas le cas
simulation = sample(c(1, 2, 3, 4, 5, 6), 100000, TRUE)
frequence =prop.table(table(simulation))
sort(frequence, TRUE)