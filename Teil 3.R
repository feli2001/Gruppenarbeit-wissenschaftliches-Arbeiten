Studienfach <- sample(c("Statistik","Data Science","Mathe","Informatik"), 100, replace = T, prob= c(0.35,0.35,0.1,0.2))

# Statistik und Data Science werden beide mit einer Wahrscheinlichkeit von 35% studiert. Mathe hat die geringste Wahrscheinlichkeit mit 10% und 
# Informatik liegt mit 20% dazwischen.



Mathe_LK <- NULL
Mathe_LK[Studienfach == "Statistik"] <- sample(c("Ja","Nein"),length(Studienfach[Studienfach == "Statistik"]), replace = T, prob= c(0.75,0.25))
Mathe_LK[Studienfach == "Data Science"] <- sample(c("Ja","Nein"),length(Studienfach[Studienfach == "Data Science"]), replace = T, prob= c(0.65,0.35))
Mathe_LK[Studienfach == "Mathe"] <- sample(c("Ja","Nein"),length(Studienfach[Studienfach == "Mathe"]), replace = T, prob= c(0.9,0.1))
Mathe_LK[Studienfach == "Informatik"] <- sample(c("Ja","Nein"),length(Studienfach[Studienfach == "Informatik"]), replace = T, prob= c(0.5,0.5))

# Statistik-Studenten hatten mit einer Wahrscheinlichkeit von 75% Mathe-LK, Data-Scientist-Studenten mit 65% Wahrscheinlichkeit.
# Mathematik-Studenten hatten mit einer Wahrscheinlichekti von 90% Mathe-LK und Informatik-Studenten nur mit einer Wahrscheinlichkeit von 50%.